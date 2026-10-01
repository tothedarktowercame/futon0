// Playwright validation of the merged Minard page (analysis/audits/minard-operator-work-2026-10-01.html).
// Run: cd /tmp/pw && node /home/joe/code/futon0/analysis/audits/check_minard_page.mjs
import { createRequire } from 'node:module';
import { mkdirSync } from 'node:fs';
// playwright is installed in /tmp/pw; resolve it from there regardless of cwd.
const require = createRequire(process.env.PW_PROJECT || '/tmp/pw/package.json');
const { chromium } = require('playwright');

const page = process.argv[2] || '/home/joe/code/futon0/analysis/audits/minard-operator-work-2026-10-01.html';
const shots = '/tmp/minard-shots';
mkdirSync(shots, { recursive: true });

const browser = await chromium.launch({
  executablePath: '/home/joe/.cache/ms-playwright/chromium_headless_shell-1234/chrome-headless-shell-linux64/chrome-headless-shell',
});
const ctx = await browser.newContext({ viewport: { width: 1600, height: 1000 } });
const pg = await ctx.newPage();

const problems = [];
pg.on('pageerror', e => problems.push('pageerror: ' + e.message));
pg.on('console', m => { if (m.type() === 'error') problems.push('console.error: ' + m.text()); });
pg.on('requestfailed', r => problems.push('requestfailed: ' + r.url()));
pg.on('request', r => { if (!r.url().startsWith('file://')) problems.push('external request: ' + r.url()); });

function check(name, cond, detail = '') {
  console.log((cond ? 'PASS' : 'FAIL') + ' ' + name + (detail ? ' — ' + detail : ''));
  if (!cond) problems.push(name + (detail ? ': ' + detail : ''));
}

await pg.goto('file://' + page, { waitUntil: 'load' });
await pg.waitForFunction(() => window.MINARD_DATA && window.MINARD_FAMILY_DATA);

// Main stream draws all 8 stage bands.
const stages = await pg.evaluate(() => window.MINARD_DATA.stages);
const bandCount = await pg.locator('#chart path[data-stream]').count();
check('main stream draws all 8 stage bands', bandCount === 8 && stages.length === 8, `bands=${bandCount}`);

const panel = pg.locator('#family-panel');
check('family panel starts hidden', await panel.isHidden());

async function assertOpen(stage, via) {
  check(`family panel visible after ${via} on ${stage}`, await panel.isVisible());
  const expected = await pg.evaluate(s =>
    window.MINARD_FAMILY_DATA.panels.find(p => p.stage === s).families.length, stage);
  const drawn = await pg.locator('#fchart path[data-family-band]').count();
  check(`${stage} family band count equals data family count (${via})`, drawn === expected, `drawn=${drawn} expected=${expected}`);
  const title = await pg.locator('#fam-title').textContent();
  check(`${stage} panel title names the stage (${via})`, title.includes(stage), title);
}
async function assertClosed(label) {
  check(`all-stages view restored (${label})`, await panel.isHidden());
  const dimmed = await pg.locator('#chart .dimmed').count();
  check(`no dimmed bands after going back (${label})`, dimmed === 0);
}

// Activate every stage by click, then by keyboard. Clicks land on the band's
// transparent daily-slice target (the topmost element over the visible band),
// exactly as a real user's click on the band does.
async function clickBand(stage) {
  // Clicks elsewhere (e.g. #back-all) scroll the page; reset so band points
  // computed from getBoundingClientRect land inside the viewport.
  await pg.evaluate(() => scrollTo({ top: 0, behavior: 'instant' }));
  // Compute, in the page, the viewBox point at the centre of the stage's band
  // on its thickest day, then convert to client coordinates for a real click —
  // the same point a user would pick on the visible band.
  const pt = await pg.evaluate(s => {
    const D = window.MINARD_DATA;
    const start = Date.parse('2026-08-22T00:00:00Z'), finish = Date.parse('2026-10-02T00:00:00Z');
    const order = D.days.map((d, i) => i).sort((a, b) => D.days[b].mean[s] - D.days[a].mean[s]);
    const r = document.getElementById('chart').getBoundingClientRect();
    for (const i of order) {
      const d = D.days[i];
      const sum = D.stages.reduce((n, st) => n + d.mean[st], 0);
      let y = 340 - sum * 2.15 / 2;
      for (const st of D.stages) {
        if (st === s) { y += d.mean[st] * 2.15 / 2; break; }
        y += d.mean[st] * 2.15;
      }
      const vx = 74 + 1060 * (Date.parse(d.date + 'T12:00:00Z') - start) / (finish - start);
      const p = { x: r.left + r.width * vx / 1450, y: r.top + r.height * y / 900 };
      const hit = document.elementFromPoint(p.x, p.y);
      if (hit && (hit.getAttribute('data-stage') === s || hit.getAttribute('data-stream') === s)) return p;
    }
    throw new Error('no clickable point found on band ' + s + ' ' + JSON.stringify({
      scrollY, innerHeight,
      chartTop: r.top, chartHeight: r.height,
      sample: (() => { const d = D.days[order[0]];
        const sum = D.stages.reduce((n, st) => n + d.mean[st], 0);
        let yy = 340 - sum * 2.15 / 2;
        for (const st of D.stages) { if (st === s) { yy += d.mean[st] * 2.15 / 2; break; } yy += d.mean[st] * 2.15; }
        const vx = 74 + 1060 * (Date.parse(d.date + 'T12:00:00Z') - start) / (finish - start);
        const p = { x: r.left + r.width * vx / 1450, y: r.top + r.height * yy / 900 };
        const h = document.elementFromPoint(p.x, p.y);
        return { p, hit: h ? h.tagName + ':' + (h.getAttribute('data-stage') || h.getAttribute('data-stream') || h.id || '') : null }; })(),
    }));
  }, stage);
  await pg.mouse.click(pt.x, pt.y);
}
for (const stage of stages) {
  await clickBand(stage);
  await assertOpen(stage, 'click');
  await pg.locator('#back-all').click();
  await assertClosed('back button after click');

  await pg.locator(`#chart path[data-stream="${stage}"]`).focus();
  await pg.keyboard.press('Enter');
  await assertOpen(stage, 'keyboard Enter');
  await pg.keyboard.press('Escape');
  await assertClosed('Escape after keyboard');
}

// Hover on a family band shows exact hits.
await clickBand('assurance');
const famName = await pg.locator('#fchart rect.hit[data-family]').first().getAttribute('data-family');
await pg.locator('#fchart rect.hit[data-family]').first().dispatchEvent('pointermove');
const tip = await pg.locator('#tooltip').textContent();
const readout = await pg.locator('#readout').textContent();
check('hover on a family shows exact hits',
  tip.includes('exact hits') && tip.includes(famName) && readout.includes('exact hits'),
  tip);
await pg.screenshot({ path: shots + '/open-assurance.png', fullPage: true });

await pg.locator('#back-all').click();
await clickBand('believe');
await pg.screenshot({ path: shots + '/open-believe.png', fullPage: true });
await pg.locator('#back-all').click();

// All-stages view screenshot, light then dark.
await pg.selectOption('#theme', 'light');
await pg.screenshot({ path: shots + '/all-stages-light.png', fullPage: true });
await pg.selectOption('#theme', 'dark');
check('dark theme applies', await pg.evaluate(() => document.documentElement.dataset.theme === 'dark'));
await pg.locator('#chart path[data-stream="assurance"]').focus();
await pg.keyboard.press('Enter');
check('family panel visible in dark theme', await panel.isVisible());
const darkTipOk = await (async () => {
  await pg.locator('#fchart rect.hit[data-family]').first().dispatchEvent('pointermove');
  return (await pg.locator('#tooltip').textContent()).includes('exact hits');
})();
check('hover on a family shows exact hits in dark theme', darkTipOk);
await pg.screenshot({ path: shots + '/open-assurance-dark.png', fullPage: true });
await pg.locator('#back-all').click();
await pg.screenshot({ path: shots + '/all-stages-dark.png', fullPage: true });

check('no page errors, console errors, failed or external requests', problems.length === 0, problems.join(' | '));

await browser.close();
if (problems.length) { console.error('\nFAILURES:\n' + problems.join('\n')); process.exit(1); }
console.log('\nALL CHECKS PASSED');
