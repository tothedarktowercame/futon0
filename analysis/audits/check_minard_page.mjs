// Playwright validation of the merged Minard page (analysis/audits/minard-operator-work-2026-10-01.html)
// with in-place family decomposition: a clicked band splits into family stripes
// inside its own outline in #chart. No separate family figure exists.
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

const stages = await pg.evaluate(() => window.MINARD_DATA.stages);
const bandCount = await pg.locator('#chart path[data-stream]').count();
check('main stream draws all 8 stage bands', bandCount === 8 && stages.length === 8, `bands=${bandCount}`);
check('no #fchart separate family figure exists', await pg.locator('#fchart').count() === 0);
check('no stripes before any activation', await pg.locator('#chart path[data-family-band]').count() === 0);

const SAMPLE_DAYS = [5, 22, 38]; // three sample days across the window
async function assertOpen(stage, via) {
  const drawn = await pg.locator('#chart path[data-family-band]').count();
  const expected = await pg.evaluate(s =>
    window.MINARD_FAMILY_DATA.panels.find(p => p.stage === s).families.length, stage);
  check(`${stage} stripe count inside #chart equals data family count (${via})`, drawn === expected, `drawn=${drawn} expected=${expected}`);
  const geo = await pg.evaluate(({ s, samples }) => {
    const g = window.__stripes;
    if (!g || g.stage !== s) return { ok: false, why: 'no __stripes for ' + s };
    return {
      ok: true,
      rows: samples.map(i => {
        const d = g.days[i];
        const band = d.bandBot - d.bandTop;
        const stripes = d.stripes.reduce((n, st) => n + (st.bot - st.top), 0);
        return { date: d.date, band, stripes, diff: Math.abs(band - stripes) };
      }),
    };
  }, { s: stage, samples: SAMPLE_DAYS });
  check(`${stage} stripes exposed for the opened band (${via})`, geo.ok, geo.why || '');
  for (const r of (geo.rows || []))
    check(`${stage} summed stripe thickness equals band thickness on ${r.date} (${via})`,
      r.diff < 1, `band=${r.band.toFixed(3)} stripes=${r.stripes.toFixed(3)} diff=${r.diff.toFixed(4)}px`);
  const legend = await pg.locator('#famlegend').count();
  const header = legend ? await pg.locator('#famlegend text.heading').first().textContent() : '';
  check(`${stage} family legend replaces count-integral column (${via})`,
    legend === 1 && header === stage.toUpperCase() + ' BY FAMILY', header);
  // stripes render inside the band's own vertical extent: first stripe top = band
  // top, last stripe bottom = band bottom, stripes tile without gaps
  const tiling = await pg.evaluate(({ s, samples }) => {
    const g = window.__stripes;
    return samples.map(i => {
      const d = g.days[i];
      const first = d.stripes[0], last = d.stripes[d.stripes.length - 1];
      let gap = Math.abs(first.top - d.bandTop) + Math.abs(last.bot - d.bandBot);
      for (let k = 1; k < d.stripes.length; k++) gap += Math.abs(d.stripes[k].top - d.stripes[k - 1].bot);
      return { date: d.date, gap };
    });
  }, { s: stage, samples: SAMPLE_DAYS });
  for (const r of tiling)
    check(`${stage} stripes tile the band exactly on ${r.date} (${via})`, r.gap < 1, `gap=${r.gap.toFixed(4)}px`);
}
async function assertClosed(label) {
  check(`plain bands restored, no stripes (${label})`,
    await pg.locator('#chart path[data-family-band]').count() === 0 &&
    await pg.locator('#chart path[data-stream]').count() === 8);
  check(`count-integral column restored (${label})`, await pg.locator('#famlegend').count() === 0);
  check(`no dimmed bands after close (${label})`, await pg.locator('#chart .dimmed').count() === 0);
  check(`__stripes cleared (${label})`, await pg.evaluate(() => window.__stripes === null));
}

async function bandPoint(stage) {
  await pg.evaluate(() => scrollTo({ top: 0, behavior: 'instant' }));
  return pg.evaluate(s => {
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
    throw new Error('no clickable point found on band ' + s);
  }, stage);
}
async function clickBand(stage) { const pt = await bandPoint(stage); await pg.mouse.click(pt.x, pt.y); }

// Activate every stage by click, then by keyboard.
for (const stage of stages) {
  await clickBand(stage);
  await assertOpen(stage, 'click');
  await pg.locator('#back-all').click();
  await assertClosed('All-stages button after click');

  await pg.locator(`#chart path[data-stream="${stage}"]`).focus();
  await pg.keyboard.press('Enter');
  await assertOpen(stage, 'keyboard Enter');
  await pg.keyboard.press('Escape');
  await assertClosed('Escape after keyboard');
}

// Only one stage open at a time: opening another switches.
await clickBand('assurance');
await clickBand('believe');
const openCount = await pg.locator('#chart g#famstripes').count();
const openStage = await pg.evaluate(() => window.__stripes && window.__stripes.stage);
check('clicking another stage switches (exactly one open)', openCount === 1 && openStage === 'believe', `open=${openStage}`);
// Clicking the open band itself closes it.
{
  const pt = await pg.evaluate(() => {
    const g = window.__stripes, d = g.days[22];
    const s = g.stage, D = window.MINARD_DATA;
    const start = Date.parse('2026-08-22T00:00:00Z'), finish = Date.parse('2026-10-02T00:00:00Z');
    const vx = 74 + 1060 * (Date.parse(d.date + 'T12:00:00Z') - start) / (finish - start);
    const r = document.getElementById('chart').getBoundingClientRect();
    return { x: r.left + r.width * vx / 1450, y: r.top + r.height * (d.bandTop + d.bandBot) / 2 / 900 };
  });
  await pg.mouse.click(pt.x, pt.y);
  await assertClosed('clicking the open band');
}

// Hover on a stripe shows exact hits.
await clickBand('assurance');
const famName = await pg.locator('#chart rect.stripe[data-family]').first().getAttribute('data-family');
await pg.locator('#chart rect.stripe[data-family]').first().dispatchEvent('pointermove');
const tip = await pg.locator('#tooltip').textContent();
const readout = await pg.locator('#readout').textContent();
check('hover on a stripe shows exact hits',
  tip.includes('exact hits') && tip.includes(famName) && tip.includes('assurance') && readout.includes('exact hits'),
  tip);
await pg.screenshot({ path: shots + '/inplace-assurance.png', fullPage: true });
await pg.locator('#back-all').click();

await clickBand('believe');
await pg.screenshot({ path: shots + '/inplace-believe.png', fullPage: true });
await pg.locator('#back-all').click();
await pg.screenshot({ path: shots + '/inplace-all-stages-light.png', fullPage: true });

// Dark theme.
await pg.selectOption('#theme', 'dark');
check('dark theme applies', await pg.evaluate(() => document.documentElement.dataset.theme === 'dark'));
await pg.locator('#chart path[data-stream="assurance"]').focus();
await pg.keyboard.press('Enter');
await assertOpen('assurance', 'dark theme keyboard');
await pg.locator('#chart rect.stripe[data-family]').first().dispatchEvent('pointermove');
check('hover on a stripe shows exact hits in dark theme',
  (await pg.locator('#tooltip').textContent()).includes('exact hits'));
await pg.screenshot({ path: shots + '/inplace-assurance-dark.png', fullPage: true });
await pg.keyboard.press('Escape');
await assertClosed('dark theme Escape');
await pg.selectOption('#theme', 'light');

check('no page errors, console errors, failed or external requests', problems.length === 0, problems.join(' | '));

await browser.close();
if (problems.length) { console.error('\nFAILURES:\n' + problems.join('\n')); process.exit(1); }
console.log('\nALL CHECKS PASSED');
