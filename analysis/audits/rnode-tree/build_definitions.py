"""Build rnode-definitions.edn: for each R-node, the AIF quantity it acts on, quoted from the
paper's glossary (p4ng/sec-glossary.tex) and/or its catalogue paragraph (p4ng/sec-catalog.tex);
an operator reading phrased in terms of that quantity; the operations a phrase may perform on it;
near misses; and seed cues from rnode-vocabulary.json. The admission test every new cue must pass
is stated once at the top. Quotes are extracted, not retyped; the hand-written parts are READINGS."""
import json, re
from pathlib import Path
import edn_format
from edn_format import Keyword as K

HERE = Path(__file__).resolve().parent
P4 = Path('/home/joe/code/p4ng')
GLOSSARY, CATALOG = P4 / 'sec-glossary.tex', P4 / 'sec-catalog.tex'
STAGES = P4 / 'empirics-futon/control-stages.edn'

# node -> (glossary paragraph titles, catalogue node id or None, quantity, operations, operator reading, near misses)
READINGS = {
 'R1': (['Belief state'], None, 'belief state mu', ['state'],
        'you state how things currently are taken to be: a held estimate, not a fresh report',
        ['a report of what you just saw (R2)', 'a change of mind (R3)']),
 'R2': (['Observation vector'], 'R2', 'observation o', ['report'],
        'you report what came in: something seen, returned, logged, measured',
        ['an opinion about it (R1)', 'a surprise at it (R8)']),
 'R3': (['Prediction error'], 'R3', 'prediction error epsilon driving an update of mu', ['update'],
        'you revise what you hold because what came in differed from what you expected',
        ['merely stating the belief (R1)', 'noticing the mismatch without revising (R8)']),
 'R3a': (['Prediction error'], None, 'prediction error epsilon projected onto the belief', ['attribute'],
         'you locate the error in your own assumption or reading rather than in the world',
         ['blaming the world or the system (R8)', 'a general change of mind (R3)']),
 'R7': (['Precision'], 'R7', 'precision Pi over evidence channels', ['weight'],
        'you weight how far a source or signal is to be trusted relative to others',
        ['simply trusting one result (R2)', 'calibrating a score against outcomes (R12)']),
 'R8': (['Variational free energy'], 'R8', 'present-tense mismatch F between belief and observation', ['register'],
        'you register that what is now the case does not fit what was expected or claimed',
        ['projecting a future consequence (R4)', 'locating the fault in your assumption (R3a)']),
 'R4': (['Predictive outcome distribution', 'Observation model'], 'R4', 'predicted outcomes Q(o|pi)', ['predict'],
        'you say what would follow if a course of action were taken',
        ['a plan of several steps (R13)', 'weighing whether it is worth it (R5)']),
 'R5': (['Expected free energy', 'Risk', 'Ambiguity'], None, 'expected free energy G (risk, ambiguity, information gain)', ['compare', 'weigh'],
        'you weigh an option by its expected cost against what it would let you learn',
        ['saying what would happen (R4)', 'choosing among options without weighing them (R6)']),
 'R6': (['Control states', 'Policy $\\pi$'], None, 'the set of candidate policies pi over control states U', ['open', 'enumerate'],
        'you lay out or widen the options that could be taken',
        ['committing to one of them (R14)', 'a multi-step plan (R13)']),
 'R13': (['Control states'], None, 'policy depth: the horizon of pi', ['extend'],
         'you look several steps ahead and order actions over that horizon',
         ['a single prediction (R4)', 'scheduling for later without ordering (R10)']),
 'R14': (['Softmax and controller calibration'], None, 'temperature tau / precision over policies', ['set'],
         'you set how sharply one option wins over the others: firm commitment, provisional lean, deliberate hedging',
         ['naming the options (R6)', 'stating a value or goal (R19)']),
 'R15': ([], 'R15', 'the hierarchy of fast and slow loops and their timescales', ['separate', 'connect'],
         'you separate or connect a fast loop and a slow one: what this run feeds into the next, tactics versus strategy',
         ['ordering steps within one loop (R13)', 'a schedule (R10)']),
 'R11': ([], 'R11', 'a shared budget arbitrated across levels', ['allocate'],
         'you allocate or ration a resource shared across the work: usage, tokens, time',
         ['an alarm about running out (R20)', 'weighing one option (R5)']),
 'R16': ([], None, 'grounded enactment u: the action that changes the world', ['enact', 'direct'],
         'you direct that a change actually be made in the world, and grounded rather than re-observed',
         ['proposing options (R6)', 'checking afterwards that it moved (R8)']),
 'R17': (['Bayesian Model Reduction', 'Dirichlet concentration parameters', 'Model uncertainty and EIG'], 'R17',
         'structure learning: counts and model shape revised from what worked', ['learn', 'prune', 'merge'],
         'you change which components get used, or the shape of the model, because of how they performed',
         ['a one-off change of mind (R3)', 'a new rule (GRAIN-GATE)']),
 'R19': (['Preference distribution'], None, 'preference C over outcomes, read from the mission', ['declare'],
         'you declare which outcomes are wanted or count as done',
         ['an order of priorities over time (CTAU)', 'how firmly to hold a choice (R14)']),
 'CTAU-TOKEN': (['Preference distribution'], None, 'per-problem preference schedule C_tau', ['schedule'],
                'you set what is preferred for this particular problem, stage by stage',
                ['what is wanted overall (R19)', 'an overall split across kinds of work (CTAU-CLASS)']),
 'CTAU-CLASS': (['Preference distribution'], None, 'class-level preference schedule C_tau for the joint decision', ['schedule'],
                'you set the split of preference across kinds of work for the whole decision',
                ['one problem\'s preference (CTAU-TOKEN)', 'a budget allocation (R11)']),
 'GRAIN-GATE': (['Act-gate'], None, 'the admissible support: which enactments may run at all', ['admit', 'forbid'],
                'you rule an action in or out before any weighing: a rule, a condition, a prohibition',
                ['a preference among allowed options (R14)', 'an alarm (R20)']),
 'R9': (['No self-certification'], 'R9', 'evidence independence: no verdict from self-produced evidence', ['require'],
        'you require that a claim be checked by someone or something other than its author',
        ['checking a result yourself (R2)', 'calibrating a measure (R12)']),
 'R10': ([], 'R10', 'scheduled entry: the loop runs without the operator triggering it', ['schedule'],
         'you arrange for work to run on its own, on a schedule or while you are away',
         ['asking whether something is still running (R2)', 'ordering steps (R13)']),
 'R12': ([], 'R12', 'two-layer calibration: internal consistency vs external value evidence', ['calibrate'],
         'you question whether a score or count means anything against real outcomes',
         ['weighting a source (R7)', 'an independent check (R9)']),
 'R20': ([], 'R20', 'interoceptive tripwires: process-health alarms that bypass the loop', ['alarm', 'halt'],
         'you raise or heed an alarm about the health of the process itself: stop, something is wrong',
         ['ordinary disappointment at a result (R8)', 'a rule set in advance (GRAIN-GATE)']),
}

ADMISSION = ('A phrase may be proposed as a cue for node N only if the proposer can say, in one line, '
             'which quantity of N the operator\'s words act on and which of N\'s operations they perform on it '
             '(e.g. "\'let\'s not over-commit\' -> R14: lowers precision over two policies"). '
             'Resemblance to the seed cues is not enough. A phrase that is already an intent cue is not proposed. '
             'Nodes with no glossary formula (R9, R10, R11, R12, R15, R16, R20, GRAIN-GATE) rest on their catalogue '
             'paragraph or reading: hold proposals for them to the reading and its near misses.')


def strip_tex(s):
    s = re.sub(r'\\(?:textbf|emph|texttt|textsc)\{([^}]*)\}', r'\1', s)
    s = re.sub(r'\\eqanchor\{[^}]*\}|\\footnote\{.*?\}|\\label\{[^}]*\}|\\apparatusmark\{\}|\\hspace\{[^}]*\}', '', s)
    s = re.sub(r'\$([^$]*)\$', r'\1', s)
    s = re.sub(r'\\([a-zA-Z]+)', r'\1', s)
    return ' '.join(s.replace('{', '').replace('}', '').replace('``', '"').replace("''", '"').split())


def paragraphs(path):
    out = {}
    for m in re.finditer(r'^\\paragraph\{(.+?)\}(.*?)(?=^\\paragraph|^\\section|\Z)', path.read_text(), re.M | re.S):
        out[m.group(1)] = m.group(2)
    return out


def main():
    gl, cat = paragraphs(GLOSSARY), paragraphs(CATALOG)
    vocab = {n['id']: n['cues'] for n in json.loads((HERE / 'rnode-vocabulary.json').read_text())['nodes']}
    labels = {n[K('node')]: n[K('label')] for n in edn_format.loads(STAGES.read_text())[K('nodes')]}
    nodes = []
    for node, (gkeys, ckey, qty, ops, reading, misses) in READINGS.items():
        gq = []
        for k in gkeys:
            title = next(t for t in gl if t.startswith(k))
            gq.append({'paragraph': strip_tex(title).rstrip('.'), 'text': strip_tex(gl[title])[:400]})
        cq = None
        if ckey:
            title = next(t for t in cat if f'({ckey}' in t)
            cq = {'paragraph': strip_tex(title).rstrip('.'), 'text': strip_tex(cat[title])[:400]}
        assert gq or cq or node in ('R16', 'R13'), node
        nodes.append({K('node'): node, K('label'): labels[node], K('quantity'): qty, K('operations'): ops,
                      K('operator-reading'): reading, K('near-misses'): misses,
                      K('glossary'): [{K('paragraph'): g['paragraph'], K('text'): g['text']} for g in gq],
                      K('catalogue'): ({K('paragraph'): cq['paragraph'], K('text'): cq['text']} if cq else None),
                      K('seeds'): vocab.get(node, [])[:12]})
    assert {n[K('node')] for n in nodes} == set(labels) - {'TRACE'}
    doc = {K('schema'): K('rnode-definitions/v1'), K('admission'): ADMISSION,
           K('sources'): [str(GLOSSARY), str(CATALOG), str(HERE / 'rnode-vocabulary.json')], K('nodes'): nodes}
    (HERE / 'rnode-definitions.edn').write_text(
        ';; GENERATED by build_definitions.py: glossary/catalogue text is quoted; quantity, operations,\n'
        ';; operator-reading and near-misses are claude-17 readings (2026-10-02). Seeds are from\n'
        ';; rnode-vocabulary.json and are not shown to the operator.\n' + edn_format.dumps(doc) + '\n')
    print(len(nodes), 'nodes;', sum(1 for n in nodes if n[K('glossary')]), 'with glossary text;',
          sum(1 for n in nodes if n[K('catalogue')]), 'with catalogue text;',
          len((HERE / 'rnode-definitions.edn').read_text()), 'chars')


if __name__ == '__main__':
    main()
