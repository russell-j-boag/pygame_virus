"""Independent source/result checks; run from the repository root with r-pygame Python."""
import collections
import csv
import hashlib
import math
from pathlib import Path
import statistics
import subprocess
import tempfile
import os
from html.parser import HTMLParser

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / 'analysis_outputs/semester2_2026_hypotheses'

def read(path):
    with Path(path).open() as f:
        return list(csv.DictReader(f))

def binary(value):
    normalized = str(value).lower()
    assert normalized in ('true', 'false', '1', '0'), value
    return int(normalized in ('true', '1'))

def close(a, b):
    assert math.isclose(float(a), float(b), rel_tol=1e-10, abs_tol=1e-10), (a, b)

trials = read(ROOT / 'data/data_virus_all.csv')
questions = read(ROOT / 'data/data_virus_postblock_all.csv')
sliders = read(ROOT / 'data/data_virus_sliders_all.csv')
averages = read(ROOT / 'data/semester2_2026_averaged_within_participants.csv')
manifest = read(ROOT / 'data/collation_manifest.csv')
assert (len(trials), len(questions), len(sliders), len(averages), len(manifest)) == (46800, 720, 180, 60, 180)
assert {int(r['participant_id']) for r in trials} == set(range(1, 61))
assert set(collections.Counter((r['participant_id'], r['aid_condition']) for r in trials).values()) == {260}
for rows in (trials, questions, sliders):
    assert {r['run_timestamp'] for r in rows if r['participant_id'] == '59'} == {'20260924_110831'}

# Verify each output cell against its selected raw export, not merely the manifest.
output_by_kind = {'trials': trials, 'questionnaire': questions, 'sliders': sliders}
compare_fields = {
    'trials': ('aid_condition', 'trial', 'decision1_response', 'decision2_response'),
    'questionnaire': ('aid_condition', 'question_idx', 'response'),
    'sliders': ('aid_condition', 'question_key', 'response_percent'),
}
for m in manifest:
    path = Path(m['source_file'])
    assert hashlib.md5(path.read_bytes()).hexdigest() == m['source_md5']
    kind = m['export_type']
    source = read(path)
    observed = [r for r in output_by_kind[kind] if r['participant_id'] == m['participant_id']]
    fields = compare_fields[kind]
    canonical = lambda rows: sorted(tuple(r[f] for f in fields) for r in rows)
    assert canonical(source) == canonical(observed), (path, kind)

by_cell = collections.defaultdict(list)
for r in trials:
    by_cell[(r['participant_id'], r['aid_condition'])].append(r)
    assert binary(r['changed_response']) == (r['decision1_response'] != r['decision2_response'])
    assert binary(r['changed_response']) == (binary(r['decision1_correct']) != binary(r['decision2_correct']))
    if r['aid_condition'] != 'manual':
        assert binary(r['aid_correct']) == (r['aid_label'] == r['stimulus'])
        assert binary(r['decision2_correct']) == (r['decision2_response'] == r['stimulus'])

# Reconstruct every one of the 16 participant measures independently of R.
for row in averages:
    pid = row['subject_no']
    expected = {}
    for condition in ('manual', 'aid_first', 'stimulus_first'):
        cell = by_cell[(pid, condition)]
        expected[f'switch_proportion_{condition}'] = statistics.mean(binary(r['changed_response']) for r in cell)
        key = f'decision2_accuracy_proportion_{condition}' + ('' if condition == 'manual' else '_overall')
        expected[key] = statistics.mean(binary(r['decision2_correct']) for r in cell)
        rating = [r for r in sliders if r['participant_id'] == pid and r['aid_condition'] == condition]
        assert len(rating) == 1
        expected[f'{condition}_reliability_rating_pct'] = float(rating[0]['response_percent'])
        if condition != 'manual':
            for correct, name in ((1, 'correct'), (0, 'incorrect')):
                subset = [r for r in cell if binary(r['aid_correct']) == correct]
                expected[f'decision2_accuracy_proportion_{condition}_aid_{name}'] = statistics.mean(binary(r['decision2_correct']) for r in subset)
            q = [r for r in questions if r['participant_id'] == pid and r['aid_condition'] == condition]
            assert len(q) == len({r['question_idx'] for r in q}) == 6
            expected[f'trust_mean_{condition}_1to5'] = statistics.mean(float(r['response']) for r in q)
    expected['aid_reliability_rating_mean_pct'] = statistics.mean(expected[f'{c}_reliability_rating_pct'] for c in ('aid_first', 'stimulus_first'))
    assert set(expected) == set(row) - {'subject_no'}
    for key, value in expected.items():
        close(row[key], value)

label_to_condition = {'Manual': 'manual', 'Aid first': 'aid_first', 'Stimulus first': 'stimulus_first'}
for row in read(OUT / 'switch_decomposition_participants.csv'):
    cell = by_cell[(row['subject_no'], label_to_condition[row['condition']])]
    if row['advice_subset'] != 'Overall':
        correct = row['advice_subset'] == 'Correct advice'
        cell = [r for r in cell if binary(r['aid_correct']) == correct]
    n = len(cell)
    beneficial = sum(binary(r['decision1_correct']) == 0 and binary(r['decision2_correct']) == 1 for r in cell)
    harmful = sum(binary(r['decision1_correct']) == 1 and binary(r['decision2_correct']) == 0 for r in cell)
    close(row['n_trials'], n)
    close(row['n_beneficial'], beneficial)
    close(row['n_harmful'], harmful)
    close(row['switch_proportion'], (beneficial + harmful) / n)
    close(row['net_accuracy_gain'], (beneficial - harmful) / n)

primary = read(OUT / 'primary_hypothesis_tests.csv')
paired = read(OUT / 'participant_level_sensitivity_tests.csv')
assert len(primary) == 14 and len(paired) == 12
for table in (primary, paired):
    assert {r['n_participants'] for r in table} == {'60'}
    for h in ('H2', 'H3', 'H4', 'H5'):
        rows = sorted((r for r in table if r['hypothesis'] == h), key=lambda r: float(r['p_value_raw']))
        assert len(rows) == 3
        maximum = 0
        for i, r in enumerate(rows):
            maximum = min(1, max(maximum, (3-i)*float(r['p_value_raw'])))
            close(r['p_value_adjusted'], maximum)
for r in read(OUT / 'research_question_contrasts.csv'):
    match = next(p for p in paired if p['hypothesis'] == r['hypothesis'] and p['contrast'] == r['paired_contrast'])
    close(r['difference_pp'], 100*float(match['estimate']))
    close(r['paired_conf_low_pp'], 100*float(match['conf_low']))
    close(r['paired_conf_high_pp'], 100*float(match['conf_high']))
for filename in ('model_diagnostics.csv', 'harmful_switch_model_diagnostics.csv', 'timing_advice_interaction_diagnostics.csv'):
    for row in read(OUT / filename):
        assert row['converged'] == 'TRUE' and row['singular'] == 'FALSE', (filename, row)
for row in read(OUT / 'analysis_input_checksums.csv'):
    assert hashlib.md5(Path(row['path']).read_bytes()).hexdigest() == row['md5']

class ReportParser(HTMLParser):
    def __init__(self):
        super().__init__()
        self.links = []
        self.text = []
    def handle_starttag(self, tag, attrs):
        if tag == 'a':
            self.links.append(dict(attrs)['href'])
    def handle_data(self, data):
        self.text.append(data)

for name in ('research_questions_report.html', 'switch_proportion_breakdowns.html'):
    parser = ReportParser()
    parser.feed((OUT / name).read_text())
    assert all((OUT / link).is_file() for link in parser.links)
    text = ' '.join(parser.text)
    assert '60 participants' in text and '20260924_110831' in text
    assert '59 participants analysed' not in text and 'Participant 59 excluded' not in text
    if name == 'research_questions_report.html':
        assert text.count('Predicted directions:') == 4
        for row in read(OUT / 'research_question_contrasts.csv'):
            for field in ('difference_pp', 'paired_conf_low_pp', 'paired_conf_high_pp'):
                assert f'{float(row[field]):.2f}' in text, (field, row)

# Regression: copying old files more recently must not override the run timestamp;
# a missing latest-run export must fail before any output CSV is written.
with tempfile.TemporaryDirectory(prefix='aid-onset-run-selection-') as tmp:
    base = Path(tmp)
    inputs, outputs = base / 'inputs', base / 'outputs'
    inputs.mkdir()
    for timestamp in ('20260903_120309', '20260924_110831'):
        for suffix in ('ALL', 'POSTBLOCK_ALL', 'POSTBLOCK_SLIDERS_ALL'):
            path = inputs / f'results_p059_{timestamp}_b00_{suffix}.csv'
            path.write_text(f'participant_id,run_timestamp\n59,{timestamp}\n')
            os.utime(path, (2000000000, 2000000000) if timestamp.startswith('20260903') else (1000000000, 1000000000))
    command = ['Rscript', str(ROOT / 'collate_data.R'), str(inputs), str(outputs)]
    result = subprocess.run(command, capture_output=True, text=True)
    assert result.returncode == 0, result.stderr
    selected = read(outputs / 'collation_manifest.csv')
    assert len(selected) == 3 and {r['run_timestamp'] for r in selected} == {'20260924_110831'}
    (inputs / 'results_p059_20260924_110831_b00_POSTBLOCK_ALL.csv').unlink()
    fail_outputs = base / 'incomplete-output'
    result = subprocess.run(command[:-1] + [str(fail_outputs)], capture_output=True, text=True)
    assert result.returncode != 0 and 'latest run' in result.stderr
    assert not list(fail_outputs.glob('*.csv'))
    for path in inputs.glob('*20260924*'):
        path.unlink()
    result = subprocess.run(command[:-1] + [str(base / 'old-only')], capture_output=True, text=True)
    assert result.returncode != 0 and 'earlier p59 run' in result.stderr

print('PASS: 60 participants; coherent replacement run; 960 independently recomputed measures;')
print('      binary transitions; contrast joins; Holm families; diagnostics; input checksums;')
print('      timestamp-vs-mtime, incomplete latest run, and excluded old-run regression cases.')
