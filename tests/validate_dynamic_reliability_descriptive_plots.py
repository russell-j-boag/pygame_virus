"""Reconcile calibration summaries and verify the complete descriptive plot bundle."""
import csv
import hashlib
import math
import re
import subprocess
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
from PIL import Image

root = Path(__file__).resolve().parents[1]
base = root / 'plots/semester2_2026_data'
data = root / 'data'
with (data/'data_virus_all.csv').open() as f:
    trials = list(csv.DictReader(f))
ids = {r['participant_id'] for r in trials}
def label(pid):
    n, suffix = re.fullmatch(r'(\d+)([a-z]*)', pid).groups()
    return 'p'+n.zfill(3)+suffix
assert {p.name for p in (base/'individual').iterdir()} == {label(p) for p in ids}
values = defaultdict(list)
for r in trials:
    code = ('CAL' if r['block'] == 'CALIBRATION' else 'MAN_PRE' if r['manual_segment'] == 'PRE_AUTOMATION' else 'MAN_POST' if r['manual_segment'] == 'POST_AUTOMATION' else 'AUTOMATION')
    if code in ('MAN_PRE', 'MAN_POST') or (code == 'CAL' and int(r['trial']) > 150):
        values[r['participant_id'],code].append(int(r['correct'].upper() in ('TRUE','1')))
with (base/'calibration_manual_pre_post_accuracy_by_participant.csv').open() as f:
    summaries = list(csv.DictReader(f))
assert {r['participant_id'] for r in summaries} == ids and len(summaries) == len(ids)
for r in summaries:
    for col, code in [('calibration','CAL'),('manual_pre','MAN_PRE'),('manual_post','MAN_POST')]:
        v = values[r['participant_id'],code]
        assert int(r['n_trials_'+col]) == len(v)
        assert math.isclose(float(r['accuracy_'+col]), sum(v)/len(v), abs_tol=1e-12)
for pid in ids:
    folder = base/'individual'/label(pid)
    assert len(list(folder.glob('*.png'))) == len(list(folder.glob('*.pdf'))) == 5
files = sorted(p for p in base.rglob('*') if p.suffix in ('.png','.pdf'))
assert {p.with_suffix('') for p in files if p.suffix=='.png'} == {p.with_suffix('') for p in files if p.suffix=='.pdf'}
def check(path):
    if path.suffix == '.png':
        with Image.open(path) as im:
            assert im.width >= 1000 and im.height >= 900
            info = str(im.size)
            im.verify()
    else:
        result = subprocess.run(['pdfinfo',str(path)],capture_output=True,text=True,check=True)
        pages = int(re.search(r'^Pages:\s+(\d+)', result.stdout, re.M)[1])
        assert pages == 1
        info = '1 page'
    return dict(path=str(path.relative_to(root)),bytes=path.stat().st_size,
                sha256=hashlib.sha256(path.read_bytes()).hexdigest(),dimensions_or_pages=info)
with ThreadPoolExecutor(max_workers=6) as pool:
    records = list(pool.map(check, files))
with (data/'plot_artifact_manifest.csv').open('w') as f:
    writer=csv.DictWriter(f,fieldnames=records[0],lineterminator='\n');writer.writeheader();writer.writerows(records)
print(f'PASS: {len(ids)} individual plot sets; {len(files)//2} PDF/PNG pairs; all calibration/manual means match source trials.')
