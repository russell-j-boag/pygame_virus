#!/usr/bin/env python3
"""Build synthetic review pages and exercise safe report replacement/failure."""
import argparse
import json
import subprocess
import sys
from pathlib import Path

ROOT=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(ROOT))
from test_participant_qc import fixture, correct, write_trials
from validate_participant_qc import validate, sha


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--work-dir',type=Path,default=ROOT/'analysis_outputs/semester1_2026_qc_validation/synthetic')
    args=parser.parse_args();work=args.work_dir.resolve()
    # A dedicated empty directory prevents fixture creation from overwriting data.
    if work.exists() and any(work.iterdir()):raise ValueError('Use an empty synthetic work directory')
    work.mkdir(parents=True,exist_ok=True);raw=work/'raw';raw.mkdir()
    records=[]
    for pid in [1,2,7,13,25,37]:
        rows=fixture(pid)
        if pid in [1,7,13,25]:
            for r in rows:
                if r['block']=='MANUAL':r['rt_s']=.1;correct(r,False)
            rows[305]['rt_s']=0;rows[306]['rt_s']=-.003
        if pid==2:rows=[r for r in rows if r['global_trial']!=305]
        records+=rows
        write_trials(raw/f'results_p{pid:03d}_20260101_120000_b00_ALL.csv',rows)
    source=work/'input.csv';write_trials(source,records)
    report=work/'report'
    command=[sys.executable,str(ROOT/'participant_qc/run_qc.py'),'--input',str(source),'--raw-dir',str(raw),'--output-dir',str(report)]
    subprocess.run(command,check=True)
    first=validate(report);assert first['candidate_ids']==[1,7,13,25]
    assert first['pdf_pages']['anticipatory_comparisons.pdf']==3
    before=sha(report/'manifest.json')
    subprocess.run(command,check=True)
    backups=list(work.glob('report.previous_*'));assert len(backups)==1 and sha(backups[0]/'manifest.json')==before
    receipt=validate(report)
    published=sha(report/'manifest.json')
    altered=fixture(37);altered[0]['rt_s']=.9
    path=raw/'results_p037_20260101_120000_b00_ALL.csv';original=path.read_bytes()
    try:
        write_trials(path,altered)
        rejected=subprocess.run(command,capture_output=True,text=True)
        assert rejected.returncode!=0 and 'source mismatch' in rejected.stderr
        assert sha(report/'manifest.json')==published
    finally:path.write_bytes(original)
    receipt.update(backup_preserved=True,source_mismatch_rejected=True,published_report_unchanged_on_failure=True,
                   smoke_script_sha256=sha(__file__))
    (work/'validation.json').write_text(json.dumps(receipt,indent=2)+'\n')
    print('Synthetic comparisons, missing scopes, independent validation, backup and rejection checks passed.')


if __name__=='__main__':main()
