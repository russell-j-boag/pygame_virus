"""Synthetic rule, provenance and allocation tests; no study data are modified."""
import copy
import csv
import json
import os
import sys
import tempfile
import unittest
from pathlib import Path

from participant_qc.core import (ROOT, REQUIRED, CONDITIONS, analyse, load_trials,
    load_config, screen, wilson, choose_comparators, exclusion_summary)
from participant_qc import study
from participant_qc.provenance import reconcile_sources
from participant_qc.run_qc import check_destination


def fixture(pid=1, timestamp="20260101_120000"):
    mapping = study.key_mapping_for_participant(pid)
    rows = []
    for idx, block in enumerate(study.build_blocks_for_participant(pid), 1):
        for trial in range(1, block['N_TRIALS']+1):
            stimulus = 'BLACK' if trial % 2 else 'WHITE'
            other = 'WHITE' if stimulus == 'BLACK' else 'BLACK'
            ok = trial % 5 != 0
            aided = block['AUTOMATION_ON']
            aid_ok = (trial-1) % 100 < 100*block['AID_ACCURACY']
            row = dict.fromkeys(sorted(REQUIRED), '')
            row.update(participant_id=pid, run_timestamp=timestamp, block=block['name'],
                block_idx=idx, trial=trial, global_trial=len(rows)+1, auto_on=str(aided),
                keymap_flip=str(mapping['flip']), key_black=mapping['key_black_name'],
                key_white=mapping['key_white_name'], aid_accuracy_setting=block['AID_ACCURACY'] if aided else '',
                stimulus=stimulus, response=stimulus if ok else other, correct=str(ok),
                feedback='CORRECT' if ok else 'INCORRECT', rt_s=.6,
                aid_label=(stimulus if aid_ok else other) if aided else '', aid_correct=str(aid_ok) if aided else '')
            rows.append(row)
    return rows


def write_trials(path, rows):
    with Path(path).open('w', newline='') as handle:
        writer = csv.DictWriter(handle, fieldnames=rows[0].keys())
        writer.writeheader(); writer.writerows(rows)


def parsed(rows):
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp)/'trials.csv'
        write_trials(path, rows)
        return load_trials(path)


def correct(row, ok):
    row.update(response=row['stimulus'] if ok else ('WHITE' if row['stimulus']=='BLACK' else 'BLACK'),
               correct=str(ok), feedback='CORRECT' if ok else 'INCORRECT')


def block_rows(rows, code):
    return [r for r in rows if r['block']==code]


class QCTests(unittest.TestCase):
    def setUp(self):
        self.cfg=load_config(ROOT/'participant_qc/config.json')

    def result(self, rows):
        return analyse(parsed(rows), self.cfg)

    def metric(self, rows, scope='MANUAL'):
        return next(m for m in self.result(rows)['metrics'] if m['scope']==scope)

    def test_all_twelve_allocations_and_task_contract(self):
        os.environ.setdefault('PYGAME_HIDE_SUPPORT_PROMPT','1')
        sys.path.insert(0, str(ROOT/'python'))
        from python import virus_task
        cells=set()
        for pid in range(1,13):
            result=self.result(fixture(pid)); summary=result['summaries'][0]
            self.assertFalse(result['issues'])
            self.assertEqual(tuple(len(result[k]) for k in ('metrics','checks','bins')),(7,14,36))
            self.assertTrue(all(c['status']=='clear' for c in result['checks']))
            cells.add((summary['main_block_order'],summary['key_black']))
            actual=virus_task.build_blocks_for_participant(pid,virus_task.BLOCKS)
            for audit, task in zip(study.build_blocks_for_participant(pid),actual):
                for key in ('name','N_TRIALS','AUTOMATION_ON','AID_ACCURACY'):
                    self.assertEqual(audit[key],task[key])
                self.assertEqual(task['TARGET_ACC'],.8)
            self.assertEqual(study.key_mapping_for_participant(pid),
                {k:virus_task.key_mapping_for_participant(pid)[k] for k in study.key_mapping_for_participant(pid)})
        self.assertEqual(len(cells),12)
        self.assertEqual(virus_task.TRIAL_DEADLINE_MS,10000)

    def test_target_and_chance_boundaries(self):
        raw=fixture(); rr=block_rows(raw,'MANUAL')
        for count,flag in [(350,False),(349,True),(450,False),(451,True)]:
            for i,r in enumerate(rr):correct(r,i<count)
            self.assertEqual(screen(self.metric(raw),'manual_target',self.cfg)['status'],'flag' if flag else 'clear')
        for count,flag in [(275,True),(276,False)]:
            for i,r in enumerate(rr):correct(r,i<count)
            self.assertEqual(screen(self.metric(raw),'near_chance_or_worse',self.cfg)['status'],'flag' if flag else 'clear')
        for count,flag in [(105,False),(104,True),(135,False),(136,True)]:
            for i,r in enumerate(raw[150:300]):correct(r,i<count)
            self.assertEqual(self.result(raw)['summaries'][0]['calibration_target'],'flag' if flag else 'clear')

    def test_calibration_tail_and_manual_diagnostic(self):
        raw=fixture()
        for r in raw[:150]:correct(r,False)
        self.assertEqual(self.result(raw)['summaries'][0]['calibration_target'],'clear')
        raw=fixture()
        for i,r in enumerate(block_rows(raw,'MANUAL')):correct(r,i<300)
        result=self.result(raw)
        self.assertEqual(result['summaries'][0]['manual_target'],'flag')
        self.assertFalse(result['summaries'][0]['exclude_by_rules'])
        self.assertEqual(exclusion_summary(result)['diagnostic_only_participant_ids'],[1])

    def test_joint_fast_boundaries_all_whole_blocks(self):
        for code in CONDITIONS:
            raw=fixture();rr=block_rows(raw,code); n=len(rr)//5
            for i,r in enumerate(rr[:n]):r['rt_s']=.1;correct(r,i<int(.55*n))
            rr[0]['rt_s']=0;rr[1]['rt_s']=-.001;rr[n]['rt_s']=.2
            m=self.metric(raw,code)
            self.assertEqual((m['n_fast'],m['n_nonpositive_rt']),(n,2))
            self.assertEqual(screen(m,'anticipatory',self.cfg)['status'],'flag')
            correct(rr[int(.55*n)],True)
            self.assertEqual(screen(self.metric(raw,code),'anticipatory',self.cfg)['status'],'clear')
            correct(rr[int(.55*n)],False);rr[n-1]['rt_s']=.2
            self.assertEqual(screen(self.metric(raw,code),'anticipatory',self.cfg)['status'],'clear')
        m.update(n_fast=19,fast_fraction=.2,fast_accuracy=.5)
        self.assertEqual(screen(m,'anticipatory',self.cfg)['status'],'clear')
        m['n_fast']=20
        self.assertEqual(screen(m,'anticipatory',self.cfg)['status'],'flag')

    def test_timeouts_nonpositive_deadline_and_empty_fast(self):
        raw=fixture();rr=block_rows(raw,'MANUAL')
        rr[0].update(response='TIMEOUT',correct='NA',feedback='TOO SLOW',rt_s=-.01)
        rr[1]['rt_s']=10.001
        m=self.metric(raw)
        self.assertTrue(m['assessable'])
        self.assertEqual((m['n_timeouts'],m['n_timeouts_with_recorded_rt'],m['n_fast'],m['n_beyond_deadline']),(1,1,0,1))
        self.assertEqual(m['timeout_fraction'],1/500)
        self.assertEqual(m['answered_accuracy'],m['n_correct']/499)
        self.assertIsNone(m['fast_accuracy'])
        rr[2]['rt_s']='NaN'
        self.assertFalse(self.metric(raw)['assessable'])

    def test_missing_and_invalid_scopes(self):
        raw=fixture();raw.remove(block_rows(raw,'AUTOMATION1')[4]);result=self.result(raw)
        m={m['scope']:m for m in result['metrics']}
        self.assertFalse(m['AUTOMATION1']['assessable']);self.assertFalse(m['aided_overall']['assessable'])
        self.assertTrue(m['MANUAL']['assessable'])
        self.assertEqual(exclusion_summary(result)['incomplete_assessment_ids'],[1])
        for key,value in [('keymap_flip','True'),('block_idx',6),('aid_accuracy_setting',.65),('feedback','CORRECT'),('response','unknown')]:
            raw=fixture();block_rows(raw,'AUTOMATION1')[4][key]=value
            self.assertFalse(self.metric(raw,'AUTOMATION1')['assessable'],key)
        raw=[r for r in fixture() if r['block']!='MANUAL']
        self.assertFalse(self.metric(raw)['assessable'])
        self.assertEqual(self.result(raw)['summaries'][0]['manual_target'],'not_assessable')

    def test_schema_duplicates_and_multiple_runs_are_fatal(self):
        with self.assertRaisesRegex(ValueError,'Duplicate'):parsed(fixture()+fixture())
        raw=fixture();raw[-1]['run_timestamp']='another_run'
        with self.assertRaisesRegex(ValueError,'multiple runs'):parsed(raw)
        raw=fixture()
        for r in raw:del r['correct']
        with self.assertRaisesRegex(ValueError,'Missing required'):parsed(raw)

    def test_noncontiguous_aided_scope_and_unique_union(self):
        raw=fixture(3)
        for r in raw:correct(r,r['block'].startswith('AUTOMATION'))
        self.assertEqual(self.metric(raw,'aided_overall')['accuracy'],1)
        raw=fixture()
        for r in raw:correct(r,False);r['rt_s']=.1
        exclusions=exclusion_summary(self.result(raw))
        self.assertEqual(exclusions['participant_ids'],[1])
        self.assertEqual(exclusions['unique_flagged_n'],1)
        self.assertEqual(exclusions['overlap'][2]['participant_n'],1)

    def test_comparator_order_keys_mad_ties_and_empty(self):
        raw=sum((fixture(p) for p in [1,2,7,13,25,37]),[])
        for r in raw:
            if r['participant_id']==1 and r['block']=='MANUAL':r['rt_s']=.1;correct(r,False)
        result=self.result(raw)
        match=choose_comparators(result)[0]
        self.assertEqual([r['participant_id'] for r in match['ranked_candidates']],[13,25,37])
        self.assertEqual(match['comparator'],13)
        self.assertTrue(all(r['score']==0 for r in match['ranked_candidates']))
        for m in result['metrics']:
            if m['scope']=='experimental_overall' and m['participant_id'] in [13,25,37]:
                m['median_positive_rt_ms']={13:100,25:600,37:700}[m['participant_id']]
        self.assertEqual(choose_comparators(result)[0]['comparator'],25)
        for s in result['summaries']:
            if s['participant_id'] in [13,25,37]:s['n_not_assessable']=1
        self.assertIsNone(choose_comparators(result)[0]['comparator'])

    def test_zero_candidate_and_peer_alignment(self):
        from participant_qc.plotting import peer_references
        result=self.result(fixture()+fixture(2)+fixture(7))
        self.assertEqual(exclusion_summary(result)['participant_ids'],[])
        self.assertEqual(choose_comparators(result),[])
        _,_,pools,bands=peer_references(result)
        self.assertEqual(pools['1'],[2,7])
        self.assertTrue(all(b['peer_n']==2 for b in bands))
        self.assertTrue(all(abs(b['accuracy_median']-80)<1e-10 for b in bands))

    def test_bins_do_not_trigger_rules_and_other_flags_allow_comparison(self):
        raw=fixture()
        for r in block_rows(raw,'MANUAL')[:20]:r['rt_s']=.1;correct(r,False)
        result=self.result(raw)
        self.assertEqual(result['summaries'][0]['anticipatory'],'clear')
        self.assertTrue(any(b['fast_fraction']>=.2 and b['fast_accuracy']==0 for b in result['bins'] if b['n_fast']))
        raw=fixture()+fixture(13)
        for r in raw:
            if r['participant_id']==1 and r['block']=='MANUAL':r['rt_s']=.1;correct(r,False)
            if r['participant_id']==13 and r['block']=='CALIBRATION':correct(r,False)
        result=self.result(raw)
        self.assertTrue(result['summaries'][1]['exclude_by_rules'])
        self.assertEqual(choose_comparators(result)[0]['comparator'],13)

    def test_raw_source_reconciliation_alternatives_and_mismatch(self):
        with tempfile.TemporaryDirectory() as tmp:
            tmp=Path(tmp);raw=tmp/'raw';raw.mkdir();source=tmp/'canonical.csv'
            rows=fixture();write_trials(source,rows)
            selected=raw/'results_p001_20260101_120000_b00_ALL.csv';write_trials(selected,rows)
            alternative=raw/'results_p001_20250101_120000_b00_ALL.csv';write_trials(alternative,fixture(timestamp='20250101_120000'))
            ledger=reconcile_sources(source,raw)
            self.assertEqual(sum(r['selected'] for r in ledger),1)
            rows[0]['rt_s']=.7;write_trials(selected,rows)
            with self.assertRaisesRegex(ValueError,'source mismatch'):reconcile_sources(source,raw)
            write_trials(selected,fixture())
            write_trials(raw/'results_p1_20260101_120000_b00_ALL.csv',fixture())
            with self.assertRaisesRegex(ValueError,'Ambiguous'):reconcile_sources(source,raw)
            selected.unlink();(raw/'results_p1_20260101_120000_b00_ALL.csv').unlink()
            with self.assertRaisesRegex(ValueError,'Missing raw sources'):reconcile_sources(source,raw)

    def test_config_and_destination_guards(self):
        for p in [ROOT,ROOT/'data/new',ROOT/'source_archives/new',ROOT/'plots',ROOT/'python']:
            with self.assertRaises(ValueError):check_destination(ROOT/'data/data_virus_all.csv',p)
        with tempfile.TemporaryDirectory() as tmp:
            tmp=Path(tmp)
            cfg=copy.deepcopy(self.cfg);cfg['bin_size']=40
            path=tmp/'config.json';path.write_text(json.dumps(cfg))
            with self.assertRaises(ValueError):load_config(path)
            with self.assertRaises(ValueError):check_destination(ROOT/'data/data_virus_all.csv',tmp/'config.json',path)
            with self.assertRaises(ValueError):check_destination(ROOT/'data/data_virus_all.csv',tmp/'raw/new',raw_dir=tmp/'raw')
            with self.assertRaises(ValueError):check_destination(ROOT/'data/data_virus_all.csv',tmp)

    def test_wilson(self):
        self.assertEqual(wilson(0,0),(None,None))
        self.assertAlmostEqual(wilson(50,100)[0],.4038315303659956)
        self.assertAlmostEqual(wilson(50,100)[1],.5961684696340044)


if __name__=='__main__':
    unittest.main()
