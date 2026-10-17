"""Synthetic rule, design, provenance and publication-safety regressions."""
import contextlib
import csv
import hashlib
import io
import json
import tempfile
import unittest
from unittest.mock import patch
from pathlib import Path

from participant_qc.core import (ROOT, REQUIRED, MAIN, analyse, load_config, load_dataset,
    parse_rows, screen, choose_comparators, exclusion_summary, task)
from participant_qc.run_qc import check_destination, main as run_qc


def fixture(pid=1, timestamp='20260101_120000'):
    order = [task.block_condition_code(b) for b in task.build_blocks_for_participant(pid, task.BLOCKS)]
    mapping = task.key_mapping_for_participant(pid)
    rows=[]
    for idx,code in enumerate(['PRACTICE',*order]):
        for t in range(1,(60 if idx==0 else 260)+1):
            stimulus='BLACK' if t%2 else 'WHITE'; other='WHITE' if stimulus=='BLACK' else 'BLACK'
            aided=code in ('AIDFIRST','STIMFIRST'); ok=t%4!=0
            aid=stimulus if t%20<17 else other
            r=dict.fromkeys(REQUIRED,'')
            r.update(participant_id=pid,run_timestamp=timestamp,block_idx=idx,condition_code=code,trial=t,
                global_trial=t if idx==0 else (idx-1)*260+t,
                key_black=mapping['key_black_name'],key_white=mapping['key_white_name'],keymap_flip=str(mapping['flip']),
                aid_condition={'PRACTICE':'manual','MANUAL':'manual','AIDFIRST':'aid_first','STIMFIRST':'stimulus_first'}[code],
                difficulty_mode='staircase' if idx==0 else 'fixed_delta',staircase_target_accuracy=.75 if idx==0 else '',
                stimulus=stimulus,auto_on=int(aided),aid_label=aid if aided else '',aid_correct=str(aid==stimulus) if aided else '',aid_accuracy_setting=.85 if aided else '',
                preview_display='aid_only' if code=='AIDFIRST' else 'masked_placeholder',preview_label=aid if code=='AIDFIRST' else '#####',
                decision2_preview_display='aid_only' if code=='STIMFIRST' else 'masked_placeholder',decision2_preview_label=aid if code=='STIMFIRST' else '#####',
                decision1_display='stimulus_only',decision2_display='blank_response')
            for d in (1,2):
                r[f'decision{d}_rt_s']=.6
                r[f'decision{d}_response']=stimulus if ok else other
                r[f'decision{d}_correct']=str(ok)
                r[f'decision{d}_matches_aid']=str(r[f'decision{d}_response']==aid) if aided else ''
            r['changed_response']='False';r['feedback']='CORRECT' if ok else 'INCORRECT'
            rows.append(r)
    return rows


def correct(row,d,ok):
    other='WHITE' if row['stimulus']=='BLACK' else 'BLACK'
    row[f'decision{d}_response']=row['stimulus'] if ok else other
    row[f'decision{d}_correct']=str(ok)
    if row['aid_label']:
        row[f'decision{d}_matches_aid']=str(row[f'decision{d}_response']==row['aid_label'])
    row['changed_response']=str(row['decision1_response']!=row['decision2_response'])
    if d==2:row['feedback']='CORRECT' if ok else 'INCORRECT'


def write_csv(path,rows,fields=None):
    with Path(path).open('w',newline='') as f:
        w=csv.DictWriter(f,fieldnames=fields or rows[0].keys());w.writeheader();w.writerows(rows)


def source_fixture(directory,pid=1,timestamp='20260101_120000'):
    directory=Path(directory);rows=fixture(pid,timestamp)
    main=[r for r in rows if r['condition_code']!='PRACTICE'];cal=rows[:60]
    canonical=directory/'combined.csv';write_csv(canonical,main)
    recs=[]
    for kind,suffix in (('trials','ALL'),('questionnaire','POSTBLOCK_ALL'),('sliders','POSTBLOCK_SLIDERS_ALL')):
        source=directory/f'results_p{pid:03d}_{timestamp}_b00_{suffix}.csv'
        write_csv(source,main if kind=='trials' else [dict(participant_id=pid,run_timestamp=timestamp)])
        recs.append(dict(participant_id=pid,run_timestamp=timestamp,export_type=kind,source_file=str(source),source_md5=hashlib.md5(source.read_bytes()).hexdigest()))
    manifest=directory/'manifest.csv';write_csv(manifest,recs)
    practice=directory/f'results_p{pid:03d}_{timestamp}_b00_PRACTICE.csv';write_csv(practice,cal)
    delta=directory/f'delta_p{pid:03d}_{timestamp}_b00_PRACTICE.csv'
    write_csv(delta,[dict(participant_id=pid,run_timestamp=timestamp,staircase_target_accuracy=.75,burnin_trials_excluded=20,n_trials_summarised=40,n_trials_total=60)])
    return canonical,manifest,practice,delta


class QCTests(unittest.TestCase):
    def setUp(self):self.cfg=load_config(ROOT/'participant_qc/config.json')
    def result(self,rows):return analyse(parse_rows(rows),self.cfg)
    def metric(self,rows,scope='MANUAL',d=1):
        return next(m for m in self.result(rows)['metrics'] if m['scope']==scope and m['decision']==d)

    def test_all_counterbalances_and_partial_bins(self):
        cells=set()
        for pid in range(1,13):
            r=self.result(fixture(pid));s=r['summaries'][0]
            self.assertFalse(r['issues']);self.assertFalse(s['exclude_by_rules'])
            self.assertEqual((len(r['metrics']),len(r['checks']),len(r['bins'])),(13,26,48))
            self.assertTrue(all(c['status']=='clear' for c in r['checks']))
            self.assertEqual([b['n_trials'] for b in r['bins'] if b['scope']=='MANUAL' and b['decision']==1],[50]*5+[10])
            cells.add((s['main_block_order'],s['key_black']))
        self.assertEqual(len(cells),12)

    def test_accuracy_boundary_both_decisions_all_main_scopes(self):
        for d in (1,2):
            for code in MAIN:
                rows=fixture();rr=[r for r in rows if r['condition_code']==code]
                for count,flag in ((143,True),(144,False)):
                    for i,row in enumerate(rr):correct(row,d,i<count)
                    m=self.metric(rows,code,d)
                    self.assertEqual(screen(m,'near_chance_or_worse',self.cfg)['status'],'flag' if flag else 'clear')
                    self.assertTrue(screen(m,'near_chance_or_worse',self.cfg)['use_for_exclusion'])
                other=self.metric(rows,code,3-d)
                self.assertEqual(other['accuracy'],.75)

    def test_target_boundaries_diagnostic_only(self):
        for count,flag in ((26,False),(25,True),(34,False),(35,True)):
            rows=fixture();tail=rows[20:60]
            for i,r in enumerate(tail):correct(r,2,i<count)
            result=self.result(rows);c=next(c for c in result['checks'] if c['category']=='calibration_target')
            self.assertEqual(c['status'],'flag' if flag else 'clear')
            self.assertFalse(c['use_for_exclusion']);self.assertFalse(result['summaries'][0]['exclude_by_rules'])
        rows=fixture()
        for r in rows:
            if r['condition_code']=='MANUAL':correct(r,2,True)
        self.assertTrue(self.result(rows)['summaries'][0]['diagnostic_only'])

    def test_fast_rule_joint_boundaries_and_prepared_decisions(self):
        for code in MAIN:
            for d in (1,2):
                rows=fixture();rr=[r for r in rows if r['condition_code']==code]
                for i,r in enumerate(rr[:52]):
                    r[f'decision{d}_rt_s']=.1;correct(r,d,i<28)
                rr[0][f'decision{d}_rt_s']=0;rr[1][f'decision{d}_rt_s']=-.001;rr[52][f'decision{d}_rt_s']=.2
                m=self.metric(rows,code,d)
                self.assertEqual((m['n_fast'],m['n_nonpositive_rt']),(52,2))
                self.assertEqual(screen(m,'anticipatory',self.cfg)['status'],'flag')
                self.assertTrue(screen(m,'anticipatory',self.cfg)['use_for_exclusion'])
                correct(rr[28],d,True)
                self.assertEqual(screen(self.metric(rows,code,d),'anticipatory',self.cfg)['status'],'clear')
                correct(rr[28],d,False);rr[51][f'decision{d}_rt_s']=.2
                self.assertEqual(screen(self.metric(rows,code,d),'anticipatory',self.cfg)['status'],'clear')
        m.update(n_fast=19,fast_fraction=.5,fast_accuracy=.5)
        self.assertEqual(screen(m,'anticipatory',self.cfg)['status'],'clear')
        m.update(n_fast=20,fast_fraction=.2,fast_accuracy=.55)
        self.assertEqual(screen(m,'anticipatory',self.cfg)['status'],'flag')

    def test_practice_all_diagnostic_and_burnin(self):
        rows=fixture()
        for r in rows[:60]:
            for d in (1,2):correct(r,d,False);r[f'decision{d}_rt_s']=.1
        result=self.result(rows)
        self.assertFalse(result['summaries'][0]['exclude_by_rules'])
        self.assertTrue(all(not c['use_for_exclusion'] for c in result['checks'] if c['scope'] in ('PRACTICE','calibration_tail')))
        rows=fixture()
        for r in rows[:20]:correct(r,2,False)
        c=next(c for c in self.result(rows)['checks'] if c['category']=='calibration_target')
        self.assertEqual(c['status'],'clear')

    def test_timeout_missing_rt_and_empty_fast(self):
        rows=fixture();r=next(r for r in rows if r['condition_code']=='MANUAL')
        r.update(decision1_response='TIMEOUT',decision1_correct='',decision1_rt_s=.05,changed_response='True')
        m=self.metric(rows)
        self.assertTrue(m['assessable']);self.assertEqual(m['n_timeouts'],1)
        self.assertEqual(m['n_fast'],0);self.assertIsNone(m['fast_accuracy'])
        self.assertEqual(m['n_timeouts_with_recorded_rt'],1)
        self.assertEqual(m['accuracy'],m['n_correct']/260)
        self.assertEqual(m['answered_accuracy'],m['n_correct']/259)
        r['decision1_response']=r['decision2_response'];r['decision1_correct']=r['decision2_correct'];r['decision1_rt_s']='NaN';r['changed_response']='False'
        self.assertFalse(self.metric(rows)['assessable']);self.assertTrue(self.metric(rows,d=2)['assessable'])

    def test_missing_block_propagates_only_affected_scopes(self):
        rows=[r for r in fixture() if r['condition_code']!='AIDFIRST']
        result=self.result(rows)
        for m in result['metrics']:
            self.assertEqual(m['assessable'],m['scope'] not in ('AIDFIRST','aided_overall','experimental_overall'))
        self.assertEqual(exclusion_summary(result)['incomplete_assessment_ids'],[1])

    def test_integrity_and_duplicate_runs(self):
        for key,value in (('key_black','Q'),('trial_deadline_s',1),('preview_label','WRONG'),('changed_response','True')):
            rows=fixture();next(r for r in rows if r['condition_code']=='MANUAL')[key]=value
            self.assertFalse(self.metric(rows)['assessable'])
        with self.assertRaisesRegex(ValueError,'Duplicate'):parse_rows(fixture()+fixture())
        with self.assertRaisesRegex(ValueError,'multiple runs'):parse_rows(fixture()+fixture(timestamp='20260102_120000'))

    def test_union_and_noncontiguous_pool(self):
        rows=fixture(5)+fixture(6)
        for r in rows:
            if r['participant_id']==5 and r['condition_code'] in ('AIDFIRST','STIMFIRST'):
                for d in (1,2):correct(r,d,r['trial']%2==0);r[f'decision{d}_rt_s']=.1
        result=self.result(rows);s=exclusion_summary(result)
        self.assertEqual(s['participant_ids'],[5]);self.assertEqual(s['rule_counts'],{'near_chance_or_worse':1,'anticipatory':1})
        self.assertEqual(s['overlap_counts'],{2:1})
        mm=next(m for m in result['metrics'] if m['participant_id']==5 and m['scope']=='aided_overall' and m['decision']==1)
        self.assertEqual((mm['n_trials'],mm['accuracy'],mm['fast_fraction']),(520,.5,1))

    def test_comparator_ties_missing_and_other_flags(self):
        result=self.result(fixture(1)+fixture(3)+fixture(5))
        result['summaries'][0]['anticipatory']='flag'
        result['summaries'][1]['near_chance_or_worse']='flag'
        self.assertEqual(choose_comparators(result)[0]['comparator'],3)
        result['summaries'][1]['n_not_assessable']=1;result['summaries'][2]['anticipatory']='flag'
        self.assertIsNone(choose_comparators(result)[0]['comparator'])

    def test_exact_run_selection_missing_practice_and_p59(self):
        with tempfile.TemporaryDirectory() as td:
            canonical,manifest,practice,delta=source_fixture(td,59,'20260924_110831')
            old=Path(td)/'results_p059_20260903_120309_b00_PRACTICE.csv'
            old.write_text(practice.read_text().replace('20260924_110831','20260903_120309'))
            people,sources,issues=load_dataset(canonical,manifest)
            self.assertEqual(people[59][0]['run_timestamp'],'20260924_110831');self.assertEqual(len(people[59]),840)
            self.assertNotIn(str(old),[s['source_file'] for s in sources])
            practice.unlink()
            people,sources,issues=load_dataset(canonical,manifest)
            result=analyse(people,self.cfg,issues)
            self.assertEqual(len(people[59]),780);self.assertEqual(result['summaries'][0]['calibration_target'],'not_assessable')

    def test_provenance_mismatch_and_missing_schema_stop(self):
        with tempfile.TemporaryDirectory() as td:
            canonical,manifest,practice,delta=source_fixture(td)
            practice.write_text(practice.read_text().replace('20260101_120000','20260102_120000'))
            with self.assertRaisesRegex(ValueError,'Practice identity'):load_dataset(canonical,manifest)
        with tempfile.TemporaryDirectory() as td:
            canonical,manifest,practice,delta=source_fixture(td)
            canonical.write_text(canonical.read_text().replace('0.6','0.7',1))
            with self.assertRaisesRegex(ValueError,'Canonical/raw mismatch'):load_dataset(canonical,manifest)
        with tempfile.TemporaryDirectory() as td:
            canonical,manifest,practice,delta=source_fixture(td)
            practice.write_text(practice.read_text().replace('decision1_rt_s','wrong_rt'))
            with self.assertRaisesRegex(ValueError,'Missing required'):load_dataset(canonical,manifest)

    def test_publication_protection(self):
        for path in (ROOT,ROOT/'data',ROOT/'plots/qc',ROOT/'analysis_outputs/semester2_2026_hypotheses'):
            with self.assertRaises(ValueError):check_destination(path,[])
        with tempfile.TemporaryDirectory() as td:
            path=Path(td)/'report';path.mkdir();(path/'unrelated.txt').write_text('preserve')
            with self.assertRaises(ValueError):check_destination(path,[])
            (path/'manifest.json').write_text(json.dumps({'generator':'participant_qc','study':'aid_onset'}))
            check_destination(path,[])
            with self.assertRaises(ValueError):check_destination(path,[path/'input.csv'])

    def test_staged_publication_failure_and_previous_archive(self):
        with tempfile.TemporaryDirectory() as td:
            canonical,manifest,practice,delta=source_fixture(td)
            out=Path(td)/'audit';config=Path(td)/'config.json'
            config.write_text(json.dumps(self.cfg))
            argv=['--input',str(canonical),'--manifest',str(manifest),'--config',str(config),'--output-dir',str(out)]
            empty_plots=([],[],[],{},[])
            with patch('participant_qc.plotting.generate_plots',return_value=empty_plots),contextlib.redirect_stdout(io.StringIO()):run_qc(argv)
            first={str(p.relative_to(out)):p.read_bytes() for p in out.rglob('*') if p.is_file()}
            with patch('participant_qc.plotting.generate_plots',side_effect=RuntimeError('synthetic plot failure')),contextlib.redirect_stdout(io.StringIO()):
                with self.assertRaisesRegex(RuntimeError,'synthetic plot'):run_qc(argv)
            self.assertEqual(first,{str(p.relative_to(out)):p.read_bytes() for p in out.rglob('*') if p.is_file()})
            def changed_source(*args):
                config.write_text(json.dumps(self.cfg)+'\n')
                return empty_plots
            with patch('participant_qc.plotting.generate_plots',side_effect=changed_source),contextlib.redirect_stdout(io.StringIO()):
                with self.assertRaisesRegex(RuntimeError,'changed during generation'):run_qc(argv)
            self.assertEqual(first,{str(p.relative_to(out)):p.read_bytes() for p in out.rglob('*') if p.is_file()})
            with patch('participant_qc.plotting.generate_plots',return_value=empty_plots),contextlib.redirect_stdout(io.StringIO()):run_qc(argv)
            archived=list(Path(td).glob('audit.previous_*'))
            self.assertEqual(len(archived),1)
            self.assertEqual(first,{str(p.relative_to(archived[0])):p.read_bytes() for p in archived[0].rglob('*') if p.is_file()})


if __name__=='__main__':unittest.main()
