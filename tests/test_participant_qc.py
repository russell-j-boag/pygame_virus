"""Synthetic integrity and rule-boundary tests; never modify study data."""
import copy
import csv
import json
import random
import tempfile
import unittest
from pathlib import Path
from participant_qc.core import (ROOT, REQUIRED, CONDITIONS, CATEGORIES, task, analyse,
    load_config, load_trials, screen, wilson, choose_comparators, exclusion_summary)
from participant_qc.run_qc import check_destination


def fixture(pid=1, source=None, timestamp="20260101_120000"):
    source = pid if source is None else source
    allocation = task.counterbalance_allocation_for_participant(source)
    mapping = task.key_mapping_for_participant(source)
    rows = []; t = 0
    for idx, block in enumerate(task.build_blocks_for_participant(source, task.BLOCKS), 1):
        for trial in range(1,block['N_TRIALS']+1):
            t += 1
            stimulus = 'BLACK' if trial % 2 else 'WHITE'
            other = 'WHITE' if stimulus == 'BLACK' else 'BLACK'
            ok = (trial-1) % 100 < 77
            aided = block['AUTOMATION_ON']
            aid_ok = (trial-1) % 100 < 100*block['AID_ACCURACY'] if aided else False
            row = dict.fromkeys(REQUIRED,'')
            row.update(participant_id=pid,source_participant_id=source,run_timestamp=timestamp,
                block=block['name'],block_idx=idx,trial=trial,global_trial=t,
                condition_deadline_code=block['CONDITION_DEADLINE_CODE'],time_pressure_condition=block['TIME_PRESSURE_CONDITION'],
                trial_deadline_s=block['TRIAL_DEADLINE_MS']/1000,auto_on=str(aided),
                automation_reliability_pattern=allocation['reliability_pattern'],
                automation_reliability_group=block['AUTOMATION_RELIABILITY_GROUP'],
                keymap_flip=str(allocation['key_flip']),key_black=mapping['key_black_name'],key_white=mapping['key_white_name'],
                aid_accuracy_setting=block['AID_ACCURACY'] if aided else '',
                stimulus=stimulus,response=stimulus if ok else other,correct=str(ok),
                feedback='CORRECT' if ok else 'INCORRECT',rt_s=.6,
                aid_label=(stimulus if aid_ok else other) if aided else '',aid_correct=str(aid_ok) if aided else '')
            rows.append(row)
    return rows


def parsed(rows):
    with tempfile.TemporaryDirectory() as tmp:
        path=Path(tmp)/'trials.csv'
        with path.open('w',newline='') as f:
            writer=csv.DictWriter(f,fieldnames=sorted(REQUIRED));writer.writeheader();writer.writerows(rows)
        return load_trials(path)


def correct(row, ok):
    row.update(response=row['stimulus'] if ok else ('WHITE' if row['stimulus']=='BLACK' else 'BLACK'),
               correct=str(ok),feedback='CORRECT' if ok else 'INCORRECT')


def block_rows(rows, code):return [r for r in rows if r['condition_deadline_code']==code]


class QCTests(unittest.TestCase):
    def setUp(self):self.cfg=load_config(ROOT/'participant_qc/config.json')
    def result(self, rows):return analyse(parsed(rows),self.cfg)
    def metric(self, rows, scope='M_LP'):
        return next(m for m in self.result(rows)['metrics'] if m['scope']==scope)

    def test_all_96_allocations_and_counts(self):
        cells=set()
        for pid in range(1,97):
            result=self.result(fixture(pid));s=result['summaries'][0]
            self.assertFalse(result['issues'],pid)
            self.assertEqual((len(result['metrics']),len(result['checks']),len(result['bins'])),(8,16,38))
            self.assertTrue(all(c['status']=='clear' for c in result['checks']))
            cells.add((s['main_block_order'],s['automation_reliability_pattern'],s['key_black']))
        self.assertEqual(len(cells),96)

    def test_target_and_chance_boundaries(self):
        raw=fixture(); rr=block_rows(raw,'M_LP')
        for count,flag in [(268,False),(267,True),(348,False),(349,True)]:
            for i,r in enumerate(rr):correct(r,i<count)
            m=self.metric(raw)
            self.assertEqual(screen(m,'manual_target',self.cfg)['status'],'flag' if flag else 'clear')
        for count,flag in [(220,True),(221,False)]:
            for i,r in enumerate(rr):correct(r,i<count)
            self.assertEqual(screen(self.metric(raw),'near_chance_or_worse',self.cfg)['status'],'flag' if flag else 'clear')

    def test_calibration_tail_only(self):
        raw=fixture()
        for r in raw[:150]:correct(r,False)
        result=self.result(raw)
        self.assertEqual(result['summaries'][0]['calibration_target'],'clear')
        for r in raw[150:300]:correct(r,False)
        self.assertEqual(self.result(raw)['summaries'][0]['calibration_target'],'flag')

    def test_manual_target_diagnostic_only_hp_no_target(self):
        raw=fixture()
        for code in ['M_LP','M_HP']:
            for i,r in enumerate(block_rows(raw,code)):correct(r,i<240)
        result=self.result(raw);s=result['summaries'][0]
        self.assertEqual(s['manual_target'],'flag');self.assertFalse(s['exclude_by_rules'])
        self.assertTrue(s['review_required']);self.assertEqual(s['n_exclusion_rules'],0)
        self.assertEqual(exclusion_summary(result)['diagnostic_only_participant_ids'],[1])
        checks=[c for c in result['checks'] if c['category']=='manual_target']
        self.assertEqual([c['scope'] for c in checks],['M_LP'])
        self.assertFalse(checks[0]['use_for_exclusion'])
        for i,r in enumerate(block_rows(raw,'M_LP')):correct(r,i<220)
        self.assertTrue(self.result(raw)['summaries'][0]['exclude_by_rules'])

    def test_fast_joint_rule_both_pressures(self):
        for code in ['M_HP','A_HP','M_LP','A_LP']:
            raw=fixture();rr=block_rows(raw,code)
            for i,r in enumerate(rr[:80]):r['rt_s']=.1;correct(r,i<44)
            rr[0]['rt_s']=0;rr[1]['rt_s']=-.001;rr[80]['rt_s']=.2
            m=self.metric(raw,code)
            self.assertEqual((m['n_fast'],m['n_nonpositive_rt']),(80,2))
            self.assertEqual(screen(m,'anticipatory',self.cfg)['status'],'flag')
            correct(rr[44],True)
            self.assertEqual(screen(self.metric(raw,code),'anticipatory',self.cfg)['status'],'clear')
            correct(rr[44],False);rr[79]['rt_s']=.2
            self.assertEqual(screen(self.metric(raw,code),'anticipatory',self.cfg)['status'],'clear')
        m.update(n_fast=19,fast_fraction=.2,fast_accuracy=.5)
        self.assertEqual(screen(m,'anticipatory',self.cfg)['status'],'clear')

    def test_timeouts_denominators_invalid_rt_and_empty_fast(self):
        raw=fixture();rr=block_rows(raw,'M_HP')
        rr[0].update(response='TIMEOUT',correct='False',feedback='TOO SLOW',rt_s='NA')
        m=self.metric(raw,'M_HP')
        self.assertTrue(m['assessable']);self.assertEqual(m['n_timeouts'],1)
        self.assertEqual(m['timeout_fraction'],1/400)
        self.assertEqual(m['answered_accuracy'],m['n_correct']/399)
        self.assertIsNone(m['fast_accuracy']);self.assertEqual(m['fast_fraction'],0)
        rr[0].update(correct='NA',rt_s=1.002)
        m=self.metric(raw,'M_HP')
        self.assertTrue(m['assessable']);self.assertEqual(m['n_timeouts_with_recorded_rt'],1)
        self.assertEqual(m['n_fast'],0)
        rr[1]['rt_s']='NaN'
        self.assertFalse(self.metric(raw,'M_HP')['assessable'])

    def test_incomplete_and_malformed_scope(self):
        raw=fixture();raw.remove(block_rows(raw,'A_HP')[4]);result=self.result(raw)
        metrics={m['scope']:m for m in result['metrics']}
        self.assertFalse(metrics['A_HP']['assessable']);self.assertFalse(metrics['aided_overall']['assessable'])
        self.assertTrue(metrics['M_LP']['assessable'])
        self.assertEqual(exclusion_summary(result)['incomplete_assessment_ids'],[1])
        for key,value in [('trial_deadline_s',3),('keymap_flip','True'),('block_idx',6),('aid_accuracy_setting',.65)]:
            raw=fixture();block_rows(raw,'A_HP')[0][key]=value
            self.assertFalse(self.metric(raw,'A_HP')['assessable'],key)
        raw=[r for r in fixture() if r['condition_deadline_code']!='M_LP']
        self.assertFalse(self.metric(raw)['assessable'])

    def test_noncontiguous_aided_scope(self):
        raw=fixture(17)
        for r in raw:correct(r,r['block']=='AUTOMATION')
        m=self.metric(raw,'aided_overall')
        self.assertEqual(m['n_trials'],800);self.assertEqual(m['accuracy'],1)

    def test_identity_correction_and_collision(self):
        raw=fixture(83,timestamp='20261015_125827')+fixture(84,83,'20261015_130522')
        people=parsed(raw);result=analyse(people,self.cfg)
        self.assertEqual(list(people),[83,84]);self.assertFalse(result['issues'])
        self.assertEqual(result['summaries'][0]['main_block_order'],result['summaries'][1]['main_block_order'])
        self.assertEqual(result['summaries'][1]['source_participant_id'],83)
        identity=exclusion_summary(result)['identity_reuse_cases']
        self.assertEqual(len(identity),1)
        self.assertEqual(identity[0]['analysis_participant_ids'],[83,84])
        self.assertEqual([r['run_timestamp'] for r in identity[0]['runs']],['20261015_125827','20261015_130522'])
        self.assertTrue(all(r['allocation_source_participant_id']==83 for r in identity[0]['runs']))
        result['primary_analysis']=dict(exclusions_applied=True,excluded_participant_ids=[4,10,48],included_participant_n=84)
        summary=exclusion_summary(result)
        self.assertTrue(summary['exclusions_applied'])
        self.assertFalse(summary['qc_data_filtered'])
        self.assertTrue(all(r['included_in_primary'] for r in summary['identity_reuse_cases'][0]['runs']))
        raw[-1]['participant_id']=83
        with self.assertRaisesRegex(ValueError,'Inconsistent'):parsed(raw)
        with self.assertRaisesRegex(ValueError,'Duplicate'):parsed(fixture()+fixture())
        raw=fixture();raw[-1]['run_timestamp']='another_run'
        with self.assertRaisesRegex(ValueError,'multiple runs'):parsed(raw)

    def test_unique_union_and_diagnostic_only(self):
        raw=fixture(1)+fixture(5)
        for r in raw:
            if r['participant_id']==1 and r['condition_deadline_code'] in ('M_HP','A_HP'):
                correct(r,r['trial']%2==0);r['rt_s']=.1
        result=self.result(raw);summary=exclusion_summary(result)
        self.assertEqual(summary['participant_ids'],[1]);self.assertEqual(summary['unique_flagged_n'],1)
        self.assertEqual(summary['remaining_if_applied_n'],1)
        self.assertEqual(result['summaries'][0]['n_exclusion_rules'],2)
        self.assertFalse(summary['exclusions_applied'])

    def test_comparator_order_independence_tie_and_empty(self):
        raw=fixture(1)+fixture(5)+fixture(9);result=self.result(raw)
        result['summaries'][0]['anticipatory']='flag'
        matches=choose_comparators(result);self.assertEqual(matches[0]['comparator'],5)
        self.assertEqual(matches[0]['ranked_candidates'][0]['score'],0)
        for s in result['summaries']:s['anticipatory']='flag'
        self.assertIsNone(choose_comparators(result)[0]['comparator'])
        baseline=self.result(raw);random.Random(42).shuffle(raw)
        self.assertEqual(baseline,self.result(raw))

    def test_peer_bands_align_conditions_not_global_positions(self):
        from participant_qc.plotting import peer_references
        raw=fixture(1)+fixture(5)
        for r in raw:
            if r['participant_id']==5:correct(r,r['condition_deadline_code']=='M_LP')
        result=self.result(raw)
        _,refs,pools,bands=peer_references(result)
        self.assertEqual(pools['1'],[5])
        self.assertTrue(all(b['accuracy_median']==(100 if b['scope']=='M_LP' else 0) for b in bands if b['participant_id']==1))

    def test_schema_empty_config_and_destinations(self):
        with tempfile.TemporaryDirectory() as tmp:
            p=Path(tmp)/'input.csv';p.write_text('participant_id,global_trial\n1,1\n')
            with self.assertRaisesRegex(ValueError,'Missing required'):load_trials(p)
            p.write_text(','.join(sorted(REQUIRED))+'\n')
            with self.assertRaisesRegex(ValueError,'No trial'):load_trials(p)
            p=Path(tmp)/'config.json';cfg={**self.cfg,'bin_size':37};p.write_text(json.dumps(cfg))
            with self.assertRaises(ValueError):load_config(p)
            out=Path(tmp)/'report';out.mkdir();(out/'existing.txt').write_text('preserve')
            with self.assertRaisesRegex(ValueError,'not owned'):check_destination(ROOT/'data/input.csv',out)
        for dest in [ROOT,ROOT/'data',ROOT/'config',ROOT/'plots/qc',ROOT/'analysis_outputs/semester2_2026_behavioural']:
            with self.assertRaises(ValueError):check_destination(ROOT/'data/input.csv',dest)

    def test_wilson(self):
        self.assertEqual(wilson(0,0),(None,None));lo,hi=wilson(10,20)
        self.assertAlmostEqual(lo,.2992980081982123,places=12)
        self.assertAlmostEqual(hi,.7007019918017877,places=12)

if __name__=='__main__':unittest.main()
