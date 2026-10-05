"""Validate analysis exports against the live task allocation functions.

Usage: python validate_time_pressure_allocation.py INPUT_DIR OUTPUT_CSV
Run with the r-pygame interpreter. Never starts the participant task.
"""
import csv
import os
from collections import Counter
from pathlib import Path
import sys

os.environ.setdefault("PYGAME_HIDE_SUPPORT_PROMPT", "1")
import virus_task as task


def validate(input_dir):
    cycle = [task.counterbalance_allocation_for_participant(i) for i in range(1, 97)]
    assert len({(a["post_calibration_block_order"], a["reliability_pattern"],
                 a["key_flip"]) for a in cycle}) == 96
    assert set(Counter(a["post_calibration_block_order"] for a in cycle).values()) == {4}
    assert set(Counter(a["reliability_pattern"] for a in cycle).values()) == {48}
    assert set(Counter(a["key_flip"] for a in cycle).values()) == {48}
    assert cycle[0]["reliability_pattern"] == "HP95_LP65" and not cycle[0]["key_flip"]
    records, seen = [], set()
    files = sorted(Path(input_dir).glob("results_*_b00_ALL.csv"))
    if not files:
        raise ValueError("No complete trial exports found")
    for path in files:
        with path.open(newline="") as handle:
            rows = list(csv.DictReader(handle))
        if not rows:
            raise ValueError(f"Empty export: {path}")
        pid = int(rows[0]["participant_id"])
        if pid in seen:
            raise ValueError(f"Ambiguous duplicate run for participant {pid}")
        seen.add(pid)
        allocation = task.counterbalance_allocation_for_participant(pid)
        expected = task.build_blocks_for_participant(pid, task.BLOCKS)
        # BLOCKS is the task template; use its current trial counts and metadata.
        mapping = task.key_mapping_for_participant(pid)
        for idx, block in enumerate(expected, 1):
            actual = [r for r in rows if int(r["block_idx"]) == idx]
            if len(actual) != block["N_TRIALS"]:
                raise ValueError(f"p{pid} block {idx}: unexpected trial count")
            if sorted(int(r["trial"]) for r in actual) != list(range(1, block["N_TRIALS"] + 1)):
                raise ValueError(f"p{pid} block {idx}: duplicate or missing trials")
            for r in actual:
                checks = [
                    int(r["participant_id"]) == pid,
                    r["run_timestamp"] == rows[0]["run_timestamp"],
                    r["block"] == block["name"],
                    r["condition_deadline_code"] == block["CONDITION_DEADLINE_CODE"],
                    r["time_pressure_condition"] == block["TIME_PRESSURE_CONDITION"],
                    float(r["trial_deadline_s"]) == block["TRIAL_DEADLINE_MS"] / 1000,
                    r["automation_reliability_pattern"] == allocation["reliability_pattern"],
                    r["automation_reliability_group"] == block["AUTOMATION_RELIABILITY_GROUP"],
                    r["keymap_flip"].lower() == str(allocation["key_flip"]).lower(),
                    r["key_black"] == mapping["key_black_name"],
                    r["key_white"] == mapping["key_white_name"],
                ]
                if block["AUTOMATION_ON"]:
                    checks.append(float(r["aid_accuracy_setting"]) == block["AID_ACCURACY"])
                else:
                    checks.append(r["aid_accuracy_setting"] == "")
                if not all(checks):
                    raise ValueError(f"p{pid} block {idx}: metadata differs from task allocation")
        if len(rows) != sum(b["N_TRIALS"] for b in expected):
            raise ValueError(f"p{pid}: extra trial rows")
        question_path = path.with_name(path.name.replace("_ALL.csv", "_POSTBLOCK_ALL.csv"))
        with question_path.open(newline="") as handle:
            questions = list(csv.DictReader(handle))
        for q in questions:
            item_idx = int(q["question_idx"]) - 1
            if item_idx not in range(len(task.QUESTION_ITEMS)) or q["question"].strip() != task.QUESTION_ITEMS[item_idx]["question"].strip():
                raise ValueError(f"p{pid}: trust item text differs from the task")
        records.append(dict(participant_id=pid, run_timestamp=rows[0]["run_timestamp"],
                            pattern=allocation["reliability_pattern"],
                            order=f"O{allocation['post_calibration_block_order_idx'] + 1:02d}",
                            key_mapping="flipped" if allocation["key_flip"] else "standard",
                            n_trials=len(rows), allocation_valid=True))
    return records


if __name__ == "__main__":
    if len(sys.argv) != 3:
        raise SystemExit(__doc__)
    result = validate(sys.argv[1])
    with Path(sys.argv[2]).open("w", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(result[0]))
        writer.writeheader()
        writer.writerows(result)
    print(f"Validated {len(result)} participants and the full 96-cell allocation cycle.")
