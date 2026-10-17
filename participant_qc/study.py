"""Frozen Semester 1 2026 audit contract; independent of native input capture.

Allocation is checked against the task in the test suite. Keeping this contract
local prevents future task defaults from silently changing historical screens.
"""
from itertools import permutations

CALIBRATION_TARGET_ACCURACY = .8
TRIAL_DEADLINE_MS = 10000
BLOCKS = [
    dict(name="CALIBRATION", N_TRIALS=300, AUTOMATION_ON=False, AID_ACCURACY=.9),
    dict(name="MANUAL", N_TRIALS=500, AUTOMATION_ON=False, AID_ACCURACY=.9),
    dict(name="AUTOMATION1", N_TRIALS=500, AUTOMATION_ON=True, AID_ACCURACY=.95),
    dict(name="AUTOMATION2", N_TRIALS=500, AUTOMATION_ON=True, AID_ACCURACY=.65),
]


def build_blocks_for_participant(participant_id, blocks_template=BLOCKS):
    blocks = {b["name"]: dict(b, TRIAL_DEADLINE_MS=TRIAL_DEADLINE_MS) for b in blocks_template}
    order = list(permutations(["MANUAL", "AUTOMATION1", "AUTOMATION2"]))[(participant_id - 1) % 6]
    return [blocks[name] for name in ("CALIBRATION", *order)]


def key_mapping_for_participant(participant_id):
    flip = bool(((participant_id - 1) // 6) % 2)
    return dict(flip=flip, key_black_name="J" if flip else "D", key_white_name="D" if flip else "J")
