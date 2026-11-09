"""Hypothesis profiles: `ci` (the default) is deterministic and has no deadline, as this machine and CI's are loaded and
pairings take long; `dev` explores with fresh randomness (`HYPOTHESIS_PROFILE=dev`)."""

import os

from hypothesis import HealthCheck, settings

settings.register_profile(
    "ci",
    deadline=None,
    derandomize=True,
    database=None,
    max_examples=200,
    suppress_health_check=[HealthCheck.too_slow, HealthCheck.data_too_large],
)
settings.register_profile("dev", deadline=None, max_examples=1000)
settings.load_profile(os.environ.get("HYPOTHESIS_PROFILE", "ci"))
