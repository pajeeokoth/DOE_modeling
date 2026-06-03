# Changelog

## 2026-06-03
- Added RMSE and CROSS_VALIDATION_TYPE to exported model metrics and standardized column ordering in Excel output.
- Improved ANN CV reporting by tracking effective folds (after small-data safety checks) instead of relying only on requested folds.
- Updated ANN training to use seeded random fold assignment (instead of modulo) for less order bias.
- Added optional repeated-seed ANN CV search (`cv_repeats`, `cv_seed_stride`) with tiny-dataset guards:
  - force repeats to 1 when CV is disabled,
  - cap repeats at 2 when `n < 20`.
- Increased ANN default search budget in both trainer and orchestrator:
  - `max_runtime_secs`: 120 -> 240,
  - `max_models_per_arch`: 10 -> 20.
