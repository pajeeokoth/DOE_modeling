# DOE Modeling — RSM / GP / Deep Learning Ensemble Pipeline

Systematic benchmarking of three complementary modeling approaches — classical Response Surface Methodology (RSM), Gaussian Process (GP) surrogates, and H2O Deep Learning (ANN) — across 46 Design of Experiments datasets spanning five design paradigms (BBD, CCD, DSD, Taguchi, CCFD). Results are statistically compared using non-parametric hypothesis tests.

---

## Project Structure

```
DOE_modeling/
├── model_comparison.ipynb            # Python statistical benchmark notebook
├── README.md
├── Research2026-002 data/
│   ├── utils.R                       # Core functions (scaling, GP, ANN, metrics, orchestrator)
│   ├── bbd_ensemble.R                # Box-Behnken Design — 7 datasets
│   ├── ccd_ensemble.R                # Central Composite Design — 19 datasets
│   ├── dsd_ensemble.R                # Definitive Screening Design — 8 datasets
│   ├── taguchi_ensemble.R            # Taguchi Orthogonal Array — 8 datasets
│   ├── ccfd_ensemble.R               # Mixed factorial designs — 4 datasets
│   ├── [50+ data files]              # Train/test splits (.txt, .csv)
│   └── [Output: Metrics.xlsx]        # Model results (generated)
├── bbd_papers/      (15 PDFs)        # BBD source references
├── ccd_papers/      (15 PDFs)        # CCD source references
├── dsd_papers/      (7 PDFs)         # DSD source references
├── taguchi_papers/  (14 PDFs)        # Taguchi source references
├── ccfd_papers/     (2 PDFs)         # CCFD source references
├── pbd_papers/      (1 PDF)          # Plackett-Burman reference
└── FD/              (2 PDFs)         # Factorial design references
```

## Workflow

1. **Data ingestion** — Each ensemble script loads DOE datasets with train/test splits.
2. **Scaling** — RSM uses coded variables \[-1, 0, 1\]; GP and ANN use standardized variables (μ=0, σ=1).
3. **Model fitting** (via `doe_meta_model()` in `utils.R`):
   - **RSM**: `rsm::rsm()` with user-defined second-order formula
   - **GP**: `gp_master_smallDOE()` — hetGP with automatic kernel selection (Gaussian, Matérn 5/2, Matérn 3/2) via LOOCV
   - **ANN**: `run_DOE_ANN_full()` — H2O deep learning with tiered hidden-layer grid search sized for small DOEs
   - **Ensemble**: Inverse-RMSE weighted blend of RSM + GP + ANN
4. **Metrics** — MASE and MAPE computed per model per response.
5. **Export** — Results written to Excel ("Model Metrics" and "GP Kernel Search" sheets).
6. **Statistical benchmarking** (Python notebook) — Shapiro-Wilk, Levene, Wilcoxon, ANOVA, Kruskal-Wallis, Dunn's post-hoc tests.

## Key Functions (`utils.R`)

| Function | Purpose |
|---|---|
| `scale_design()` | Code factors to \[-1, 0, 1\] or standardize to μ=0, σ=1 |
| `inverse_scale()` | Reverse coding/standardization to original scale |
| `mase()` / `mape()` | Mean Absolute Scaled Error / Mean Absolute Percentage Error |
| `gp_master_smallDOE()` | Fit hetGP with kernel grid search and LOOCV-based selection |
| `run_DOE_ANN_full()` | H2O deep learning with architecture grid search (tiered by dataset size) |
| `plot_h2o_dl_topology()` | Visualize ANN topology via DiagrammeR |
| `save_model_metrics()` | Append model metrics to an Excel workbook |
| `doe_meta_model()` | Master orchestrator — fits all models for all responses |

## ANN Architecture Grid

Hidden-layer candidates are tiered by dataset size to prevent overfitting on small DOEs:

| Tier | Condition | Architectures |
|---|---|---|
| 1 (always) | All datasets | \[3\], \[5\], \[8\], \[5,3\], \[8,5\] |
| 2 | n ≥ 20 | \[10\], \[10,5\], \[15,10\] |
| 3 | n ≥ 30 and ≤ 6 factors | \[20,10\], \[20,10,5\] |

## GP Kernel Selection

`gp_master_smallDOE()` fits Gaussian, Matérn 5/2, and Matérn 3/2 kernels and selects by LOOCV MASE (with LOOCV MAPE as tiebreaker). When LOOCV fails for all kernels, selection falls back to negative log-likelihood.

## Dependencies

### R
`tidyverse`, `readxl`, `readr`, `rsm`, `hetGP`, `h2o`, `DiagrammeR`, `openxlsx`, `Metrics`

### Python (notebook)
`pandas`, `numpy`, `scipy`, `statsmodels`, `scikit-learn`, `scikit-posthocs`, `matplotlib`, `xlsxwriter`

## Usage

1. Start an H2O cluster (launched automatically by `utils.R` if needed).
2. Source the shared utilities:
   ```r
   source("Research2026-002 data/utils.R")
   ```
3. Run an ensemble script:
   ```r
   source("Research2026-002 data/ccd_ensemble.R")
   ```
4. Open `model_comparison.ipynb` to run statistical tests on the generated `Metrics.xlsx`.
