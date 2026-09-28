# CSDG analyses

Analyses, public aggregate results, figure sources, and the computational supplement for the article
*From Interpretation Claims to Diagnostic Evidence: A Practical Framework for Interpretable Machine Learning in
Psychological Research*. The workflow itself is implemented in the R package
[`mlr3autoiml`](https://github.com/coorsaa/mlr3autoiml).

## Contents

| Folder | Content |
|---|---|
| `analysis/shild/`, `analysis/pisa/` | Complete SHILD and PISA study analyses (data preparation, resampling, models, diagnostics). |
| `analysis/evidence_synthesis.R` | 300-case systematic SHILD local-fidelity evaluation and cross-study summaries. |
| `analysis/targeted/` | Targeted SHILD comparisons (probability vs. log-odds surrogate, number of perturbed points), PISA model and plausible-value synthesis, controlled examples (Supplement G), and their public results in `results/`. |
| `analysis/pisa/run_pv1_matched_controls.R`, `analysis/pisa/run_loco_country_calibration_bootstrap.R`, `analysis/pisa/results/` | PISA matched-control and country-calibration summaries (Supplement D.8, F.2). |
| `analysis/sensitivity/` | Sensitivity of the model comparisons (Supplement I; admission allowances, tie tolerances, and aggregation rules, all specified after the original results were known), Figure 2b, and numerical checks of the article (`article_checks.R`). |
| `analysis/shared/`, `analysis/tests/` | Shared helpers and tests. |
| `final_docs/`, `baseline_evidence/` | Public aggregate result tables of the study runs. |
| `supplement/` | Builder and content of the computational supplement. |

## Package versions

| Step | `mlr3autoiml` |
|---|---|
| Primary SHILD and PISA results of the article | produced with [`v0.1.0`](https://github.com/coorsaa/mlr3autoiml/releases/tag/v0.1.0) (recorded in the runs' session information) |
| Study scripts as provided (`analysis/run_all.R`, `analysis/shild/`, `analysis/pisa/`; checked by `analysis/preflight.R`) | [`v0.1.1`](https://github.com/coorsaa/mlr3autoiml/releases/tag/v0.1.1) |
| Targeted comparisons, PISA runners, figure rendering (`analysis/targeted/`, `analysis/render_*.R`) | [`v0.1.4`](https://github.com/coorsaa/mlr3autoiml/releases/tag/v0.1.4) |
| Sensitivity summaries and article checks (`analysis/sensitivity/`) | [`v0.1.5`](https://github.com/coorsaa/mlr3autoiml/releases/tag/v0.1.5) |

```sh
Rscript -e 'remotes::install_github("coorsaa/mlr3autoiml@v0.1.5")'
```

## Data

- **PISA 2018** is public from the OECD. The 32-country extract used here is expected at `data/PISA/`.
- **SHILD** is restricted third-party data (VIVE). Authorized users place the files at `data/SHILD/`.
- Fitted models, predictions, and case-level results of the study runs are written to `paper_artifacts/`. The
  protected SHILD extensions write to a directory outside the repository:
  `Rscript analysis/targeted/run.R --mode=protected_extension --authorized-workspace=<dir> --private-output=<dir outside the repository>`.

`data/`, `paper_artifacts/`, `private/`, and all `.rds` files are excluded by `.gitignore` and must never be
committed. All tables and figures in this repository are aggregates.

## Reproducing the article's numbers without restricted data

Run from the repository root:

```sh
Rscript analysis/sensitivity/run_sensitivities.R          # Supplement I tables
Rscript analysis/sensitivity/render_paired_difference_figure.R   # Figure 2b
Rscript analysis/sensitivity/article_checks.R             # numerical checks of statements in the article
Rscript analysis/targeted/run.R --mode=synthetic       # controlled examples (Supplement G)
```

`article_checks.R` also re-derives the fold-level summary of the paired SHILD comparison when `CSDG_PRIVATE_DIR`
points to the private case files; otherwise it keeps the stored aggregate
`analysis/sensitivity/results/shild_scale_paired_by_fold.csv`.

## Figures in the article

The final figure files used in the manuscript are included at the locations below. `analysis/render_decimal_figures.R`
writes the APA-labeled versions (for example ".75" instead of "0.75") into a manuscript folder:
`Rscript analysis/render_decimal_figures.R <manuscript_dir> <qa_dir> [private_case_table]`.

| Figure | File | Generator |
|---|---|---|
| Main Fig. 2a | `final_docs/EVIDENCE_FIGURES/figures/shild_g4_systematic_local_audit__apparent_vs_cross_fitted.pdf` | `analysis/render_decimal_figures.R` (needs the private case table) |
| Main Fig. 2b | `analysis/sensitivity/results/shild_common_probability_paired_rmse_difference.pdf` | `analysis/sensitivity/render_paired_difference_figure.R` |
| Main Fig. 3 | `analysis/targeted/results/pisa/pisa_model_pv_content.pdf` | `analysis/targeted/pisa_synthesis.R` |
| Suppl. Fig. S1 | `final_docs/figures/pisa_pv1_loco_{size_matched_exclusion_penalty,training_size_contrast}.pdf` | `analysis/render_retained_figures.R` |
| Suppl. Fig. S2 | `final_docs/EVIDENCE_FIGURES/figures/shild_g4_systematic_local_audit__{relative_fidelity_by_prediction,locality_curve}.pdf` | `analysis/render_decimal_figures.R`, `analysis/render_retained_figures.R` |
| Suppl. Fig. S3 | `analysis/targeted/results/shild/shild_{common_probability_weighted_rmse,common_probability_weighted_r2,convergence_weighted_r2}.pdf` | `analysis/targeted/render_targeted_figures.R`, `analysis/render_decimal_figures.R` |
| Suppl. Fig. S4 | `final_docs/EVIDENCE_FIGURES/figures/shild_g4_systematic_local_audit__absolute_fidelity_by_prediction.pdf`, `analysis/targeted/results/shild/shild_convergence_weighted_rmse.pdf` | `analysis/render_retained_figures.R`, `analysis/targeted/render_targeted_figures.R` |
| Suppl. Fig. S5 | `final_docs/EVIDENCE_FIGURES/figures/performance_explanation_sensitivity__{shild,pisa}__{practical_pair_reversals,top_five_overlap}.pdf` | `analysis/render_retained_figures.R` |

The computational supplement is assembled by `supplement/assemble_publication.py`.
