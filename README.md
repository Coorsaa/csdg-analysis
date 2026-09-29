# CSDG analyses

Analysis code, public aggregate results, figure sources, and the computational supplement of the article
*From Interpretation Claims to Diagnostic Evidence: A Practical Framework for Interpretable Machine Learning in
Psychological Research*. The repository reproduces the SHILD and PISA applications, the controlled examples, and the
simulation of the decision criteria. The workflow itself is implemented in the R package
[`mlr3autoiml`](https://github.com/coorsaa/mlr3autoiml).

All tables and figures in the repository are aggregates. Data files, fitted models, predictions, and case-level
results are not included (see [Data](#data)).

## Repository layout

| Path | Content |
|---|---|
| `analysis/shild/analysis.R`, `analysis/pisa/analysis.R`, `analysis/run_all.R` | Primary SHILD and PISA analyses (data preparation, resampling, models, diagnostics); `run_all.R` runs both studies and the evidence synthesis. |
| `analysis/evidence_synthesis.R` | 300-case systematic SHILD local-fidelity evaluation and cross-study summaries. |
| `analysis/shild/claim_extensions.R` | SHILD claim records: learner-level global importance from the stored permutation feature importance (PFI) (`--part=importance`), leave-one-covariate-out (LOCO) importance with refits (`--part=loco`), nested check of the predictor selection (`--part=selection`), repeated local surrogates of the 300 evaluated cases (`--part=local`), and aggregate tables from the frozen primary outputs (`--part=export`: local fidelity, item missingness, fold-level PFI per run). Results and column definitions: `analysis/shild/results/claim_extensions/`. |
| `analysis/pisa/ses_reanalysis.R` | PISA re-analysis of the reliance on socioeconomic indices across four learners, 10 plausible values, and refit-based designs. Stages: `exploratory` (the equal-country sample of the primary analysis), `confirmatory` (1,500 new students per country), and `tuned_xgboost` (run with `--learners=xgboost_tuned`: XGBoost with the median configuration of the nested tuning check, on the exploratory sample). Results: `analysis/pisa/results/ses_reanalysis/<stage>/`. |
| `analysis/pisa/confirmatory_claims.md` | Claims and criteria of the PISA confirmatory stage, committed before that stage was run (see [Prespecification](#prespecification-of-the-pisa-confirmatory-stage)). |
| `analysis/pisa/evaluate_confirmatory.R` | Applies the prespecified criteria to the confirmatory stage (and, for comparison, to the exploratory stage); writes `analysis/pisa/results/ses_reanalysis/confirmatory_*.csv`. |
| `analysis/pisa/render_ses_figure.R` | Figure 2 of the article (panels a and b) from the aggregates of `ses_reanalysis.R`. |
| `analysis/pisa/tuning_check.R` | Nested tuning of XGBoost and random forest (PV1) compared with the fixed learner settings; results in `analysis/pisa/results/ses_reanalysis/tuning_check/`. |
| `analysis/pisa/preprocess_PISA_v2.R`, `analysis/pisa/pisa2018_reading_compactv2_manifest.csv` | Extraction of the 32-country analytic extract from the public OECD PISA 2018 files, and the variable table of the extract used in the article. |
| `analysis/pisa/run_pv1_matched_controls.R`, `analysis/pisa/run_loco_country_calibration_bootstrap.R`, `analysis/pisa/results/` | PISA matched-control and country-calibration summaries of the primary run (retained; not reported in the current article). |
| `analysis/targeted/` | Probability versus log-odds surrogates and number of perturbed points in SHILD, the three-model PISA importance summary across plausible values, and the controlled examples (`controlled_examples.R`, Supplement G); controller `run.R`; public results in `analysis/targeted/results/`. |
| `analysis/simulation/criteria_simulation.R` | Simulation of the operating characteristics of the decision criteria (Supplement M); design and outputs in `analysis/simulation/README.md`. |
| `analysis/sensitivity/` | Sensitivity of the model comparisons (Supplement I), Figure S2b, numerical checks of statements in the article (`article_checks.R`), and the numbers the article derives from fold-level aggregates (`article_derivations.R`). |
| `analysis/render_*.R` | Figure rendering from stored aggregates. |
| `analysis/shared/`, `analysis/tests/` | Shared helpers and tests. |
| `baseline_evidence/`, `final_docs/` | Public aggregate result tables and figures of the primary study runs. |
| `supplement/` | Computational supplement (`computational_supplement.pdf`, figures C1–C28), its components, source tables, and builder. |

## Package versions

The table lists the tagged `mlr3autoiml` release to install for each group of scripts.

| Scripts | `mlr3autoiml` |
|---|---|
| Primary SHILD and PISA results (`baseline_evidence/`, `final_docs/`) | produced with [`v0.1.0`](https://github.com/coorsaa/mlr3autoiml/releases/tag/v0.1.0) (session information of the three runs in `baseline_evidence/provenance/`) |
| Study scripts as provided (`analysis/run_all.R`, `analysis/shild/analysis.R`, `analysis/pisa/analysis.R`, `analysis/evidence_synthesis.R`; checked by `analysis/preflight.R`) | [`v0.1.1`](https://github.com/coorsaa/mlr3autoiml/releases/tag/v0.1.1) |
| Targeted comparisons, PISA runners, and figure rendering (`analysis/targeted/` except `controlled_examples.R`, `analysis/pisa/run_*.R`, `analysis/render_*.R`) | [`v0.1.4`](https://github.com/coorsaa/mlr3autoiml/releases/tag/v0.1.4) |
| Sensitivity summaries and article checks (`analysis/sensitivity/`) | [`v0.1.5`](https://github.com/coorsaa/mlr3autoiml/releases/tag/v0.1.5) |
| PISA re-analysis and confirmatory stage (`analysis/pisa/ses_reanalysis.R`, `evaluate_confirmatory.R`, `render_ses_figure.R`, `tuning_check.R`), SHILD claim extensions (`analysis/shild/claim_extensions.R`), criteria simulation (`analysis/simulation/criteria_simulation.R`), and controlled examples (`analysis/targeted/controlled_examples.R`) | [`v0.1.7`](https://github.com/coorsaa/mlr3autoiml/releases/tag/v0.1.7) |

The results of the last group were produced with the development version 0.1.5.9000 (recorded in their session
information), which is identical to releases 0.1.6 and 0.1.7 in the functions these scripts use. `evaluate_confirmatory.R`,
`render_ses_figure.R` and `criteria_simulation.R` do not load `mlr3autoiml` (they need `data.table` and `ggplot2`), and
`analysis/sensitivity/article_derivations.R` needs only `data.table`;
`controlled_examples.R` also uses `jsonlite` and is usually run through `analysis/targeted/run.R`, which loads
`mlr3autoiml`. Running the scripts rewrites tracked PDF figures with identical content but different bytes; restore
them with `git checkout -- '*.pdf'` if needed.

```sh
Rscript -e 'remotes::install_github("coorsaa/mlr3autoiml@v0.1.7")'   # replace the tag for other script groups
```

The analyses were run with R 4.5.1. Further dependencies are listed in `DESCRIPTION` and loaded at the top of each
script (for example `mlr3learners`, `mlr3tuning`, `xgboost`, `ranger`, and `glmnet`); the PISA extraction needs
`haven`, `dplyr`, `stringr`, `readr`, `tibble`, and `vctrs`.

## Data

- **PISA 2018** is public. The OECD PISA 2018 database is available at
  <https://www.oecd.org/en/data/datasets/pisa-2018-database.html>. `analysis/pisa/preprocess_PISA_v2.R`, the
  coauthors' extraction script, reads the student and school questionnaire files (SPSS format,
  `CY07_MSU_STU_QQQ.sav` and `CY07_MSU_SCH_QQQ.sav`, placed in `data/PISA/raw/`) and writes the 32-country extract
  (260,641 students) and its variable table to `data/PISA/`:

  ```sh
  Rscript analysis/pisa/preprocess_PISA_v2.R
  ```

  The PISA analyses read `data/PISA/pisa2018_reading_compactv2.csv` and
  `data/PISA/pisa2018_reading_compactv2_manifest.csv` and draw the equal-country sample (1,500 students per country)
  themselves. The SHA-256 checksums of the extract used in the article are listed in the header of the extraction
  script. `analysis/pisa/analysis.R` additionally expects a copy of the extraction script and the OECD codebook,
  student questionnaire, and scale manual in `data/PISA/` (see the file names in the script) and records their
  checksums.
- **SHILD**: the analyses use the SHILD 2012 survey data, which are publicly available under CC0 on Dryad
  (<https://doi.org/10.5061/dryad.qd2nj>) and were used by Khan et al. (2024). [to be confirmed by the authors: the
  analyzed file corresponds to this release] The scripts expect the data file and its dictionary at `data/SHILD/`
  (`data/SHILD/archive/Danish_SHILD_draft_data.csv` and `data/SHILD/archive/Danish_SHILD_dictionnary.csv`).
- Fitted models, predictions, and case-level results of the study runs are written to `paper_artifacts/`. Case-level
  tables of `analysis/shild/claim_extensions.R` and the held-out predictions of `analysis/pisa/ses_reanalysis.R` are
  written only to the directory given by `CSDG_PRIVATE_DIR`. The protected SHILD extensions write to a directory
  outside the repository:
  `Rscript analysis/targeted/run.R --mode=protected_extension --authorized-workspace=<dir> --private-output=<dir outside the repository>`.

`data/`, `paper_artifacts/`, `private/`, and all `.rds` files are excluded by `.gitignore` and must never be
committed.

## Reproducing the article

Run all commands from the repository root.

### From the stored aggregates (no data needed)

```sh
Rscript analysis/pisa/render_ses_figure.R exploratory          # Figure 2a and 2b
Rscript analysis/pisa/evaluate_confirmatory.R                   # PISA confirmatory assessment
Rscript analysis/sensitivity/run_sensitivities.R                # Supplement I tables
Rscript analysis/sensitivity/render_paired_difference_figure.R  # Figure S2b
Rscript analysis/sensitivity/article_checks.R                   # numerical checks of statements in the article
Rscript analysis/sensitivity/article_derivations.R              # numbers derived from fold-level aggregates
Rscript analysis/targeted/run.R --mode=aggregate                # three-model PISA summary (Table S29), Figures S1 and S3
Rscript analysis/targeted/run.R --mode=synthetic                # controlled examples (Supplement G)
Rscript analysis/simulation/criteria_simulation.R --workers=3   # criteria simulation (Supplement M)
python3 supplement/build_supplement.py --verify                 # computational supplement: checksums and privacy checks
python3 supplement/assemble_publication.py --output build/supplement   # computational supplement PDF (XeLaTeX)
```

One installation of `mlr3autoiml` v0.1.7 suffices for all commands in this section. They were checked with the
development version 0.1.5.9000 and reproduce the stored CSV and JSON outputs exactly. `run.R --mode=aggregate` renders
three figures with leading zeros in the axis labels (`shild_common_probability_weighted_r2.pdf`,
`shild_convergence_weighted_r2.pdf`, and `shild_g4_systematic_local_audit__relative_fidelity_by_prediction.pdf`); the
stored versions carry the APA labels written by `analysis/render_decimal_figures.R` (see below).
`article_checks.R` also re-derives the fold-level summary of the paired SHILD comparison when `CSDG_PRIVATE_DIR`
points to the private case files; otherwise it keeps the stored aggregate
`analysis/sensitivity/results/shild_scale_paired_by_fold.csv`.

### With the public PISA data

```sh
Rscript analysis/pisa/ses_reanalysis.R --stage=exploratory --workers=12
Rscript analysis/pisa/ses_reanalysis.R --stage=confirmatory --workers=12
Rscript analysis/pisa/ses_reanalysis.R --stage=exploratory --learners=xgboost_tuned --designs=full \
  --output-dir=analysis/pisa/results/ses_reanalysis/tuned_xgboost --workers=12
Rscript analysis/pisa/tuning_check.R --workers=5
Rscript analysis/pisa/evaluate_confirmatory.R
Rscript analysis/pisa/render_ses_figure.R exploratory
```

Add `--quick` to `ses_reanalysis.R` or `tuning_check.R` for a smoke test (written to a `_quick` folder). The primary
PISA analysis is run with `analysis/pisa/analysis.R` (see `analysis/README.md`).

### With the SHILD data

```sh
Rscript analysis/shild/claim_extensions.R --part=importance
Rscript analysis/shild/claim_extensions.R --part=loco --workers=6
Rscript analysis/shild/claim_extensions.R --part=selection --workers=6
Rscript analysis/shild/claim_extensions.R --part=local --workers=6
Rscript analysis/shild/claim_extensions.R --part=export
```

The first four parts check the folds against the stored fold assignment of the primary SHILD run in
`paper_artifacts/current/`; `importance` and `local` also read its stored PFI and local-fidelity results. `export`
reads only the frozen outputs of the primary SHILD run and the evidence synthesis and writes aggregates of at least 10
cases or respondents per row. The primary SHILD analysis and the evidence synthesis are run with `analysis/run_all.R`
(see `analysis/README.md`).

### Article elements, scripts, and outputs

| Article element | Script | Output |
|---|---|---|
| Figure 1 (workflow; panel b applies it to statement (a)) | values from `analysis/shild/claim_extensions.R --part=importance` | `analysis/shild/results/claim_extensions/importance_{item_means,fold_ratios}.csv` |
| Numerical example (main text; claim records in Table S25) | exact values in the text; Monte Carlo check by `analysis/targeted/controlled_examples.R` (`run.R --mode=synthetic`) | `analysis/targeted/results/controlled/controlled_pfi_results.csv` |
| Figure 2a and 2b | `analysis/pisa/ses_reanalysis.R --stage=exploratory`, then `analysis/pisa/render_ses_figure.R exploratory` | `analysis/pisa/results/ses_reanalysis/exploratory/pisa_ses_figure_{a,b}.pdf`; plotted values in `pisa_ses_figure_source.csv` |
| Table 5 (SHILD claim records) | `analysis/shild/analysis.R` (predictive performance), `analysis/evidence_synthesis.R` (fidelity), `analysis/targeted/run.R --mode=protected_extension` (scale comparison), `analysis/shild/claim_extensions.R` (global importance, LOCO, selection check, repeated local surrogates) | `baseline_evidence/STUDY_RESULTS/SHILD/tables/`, `baseline_evidence/EVIDENCE_SYNTHESIS/`, `analysis/targeted/results/shild/`, `analysis/shild/results/claim_extensions/` |
| Tables 6 and 7 (PISA claim records, exploratory and confirmatory stages) | `analysis/pisa/ses_reanalysis.R` (both stages and `tuned_xgboost`), `analysis/pisa/evaluate_confirmatory.R`, `analysis/pisa/tuning_check.R` | `analysis/pisa/results/ses_reanalysis/{exploratory,confirmatory,tuned_xgboost,tuning_check}/`, `analysis/pisa/results/ses_reanalysis/confirmatory_{assessment,admission,detail_c2..c5}.csv` |
| Numbers derived from fold-level aggregates in the text and in Tables S22, S28, and S30 (admission differences, top three, five, and ten, block ratios, Rubin's total variance, C2 margins, C3 fold ratios, LOCO fold shares, Monte Carlo and Holm thresholds) | `analysis/sensitivity/article_derivations.R` | `analysis/sensitivity/results/article_derivations.{csv,txt}` |
| Tables S16, S20, and S21 (LOCO, fold-level PFI, local surrogates by decile) | `analysis/shild/claim_extensions.R --part=loco`, `--part=importance`, `--part=local` | `analysis/shild/results/claim_extensions/` |
| SHILD aggregates behind fidelity, missingness, and fold-level statements (Supplements F and H, Table S12) | `analysis/shild/claim_extensions.R --part=export` (needs the frozen primary outputs) | `analysis/shild/results/claim_extensions/export_*.csv` |
| Figure S1 (fidelity by decile) | `analysis/render_retained_figures.R`; APA labels of panel a by `analysis/render_decimal_figures.R` | `final_docs/EVIDENCE_FIGURES/figures/shild_g4_systematic_local_audit__{relative_fidelity_by_prediction,absolute_fidelity_by_prediction}.pdf`; plotted values in `supplement/plot_data/shild_g4_decile_plot_summary.csv` |
| Figure S2a | `analysis/evidence_synthesis.R`; rendered by `analysis/render_decimal_figures.R` (needs the private case table) | `final_docs/EVIDENCE_FIGURES/figures/shild_g4_systematic_local_audit__apparent_vs_cross_fitted.pdf`; summary in `analysis/shild/results/claim_extensions/export_local_fidelity_summary.csv` |
| Figure S2b | `analysis/sensitivity/render_paired_difference_figure.R` | `analysis/sensitivity/results/shild_common_probability_paired_rmse_difference.pdf` |
| Figure S3 (panels a to d) | `analysis/targeted/render_targeted_figures.R`; APA labels of panels b and c by `analysis/render_decimal_figures.R` | `analysis/targeted/results/shild/shild_{common_probability_weighted_rmse,common_probability_weighted_r2,convergence_weighted_r2,convergence_weighted_rmse}.pdf` |
| Figure S4 and Supplement M (simulation of the decision criteria) | `analysis/simulation/criteria_simulation.R` | `analysis/simulation/results/criteria_simulation.pdf`, `criteria_summary.csv` |
| Supplement G (controlled examples) | `analysis/targeted/run.R --mode=synthetic` | `analysis/targeted/results/controlled/` |
| Supplement I (sensitivity of the model comparisons; the three-model PISA importance across plausible values behind Table S29) | `analysis/sensitivity/run_sensitivities.R`; `analysis/targeted/pisa_synthesis.R` (`run.R --mode=aggregate`) | `analysis/sensitivity/results/`, `analysis/targeted/results/pisa/` |
| Computational supplement, Figures C1–C28 | `supplement/assemble_publication.py` (components: `analysis/render_supplement_figures.R` and the primary runs) | `supplement/computational_supplement.pdf` |
| Retained displays and summaries not reported in the current article (PISA matched controls and country calibration, locality curve, performance-explanation sensitivity) | `analysis/render_retained_figures.R`; `analysis/pisa/run_pv1_matched_controls.R` and `analysis/pisa/run_loco_country_calibration_bootstrap.R` (need private model objects) | `final_docs/figures/pisa_pv1_loco_*.pdf`, `final_docs/EVIDENCE_FIGURES/figures/shild_g4_systematic_local_audit__locality_curve.pdf`, `final_docs/EVIDENCE_FIGURES/figures/performance_explanation_sensitivity__*.pdf`, `analysis/pisa/results/`, `final_docs/PISA_*.csv` |

`analysis/render_decimal_figures.R` writes APA-labeled versions of selected figures (for example ".75" instead of
"0.75") into a manuscript folder:
`Rscript analysis/render_decimal_figures.R <manuscript_dir> <reference_dir> [private_case_table]`, where
`<reference_dir>` receives the unlabeled reference renderings.

## Prespecification of the PISA confirmatory stage

The claims and criteria of the PISA confirmatory stage are stated in `analysis/pisa/confirmatory_claims.md`. The file
was committed in commit `893b053` (2026-09-28, 17:50 UTC), together with the exploratory results, before the
confirmatory stage was run; its results and their assessment were committed afterward in commit `e5a554f`. The
record can be checked with:

```sh
git log --format='%h %ad %s' --date=iso -- analysis/pisa/confirmatory_claims.md analysis/pisa/results/ses_reanalysis/confirmatory
```

## Tests

```sh
for f in analysis/tests/*.R; do Rscript "$f"; done
Rscript analysis/targeted/test_targeted_extensions.R
```

The tests use synthetic data or public aggregates and do not require restricted data.

## License

The code is licensed under the GNU Lesser General Public License, version 3 (LGPL-3; see `LICENSE`), like the
companion package `mlr3autoiml`. The aggregate result tables and figures (CSV, PDF, and PNG files, including the
computational supplement) are released under the Creative Commons Attribution 4.0 International license (CC BY 4.0;
see `LICENSE-CC-BY-4.0.txt`). The PISA and SHILD data are not part of the repository and remain under the terms of
their providers.
