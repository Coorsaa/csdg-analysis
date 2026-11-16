# Reproducible CSDG manuscript analyses

This directory contains the complete SHILD and PISA analyses for the Claim-Scoped Diagnostic Gates manuscript.
The scripts are ordered as ordinary statistical R workflows and can be executed either as complete files or section
by section in an interactive R session.

## Analysis structure

- `shild/analysis.R` performs the SHILD data preparation, resampling, model fitting, diagnostic gates,
  sensitivity analyses, reporting, and figure construction.
- `pisa/analysis.R` performs the corresponding PISA analysis for all ten reading plausible values,
  including the observed-country leave-one-country-out stress test.
- `evidence_synthesis.R` performs the prespecified 300-case systematic SHILD local-fidelity audit, separates
  perturbation and surrogate-partition variation on a complete crossed secondary seed grid, and derives the
  cross-study claim-boundary summaries from verified SHILD and PISA study pairs.
- `build_delivery.R` assembles manifest-checked sanitized outputs, analysis code, figure metadata, verification
  records, a ZIP archive, and its SHA-256 checksum.
- `shared/` contains only reusable workflow, diagnostic, output, and study-support functions.
- `protocol.yml` records all publication settings, estimands, thresholds, and interpretation boundaries.
- `preprocessing_review.md` records the coauthors' verification of the analytic cuts and preprocessing (2026-09-09).

The main study scripts contain the complete sequence of substantive choices. Learned preprocessing is fitted inside
training folds. Performance, calibration, permutation importance, local fidelity, subgroup estimates, and setting
stress tests use held-out observations. Fold, plausible-value, model, and country variation is described according to
the semantics recorded in each source table; it is not treated as independent-sample inferential uncertainty.

One-dimensional ALE is the exception only where the protocol requests model-refit stability evidence: SHILD uses
respondent resampling, and PISA resamples schools independently within country. These pointwise percentile intervals
refit the complete fixed learner pipeline; they are not simultaneous bands or model-selection uncertainty.

## Claim cards in the study scripts

The study scripts configure the package's diagnostic audit with claim cards of the original runs (for example
`claim_version = "C0"`). These cards steer which diagnostics the audit runs; they are not the claims assessed in the
article. The application claims, their criteria, and their assessments are stated in the manuscript (Tables 5 and 6); the online
supplement gives the records with criteria and observations in Tables S6 and S7 and the prespecified confirmatory claims in Table S22.

## Runtime controls

The two study analyses use five environment variables:

- `CSDG_ANALYSIS_SEED`: non-negative integer; publication default `20260714`.
- `CSDG_QUICK`: `0` for publication settings or `1` for a reduced nonpublication smoke test.
- `CSDG_THREADS`: positive integer for supported learner backends; default `1`.
- `CSDG_ALE_BOOTSTRAP_WORKERS`: positive integer number of forked model-refit bootstrap workers for publication
  runs; default `4`. Quick runs remain serial. Use `1` on Windows.
- `CSDG_RUN_ID`: portable run identifier used for output and compatible checkpoints.

The evidence synthesis additionally accepts:

- `CSDG_SHILD_PARENT`: a SHILD study-pair directory, private directory, or run directory with a matching upload.
- `CSDG_PISA_PARENT`: a PISA study-pair directory, private directory, or run directory with a matching upload.
- `CSDG_G4_WORKERS`: number of local-fidelity workers, capped at six.

When the parent variables are omitted, the defaults pinned in `protocol.yml` are used.

## Install and validate

The analyses use the companion package `mlr3autoiml` in the version recorded for each step (see the repository
README). The study scripts and `analysis/preflight.R` require the tagged version 0.1.1:

```sh
Rscript -e 'remotes::install_github("coorsaa/mlr3autoiml@v0.1.1")'
Rscript analysis/preflight.R
```

Run the standalone analysis tests from a clean workspace:

```sh
for test in analysis/tests/*.R; do Rscript "$test"; done
Rscript analysis/build_delivery.R --self-test
```

## Execute the analyses

Use quick mode only to validate the computational path:

```sh
CSDG_QUICK=1 CSDG_RUN_ID=smoke-shild Rscript analysis/shild/analysis.R
CSDG_QUICK=1 CSDG_RUN_ID=smoke-pisa Rscript analysis/pisa/analysis.R
```

Run the complete analysis workflow with publication settings and one new run identifier:

```sh
CSDG_QUICK=0 CSDG_ANALYSIS_SEED=20260714 CSDG_RUN_ID=publication-current \
  Rscript analysis/run_all.R
```

The complete workflow runs the preflight, both studies, the evidence synthesis, and the delivery audit. It writes
the final private/upload pairs below `paper_artifacts/current/` and creates the shareable archive at
`paper_artifacts/current/delivery/CSDG_publication_results_upload.zip`. It stops before computation if
`paper_artifacts/current/` already exists, so a completed result is never overwritten. The archive records
`ELIGIBLE` only after all release gates, including the recorded preprocessing verification and structural output
audit, pass; otherwise it lists the unmet conditions.

The study scripts can also be run separately when an individual analysis needs inspection:

```sh
CSDG_QUICK=0 CSDG_ANALYSIS_SEED=20260714 CSDG_RUN_ID=publication-shild \
  Rscript analysis/shild/analysis.R
CSDG_QUICK=0 CSDG_ANALYSIS_SEED=20260714 CSDG_RUN_ID=publication-pisa \
  Rscript analysis/pisa/analysis.R
```

After verifying separately produced private/upload pairs, run the systematic audit and evidence synthesis:

```sh
CSDG_SHILD_PARENT=paper_artifacts/<shild-pair-or-private-run> \
CSDG_PISA_PARENT=paper_artifacts/<pisa-pair-or-private-run> \
CSDG_RUN_ID=publication-evidence \
  Rscript analysis/evidence_synthesis.R
```

### PISA country-level summaries

Two checkpointed focused runners extend the frozen PISA analysis without rerunning unrelated diagnostics. They read
protected model or prediction objects in place and write aggregate-only tables; the protected objects, sampled row
identifiers, and row-level predictions are never copied into the public outputs.

Run 50 deterministic PV1 training-size-matched removals per observed country with:

```sh
CSDG_FOCAL_OOF_RDS=<private-all-PV-OOF-object> \
CSDG_MATCHED_OUTPUT_DIR=analysis/pisa/results/pisa_matched_control_50 \
CSDG_MATCHED_REPETITIONS=50 CSDG_MATCHED_WORKERS=10 \
  Rscript analysis/pisa/run_pv1_matched_controls.R
```

Run 500 school-within-country bootstrap replicates for each stored LOCO country/plausible-value prediction set with:

```sh
CSDG_LOCO_PREDICTIONS_RDS=<private-LOCO-predictions-object> \
CSDG_FOCAL_OOF_RDS=<private-all-PV-OOF-object> \
CSDG_LOCO_CALIBRATION_OUTPUT_DIR=analysis/pisa/results/pisa_loco_country_calibration_500 \
CSDG_LOCO_CALIBRATION_REPETITIONS=500 CSDG_LOCO_CALIBRATION_WORKERS=10 \
  Rscript analysis/pisa/run_loco_country_calibration_bootstrap.R
```

The country-calibration intervals condition on the stored LOCO predictions. The pipeline is not refitted, and the
intervals omit model-training, plausible-value-generation, survey-design, new-country, and future-cycle uncertainty.

The public aggregates of both runners are stored in `analysis/pisa/results/` and summarized in `final_docs/`.

Each script is deliberately written as a sequence of named objects and analysis sections, not as one enclosing
function. To inspect intermediate objects, open the file in RStudio or another R editor and execute it section by
section from the repository root. Execute the setup and run-contract sections once, then evaluate each complete
top-level expression in order. The run logger remains active through the core analysis and is stopped before the
transactional release finalization. After an unhandled error, start a fresh run contract before producing publication
artifacts.

## Figures

Reusable table-backed graphics are produced by the package plotting API; the analysis scripts retain only
study-specific graphics. Every component is saved as a separate PDF and PNG file without titles or captions, which
belong in LaTeX. Figure metadata are written to each run's `upload/provenance/` folder. The repository README maps the
figures of the article to their generators.

## Assembling a results archive

The private result contains fitted objects and restricted diagnostics. The corresponding `_upload` directory is the
sanitized shareable result. Do not assemble an archive from selected screenshots or from a private directory.

Before assembling an archive:

1. verify each private manifest and sanitized upload manifest;
2. inspect all warnings, source tables, plots, and interpretation boundaries;
3. retain the dated responsible-co-author verification and run linkage in `preprocessing_review.md`;
4. run the output audit and package tests; and
5. build the archive from the complete sanitized directories.

The archive builder verifies its result after construction and writes a ZIP archive plus a SHA-256 sidecar. Share
the complete verified archive so that tables, figures, figure metadata, code, provenance, and verification records
remain together.
