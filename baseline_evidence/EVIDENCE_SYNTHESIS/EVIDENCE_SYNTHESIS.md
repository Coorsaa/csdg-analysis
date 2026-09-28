# Systematic local-fidelity and cross-study evidence synthesis

- Synthesis ID: `major-review-final-20260908g_evidence_synthesis`
- Frozen SHILD parent: `major-review-final-20260908g_shild`
- Frozen PISA parent: `major-review-final-20260908g_pisa2018_reading`
- Parent-derived release state: `NOT_ELIGIBLE`
- SHILD parent: structural `PASS`, release `NOT_ELIGIBLE`
- PISA parent: structural `PASS`, release `NOT_ELIGIBLE`
- Review-delivery eligibility blocker: Parent release eligibility not satisfied: SHILD=NOT_ELIGIBLE (manuscript readiness is FALSE | analyst verification not signed); PISA=NOT_ELIGIBLE (manuscript readiness is FALSE | analyst verification not signed)
- Predictive models retrained: `FALSE`
- Canonical results overwritten: `FALSE`
- Prespecified SHILD cases: `300`
- Cases below the historical weighted-R-squared reference of 0.800: `255` (`85.0%`)
- Median case-median weighted R-squared: `0.7169`
- Interquartile range: `0.6606–0.7642`
- Median case-level Monte Carlo SD across seeds: `0.1520`
- Cases crossing the threshold across individual seeds: `281` (`93.7%`)
- Median case-median weighted RMSE in prediction-score deciles 1 and 10: `0.0123` and `0.0599`
- Secondary method variants evaluated on the common 100-case grid: `4`
- Crossed-seed weighted-R-squared marginal SD medians (perturbation; cross-fit): `0.1173`; `0.0262`
- Crossed-seed weighted-R-squared interaction RMS median: `0.0498`

## Interpretation boundary

The systematic G4 result describes fidelity to frozen fold-specific XGBoost predictions in the package's declared local neighborhoods. It does not establish causal explanation, actionable individual recourse, joint empirical support, model-fit uncertainty, decision readiness, or out-of-setting validity.
Weighted R-squared is relative to the model-prediction variance inside each synthetic neighborhood and is not an absolute-error measure. Prediction-score-region patterns must therefore be read together with weighted RMSE. The crossed-seed decomposition describes only the declared computational grid.

The PISA boundary and cross-study model-sensitivity artifacts are deterministic derivations from the manifest-verified aggregate tables. No PISA model or explanation was recomputed.

