# Analysis tests

Run all standalone analysis tests from the repository root:

```sh
for test in analysis/tests/*.R; do Rscript "$test"; done
Rscript analysis/targeted/test_targeted_extensions.R
```

| Test | Checks |
|---|---|
| `test_decimal_labels.R` | APA-style decimal labels in the figure renderers |
| `test_diagnostics.R` | Study-specific diagnostic helpers |
| `test_evidence_synthesis.R` | Evidence synthesis on synthetic inputs |
| `test_output.R`, `test_output_audit.R` | Output writing and the privacy audit of shareable outputs |
| `test_shild_preprocessing.R` | SHILD recoding rules on synthetic rows |
| `test_targeted_controller.R` | Modes and safeguards of `analysis/targeted/run.R` |
| `test_study_plots.R` | Study plot helpers |
| `targeted/test_targeted_extensions.R` | Targeted SHILD comparisons on synthetic inputs; analytical values and stored results (80 rows, both model forms) of the controlled examples |

The tests use synthetic data or public aggregates and do not require restricted data.
