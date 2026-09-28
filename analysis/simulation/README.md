# Operating characteristics of the decision criteria

`criteria_simulation.R` is a small simulation of how often three decision criteria declare a claim about
permutation feature importance (PFI) "met" when the learner-level property is false, at its boundary, or true with
increasing margin. It writes only aggregate tables and one figure to `results/`.

```
Rscript analysis/simulation/criteria_simulation.R --workers=3        # full run (500 replications)
Rscript analysis/simulation/criteria_simulation.R --quick            # smoke test, writes results_quick/
```

Options: `--workers` (forked processes, default 1), `--root` (repository root, default `.`), `--output-dir`. The
full run takes about three minutes with three workers, most of it for the reference sample of 10^6 draws and the
figure. Results are reproducible for any number of workers because every replication sets its own seed
(`20260928` plus an offset).

## Design

- **Data.** `y = b_A x_A + b_B x_B + b_C x_C + sum_j b_Nj x_Nj + e` with eight standard normal predictors, pairwise
  correlation 0.3, five noise predictors with effects 0.10, 0.08, 0.06, 0.04, and 0.02, and `e ~ N(0, 1)`.
- **Learner.** Ordinary least squares with an intercept, chosen for speed.
- **PFI.** Held-out increase in mean squared error after marginal permutation of one predictor in the test fold,
  averaged over R = 10 permutations per fold and predictor.
- **Resampling.** K-fold cross-validation with K in {5, 10}; sample size n in {1,000, 5,000}; 500 replications per
  condition.
- **Claims.**
  - *Dominance:* "the learner relies mainly on A and B", that is, the PFI of A and of B is each at least twice the
    PFI of every other predictor (12 comparisons). With `b_A = 0.5` and `b_B = 0.625`, the binding comparison is
    A against C; the margin is set through `b_C`.
  - *Ordering:* "A has larger PFI than B", with `b_A = 0.5` and `b_C = 0.3`; the margin is set through `b_B`.
- **Margin.** The true PFI of A divided by the value it must exceed: `PFI(A) / (2 PFI(C))` for dominance and
  `PFI(A) / PFI(B)` for ordering. The grid runs from 0.5 to 3 in steps of 0.025. The property is false below 1,
  at its boundary at 1, and true above 1.
- **Criteria**, each applied to every comparison of a claim (a claim is met only if all its comparisons pass):
  1. `every_fold`: the difference, for example `PFI(A) - 2 PFI(C)`, is at least 0 in every one of the K folds.
  2. `interval`: the lower bound of the one-sided 95% corrected resampled t interval (Nadeau and Bengio, 2003) for
     the mean difference over folds exceeds 0. Variance `(1/K + n_test/n_train) s^2`, where `s^2` is the variance of
     the K fold-level differences and `n_test/n_train = 1/(K - 1)`; t quantile with K - 1 degrees of freedom.
  3. `mean_only`: the mean difference over folds exceeds 0.
  4. `uncorrected_interval` (reference only, not in the figure): as 2 with variance `s^2 / K`.

### True values

For marginal permutation of a standardized predictor, the population PFI of the OLS model is `2 b_j^2`, so the
margins of the grid are exact by construction (column `margin`). They are reproduced with OLS fitted on 10^6 draws
and evaluated on 10^6 independent draws (columns ending in `_population`); the reproduced margins deviate from the
design values by at most 1.6% (at the boundary: 0.990 for dominance and 0.996 for ordering). The expected PFI of
OLS fitted on the training size of a fold, `2 b_j^2 + 2 sigma_e^2 ((Sigma^-1)_jj - 1) / (n_train - p - 2)`, differs
from the population value by at most 0.0007 on the difference scale, below 0.8% of the standard deviation of the
estimate (columns `difference_training_size` and `margin_training_size`).

### Computation

For a linear model, permuting `x_j` changes each prediction by `beta_j * (x_j[perm] - x_j)`, so the held-out PFI is
`beta_j^2 * mean(delta^2) - 2 * beta_j * mean(r * delta)` with held-out residuals `r`. Because OLS is equivariant,
the fitted slopes are `b + d` with `d` the OLS fit of the errors on the predictors, and the residuals do not depend on
`b`. One fit per fold therefore serves both claims and all margins (common random numbers across the grid; the same
data are also used for K = 5 and K = 10). The script checks this shortcut against PFI computed by refitting on `y`
and predicting with the permuted column (agreement to 1e-10).

## Output files (`results/`)

`criteria_rates.csv`, one row per claim, margin, n, K, and criterion:

| Column | Meaning |
|---|---|
| `claim` | `dominance` or `ordering` |
| `margin` | true learner-level margin (design value; 1 is the boundary) |
| `margin_population` | the same margin from the reference sample of 10^6 draws |
| `status` | `false` (margin < 1), `boundary` (margin = 1), or `true` (margin > 1) |
| `n`, `folds` | sample size and number of folds K |
| `criterion` | `every_fold`, `interval`, `mean_only`, or `uncorrected_interval` |
| `met` | proportion of replications in which the claim was met |
| `mc_se` | Monte Carlo standard error of `met`, `sqrt(met (1 - met) / replications)` |
| `replications` | number of replications (500) |

`criteria_estimates.csv`, one row per claim, margin, n, and K, for the binding difference
(`PFI(A) - 2 PFI(C)` or `PFI(A) - PFI(B)`):

| Column | Meaning |
|---|---|
| `difference` | true difference (population PFI) |
| `difference_population` | true difference from the reference sample of 10^6 draws |
| `difference_training_size`, `margin_training_size` | expected difference and margin for OLS fitted on `n_train = n (K - 1) / K` |
| `mean_estimate` | mean over replications of the mean difference over folds |
| `bias` | `mean_estimate - difference` |
| `empirical_sd` | standard deviation over replications of the mean difference over folds |
| `corrected_se`, `uncorrected_se` | root mean of the corrected and uncorrected variance estimates |
| `variance_ratio_corrected`, `variance_ratio_uncorrected` | mean variance estimate divided by `empirical_sd^2` (1 is unbiased) |

`criteria_truth.csv`, one row per claim and margin: coefficients `b_A`, `b_B`, `b_C`; population PFI `pfi_A`,
`pfi_B`, `pfi_C` (`2 b_j^2`) and their reproductions `pfi_A_population`, `pfi_B_population`, `pfi_C_population`;
`difference`, `difference_population`, and `margin_population`.

`criteria_summary.csv`, one row per claim, criterion, n, and K: `met_at_boundary` and `mc_se_at_boundary`;
`max_met_at_or_below_boundary`; `met_at_margin_1.25`, `met_at_margin_1.5`, `met_at_margin_2`;
`margin_for_80_percent` (smallest grid margin with at least 80% "met", empty if none); and the variance ratios of
`criteria_estimates.csv` at the boundary.

`criteria_simulation.pdf` and `.png`: proportion of replications with the claim met against the true margin, by
criterion (line type), claim (rows), and n and K (columns). Horizontal reference lines mark .05 and .50; the
vertical line marks the boundary.

## Main results

Proportion "met" at the boundary (Monte Carlo standard error at most 0.022):

| Claim | n | K | Every fold | Corrected interval | Mean only | Uncorrected interval |
|---|---|---|---|---|---|---|
| Dominance | 1,000 | 5 | .232 | .194 | .500 | .276 |
| Dominance | 1,000 | 10 | .062 | .182 | .498 | .278 |
| Dominance | 5,000 | 5 | .240 | .202 | .550 | .290 |
| Dominance | 5,000 | 10 | .074 | .210 | .552 | .280 |
| Ordering | 1,000 | 5 | .174 | .132 | .488 | .238 |
| Ordering | 1,000 | 10 | .048 | .154 | .498 | .232 |
| Ordering | 5,000 | 5 | .204 | .166 | .478 | .254 |
| Ordering | 5,000 | 10 | .040 | .142 | .508 | .240 |

- The every-fold rule becomes stricter as K grows. At margin 1.5 and n = 1,000 it was met in 67% (K = 5) versus
  38% (K = 10) of replications for dominance and 77% versus 39% for ordering; the margin needed for 80% "met" rose
  from 1.675 to 2.425 (dominance) and from 1.525 to 2.175 (ordering).
- The mean-only rule is met in about half of the replications at the boundary (48% to 55%).
- The corrected interval gains power with n: the margin needed for 80% "met" fell from 1.70 to 1.75 (dominance)
  and 1.60 to 1.625 (ordering) at n = 1,000 to 1.275 and 1.225 at n = 5,000. It does not keep the nominal 5% rate
  at the boundary in this design (13% to 21%). The corrected variance was 23% to 36% of the empirical variance of the
  mean difference at the boundary (22% to 47% across the grid), so the corrected standard error would need to be
  about 1.7 to 2.1 times larger. The fold-level differences are more strongly correlated than the correction's
  assumption (correlation `n_test / n`) implies: the fold models share most of their training data, and every
  observation is a test point in one fold and a training point in all others.
