# Operating characteristics of the decision criteria

`criteria_simulation.R` is a small simulation of how often four decision criteria declare a claim about
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
  - *Dominance* (the relies-mainly claim of the article): "the learner relies mainly on A and B", that is, the PFI of A and of B is each at least twice the
    PFI of every other predictor (12 comparisons). With `b_A = 0.5` and `b_B = 0.625`, the binding comparison is
    A against C; the margin is set through `b_C`.
  - *Ordering* (the order claim of the article): "A has larger PFI than B", with `b_A = 0.5` and `b_C = 0.3`; the margin is set through `b_B`.
- **Margin.** The true PFI of A divided by the value it must exceed: `PFI(A) / (2 PFI(C))` for dominance and
  `PFI(A) / PFI(B)` for ordering. The grid runs from 0.5 to 3 in steps of 0.025. The property is false below 1,
  at its boundary at 1, and true above 1.
- **Criteria**, each applied to every comparison of a claim (a claim is met only if all its comparisons pass):
  1. `every_fold`: the difference, for example `PFI(A) - 2 PFI(C)`, is at least 0 in every one of the K folds.
  2. `interval`: the lower bound of the one-sided 95% corrected resampled t interval (Nadeau and Bengio, 2003) for
     the mean difference over folds exceeds 0. Variance `(1/K + n_test/n_train) s^2`, where `s^2` is the variance of
     the K fold-level differences and `n_test/n_train = 1/(K - 1)`; t quantile with K - 1 degrees of freedom.
  3. `interval_and_share`: criterion 2 holds and the difference is at least 0 in at least 90% of the folds (5 of 5 or
     9 of 10).
  4. `mean_only`: the mean difference over folds exceeds 0.
  5. `uncorrected_interval` (reference only, not in the figure): as 2 with variance `s^2 / K`.
  6. `interval_two_sided` and `interval_two_sided_and_share` (not in the figure): as 2 and 3 with the two-sided 95%
     interval, whose lower limit is a one-sided 97.5% bound; the applications and the package use this interval.

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

| Claim | n | K | Every fold | Corrected interval | Two-sided corrected interval | Mean only | Uncorrected interval |
|---|---|---|---|---|---|---|---|
| Dominance | 1,000 | 5 | .232 | .194 | .132 | .500 | .276 |
| Dominance | 1,000 | 10 | .062 | .182 | .122 | .498 | .278 |
| Dominance | 5,000 | 5 | .240 | .202 | .138 | .550 | .290 |
| Dominance | 5,000 | 10 | .074 | .210 | .174 | .552 | .280 |
| Ordering | 1,000 | 5 | .174 | .132 | .094 | .488 | .238 |
| Ordering | 1,000 | 10 | .048 | .154 | .098 | .498 | .232 |
| Ordering | 5,000 | 5 | .204 | .166 | .096 | .478 | .254 |
| Ordering | 5,000 | 10 | .040 | .142 | .106 | .508 | .240 |

- The every-fold rule becomes stricter as K grows. At margin 1.5 and n = 1,000 it was met in 67% (K = 5) versus
  38% (K = 10) of replications for dominance and 77% versus 39% for ordering; the margin needed for 80% "met" rose
  from 1.675 to 2.425 (dominance) and from 1.525 to 2.175 (ordering).
- The mean-only rule is met in about half of the replications at the boundary (48% to 55%).
- The corrected interval gains power with n: the margin needed for 80% "met" fell from 1.70 to 1.75 (dominance)
  and 1.60 to 1.625 (ordering) at n = 1,000 to 1.275 and 1.225 at n = 5,000. It does not keep the nominal 5% rate
  at the boundary in this design (13% to 21%). The two-sided 95% version that the applications and `mlr3autoiml` use
  (a one-sided 97.5% bound) was met in 9% to 17% at the boundary, and in 9% to 15% together with 90% of folds
  (criteria `interval_two_sided` and `interval_two_sided_and_share`). The corrected variance was 23% to 36% of the empirical variance of the
  mean difference at the boundary (22% to 47% across the grid), so the corrected standard error would need to be
  about 1.7 to 2.1 times larger. The fold-level differences are more strongly correlated than the correction's
  assumption (correlation `n_test / n`) implies: the fold models share most of their training data, and every
  observation is a test point in one fold and a training point in all others.

# Simulation of the workflow

`workflow_simulation.R` simulates the workflow of the article for claims about PFI: an analyst inspects held-out PFI,
states the claim the results suggest, and assesses it with the default criteria of `mlr3autoiml` 0.3.0, first in the
data that suggested it (exploratory) and then in an independent sample with the same fixed criteria. It compares the
share of false claims with a standard practice that reports a claim whenever its point estimates satisfy it and with
each component of the default rule alone. Results (aggregates only) are in `results/workflow/`.

```
Rscript analysis/simulation/workflow_simulation.R --workers=6     # full run, about 15 minutes with 6 workers
Rscript analysis/simulation/workflow_simulation.R --mode=verify   # comparison with mlr3autoiml 0.3.0 (on the library path)
Rscript analysis/simulation/workflow_simulation.R --mode=figure   # redraws the figure from the result tables
```

## Design

- **Data.** Binary outcome and 10 predictors. `Z1, Z2, Z3 ~ N(0, 1)`; `X1 = sqrt(r1) Z1 + sqrt(1 - r1) e1` and
  `X2 = sqrt(r2) Z1 + sqrt(1 - r2) e2` (two items of the same latent variable); `X3 = sqrt(.8) Z2 + sqrt(.2) e3`;
  `X4 = Z3`; `X5 ~ N(0, 1)`; `X6 = .6 X3 + .8 e6` (no effect); `X7`–`X10` noise.
  `logit P(Y = 1) = b0 + bA Z1 + bB Z2 + 0.3 Z3 + 0.3 (X5^2 - 1)`, prevalence .20.
  Scenario S1 ("clear"): `r1 = .9, r2 = .6, bA = 1.2, bB = 1.0`; scenario S2 ("ambiguous"): `r1 = r2 = .8,
  bA = bB = 0.9` (X1 and X2 share the signal of Z1 and are exchangeable).
- **Learner.** XGBoost (`binary:logistic`, 100 rounds, depth 3, eta 0.1); log loss with probabilities clipped to
  `[1e-15, 1 - 1e-15]`; model without predictors: training-fold prevalence.
- **Explanation.** Stratified 10-fold cross-validation; held-out marginal PFI with 10 permutations per predictor and
  fold.
- **Truth.** The learner's PFI and improvement for training sets of size `0.9 n`, averaged over 100 fits, each
  evaluated on an independent sample of 20,000 (the PFI of the exchangeable X1 and X2 in S2 is set to their average).
  In S1, the true top two are X1 and X3 (at n = 1,000, X3 has 4.97 times the PFI of the third predictor); in S2, the
  second and third largest PFI are equal, so every relies-mainly claim is false.
- **Claims.** With top1 and top2 the predictors with the largest fold-averaged PFI: R, "the learner relies mainly on
  top1 and top2" (factor 2); O, "the learner relies more on top1 than on top2". Both are worded "under marginal
  permutation", because the truth is the learner's marginal PFI.
- **Standard practice.** Reports R if the fold-averaged PFI of top2 is at least twice that of the third predictor;
  always reports O. As a comparator for the test in new data, standard practice in both samples also requires the point
  estimates of the independent sample to satisfy the claim.
- **CSDG.** The default checks of `csdg_check()` and the decision rule of `csdg_assess()` of `mlr3autoiml` 0.3.0:
  content with the corrected resampled t interval and the smallest relevant difference (1% of the improvement) for
  every comparison the claim requires (R: each named predictor against each of the 8 other predictors, 16
  comparisons, each with its own interval and Monte Carlo threshold), the minimum importance (1%), selection stability
  in at least 9 of 10 folds with the selection repeated, and the Monte Carlo rule; `csdg_confirm()` for the test in
  new data.
- **Components.** The same analyses are also assessed with each component of the defaults alone: content (the interval
  rule), content and minimum importance (G2), and selection stability (G5, the share of folds); measures
  `content_only_*`, `g2_only_*`, `stability_only_*` and `workflow_components.csv`. Measurement and data (G0b) are judged supported (the variables
  are recorded as simulated), as version 0.3.0 requires before an assessment can be met.
- **Conditions.** n in {300, 1,000, 5,000}; 500 replications per scenario and n, each with an exploratory and an
  independent confirmation sample.
- **Truth of a claim.** R: each named predictor has at least twice the true PFI of every other predictor and at least
  1% of the true improvement. O: false if the true difference is at most 0, true if it is at least 1% of the true
  improvement (no claim fell between).

The script reimplements the package rules for speed. `--mode=verify` analyzes six replications with the package
functions (`csdg_fit()`, `csdg_importance()`, `claim_relies_mainly()`, `claim_order()`, `csdg_check()`,
`csdg_assess()`, `csdg_confirm()`) of version 0.3.0; the statuses agreed in 240 of 240 comparisons, the fold PFI to
6e-16, and the rule functions in 24,000 of 24,000 random inputs that reach every status, including inputs with an
unnamed predictor whose mean PFI is smaller than that of another but whose fold PFI varies strongly; for 36 of these
inputs, the content status differs from that of the comparison with the largest other mean only
(`results/workflow/workflow_verification_*.csv`).

## Main results

Shares with Monte Carlo standard errors are in `workflow_operating_characteristics.csv` and `workflow_summary.csv`.

| Claim | Scenario | n | Standard practice: false among reported | CSDG exploratory: false among met | Met in both stages: false | CSDG power (exploratory; both stages) |
|---|---|---|---|---|---|---|
| R | S1 | 300 | .125 (33/263) | 0/2 | none | .006; 0 |
| R | S1 | 1,000 | 0/437 | 0/98 | 0/15 | .198; .030 |
| R | S1 | 5,000 | 0/499 | 0/461 | 0/419 | .922; .838 |
| R | S2 | 300 | 156/156 | none met | none | no true claim |
| R | S2 | 1,000 | 153/153 | none met | none | no true claim |
| R | S2 | 5,000 | 36/36 | 2/2 | none | no true claim |
| O | S1 | 300 | .322 | .224 (11/49) | 1/5 | .112; .012 |
| O | S1 | 1,000 | .226 | .107 (13/122) | 0/25 | .282; .065 |
| O | S1 | 5,000 | .064 | .004 (1/265) | 0/129 | .564; .276 |
| O | S2 | 300 | .296 | .105 (2/19) | 0/1 | .048; .003 |
| O | S2 | 1,000 | .102 | .018 (3/170) | 0/107 | .372; .238 |
| O | S2 | 5,000 | 0/500 | 0/490 | 0/487 | .980; .974 |

Conditional on the truth (measures `csdg_met_among_false`, `standard_reported_among_false`,
`standard_both_among_false`, `both_met_among_false`, `csdg_power`, `standard_power`, `standard_both_power`,
`both_power`): of the 1,642 false relies-mainly claims, CSDG met 2 (.001) and standard practice reported 378 (.230);
of the 505 false order claims, CSDG met 30 (.059; .115 in S1 at n = 1,000) and standard practice reported all. With
the point estimates required to hold in the independent sample, 42 (.026) and 133 (.263) false claims passed, versus
0 and 1 met by CSDG in both samples. Of the true claims, CSDG met 561 of 1,358 (.413) and 1,085 of 2,495 (.435),
standard practice reported 1,166 (.859) and 2,495 (1), and 982 (.723) and 2,176 (.872) held in both samples by their
point estimates. These conditional rates do not depend on the share of true claims among those stated; the shares
below do.

Over all conditions, standard practice reported 1,544 relies-mainly claims, 378 of them false (.245), and 3,000 order
claims, 505 false (.168). CSDG met 563 relies-mainly claims in the exploratory data, 2 false (.004), and 1,115 order
claims, 30 false (.027); of the claims met in both stages, 0 of 434 and 1 of 754 were false. Of the exploratory met
claims, 434 of 563 (R) and 754 of 1,115 (O) were met again in new data. Within a scenario, this share changes little
with the exploratory margin of an order claim (`workflow_replication_by_margin.csv`; at n = 1,000: S1 .20 and .21,
S2 .58 and .69 for margins of 30–100 and 100–300); pooled over scenarios it seems to rise only because the scenarios
differ in both. The two false relies-mainly claims met at n = 5,000 relied
on one of the two exchangeable items in all fold models, which share most of their training data; both were not met in
new data. At n = 300, the learner barely improves on (S1) or is worse than (S2) the model without predictors, so the
minimum importance has no scale in 34% and 55% of the analyses and the claim is unresolved. Without the wording
"under marginal permutation", the package's dependence screen (the R^2 of X1 or X2 on the other predictors is about
.54 in S1 and .64 in S2, at least the default .50) leaves almost every claim that names X1 or X2 unresolved
(`default_wording_*` measures).

**Components of the defaults** (`workflow_components.csv`; shares of the false and of the true claims, pooled over
the six conditions, exploratory sample; in parentheses, declared in both samples):

| Rule | R false (1,642) | R true (1,358) | O false (505) | O true (2,495) |
|---|---|---|---|---|
| Point estimates | .230 (.026) | .859 (.723) | 1 (.263) | 1 (.872) |
| Content (interval rule) | .005 (0) | .457 (.352) | .075 (.008) | .469 (.325) |
| Content and minimum importance (G2) | .005 (0) | .457 (.352) | .073 (.006) | .454 (.318) |
| Selection stability (G5) | .001 (0) | .423 (.333) | .095 (.006) | .503 (.352) |
| All defaults | .001 (0) | .413 (.320) | .059 (.002) | .435 (.302) |

Each component alone removes most false claims that point estimates declare; combining them declares fewer false order
claims than either component alone, at a small loss of power. For relies-mainly claims, the share of folds does nearly
all the work of the combination. The minimum importance matters mainly at n = 300.

**Every comparison.** The content of claim R requires every comparison of a named with an unnamed predictor. Compared
with the comparison against the unnamed predictor with the largest mean PFI only (the shortcut of an earlier version of
this script and of `csdg_check()`), this changed none of the 3,000 exploratory assessments of R and one of the 3,000
assessments in the confirmation samples (S1, n = 1,000, replication 416: met with the shortcut, unresolved with every
comparison), of a claim that was unresolved in its exploratory sample, so no reported rate changed (measures
`explore_changed_by_all_comparisons` and `confirm_changed_by_all_comparisons`; `workflow_comparisons.csv`). In 568 of
the 3,000 exploratory analyses of R, the comparison with the smallest lower limit was not the one with the largest
other mean (`workflow_comparisons.csv`, by condition).

## Files

| File | Content |
|---|---|
| `workflow_operating_characteristics.csv` | One row per scenario (S1, S2, pooled), n, claim, and measure, with numerator, denominator, estimate, Monte Carlo standard error, and Wilson interval |
| `workflow_summary.csv` | Key measures in wide form |
| `workflow_comparisons.csv` | Claim R, by scenario and n: analyses, analyses whose decisive comparison (smallest lower limit) is not the one with the largest other mean, assessments that differ between every comparison and the comparison with the largest other mean only (exploratory and new data), and the replications concerned |
| `workflow_components.csv` | Shares of false and of true claims declared by point estimates, by each component of the defaults alone, and by all defaults, in the exploratory sample and in both samples, by scenario (S1, S2, pooled), n, and claim, with numerators and denominators |
| `workflow_property_status.csv` | Shares of supported, open, and contradicted for content, minimum importance, and stability, and of the assessments, by scenario, n, claim, and stage |
| `workflow_replication_by_margin.csv` | Shares met in new data by bin of the exploratory margin |
| `workflow_truth.csv`, `workflow_truth_summary.csv` | True PFI per predictor, improvement, losses, and calibration ratios |
| `workflow_claim_pairs.csv` | Named pairs with their truth and true margin |
| `workflow_figure_data.csv` | Plotted values of the figure (share of false claims and of true claims declared by each procedure, per scenario, n, and claim) with counts, Wilson intervals, and whether each value is drawn |
| `workflow_verification_end_to_end.csv`, `workflow_verification_pfi.csv`, `workflow_verification_rules.csv` | Comparison with `mlr3autoiml` 0.3.0 |
| `workflow_runtime.csv`, `sessionInfo.txt`, `sessionInfo_verify.txt` | Runtime and session information |
| `workflow_simulation.pdf`, `workflow_simulation.png` | Figure of the article |
