# SHILD claims checked with mlr3autoiml 0.3.0

Written by `analysis/shild/package_check.R`, which rederives two SHILD claim assessments of the article with the
analysis-first interface of `mlr3autoiml` release 0.3.0:

- statement (a), "XGBoost relies mainly on self-rated health and depressed mood" (`claim_relies_mainly(imp, k = 2)`);
- the stronger claim, "Self-rated health is the most important item," read as a claim about the learner: "XGBoost
  relies more on self-rated health than on depressed mood" (`claim_order(imp, "A2", "D2")`).

The task and the learner are those of `analysis/shild/analysis.R`: 16,971 respondents, the 27 items, the outcome
`life_satisfaction` (positive class `discontent`), training-fold median and mode imputation and one-hot encoding
inside the learner pipeline, and XGBoost with nrounds 200, max_depth 3, eta 0.05, subsample 1, and colsample_bytree 1.
`csdg_fit()` fits XGBoost and the model without predictors on the 10 retained folds of the primary analysis (custom
resampling), `csdg_importance()` computes held-out permutation feature importance (PFI; increase in log loss,
20 permutations per item and fold, seed 20260714), and `csdg_check()` with its default criteria and `csdg_assess()`
check and assess both claims. G0b is judged with
`csdg_judge("G0b", supported(), note = "Single survey items coded as in the codebook; claims name items, not constructs.")`.
All files are aggregates; no respondent-level values, fitted models, or predictions are written.

## Running

```sh
Rscript -e 'remotes::install_github("coorsaa/mlr3autoiml@v0.3.0")'
Rscript analysis/shild/package_check.R
```

Run from the repository root. The script reads the SHILD data in `data/SHILD/archive/` and, from the primary SHILD run
in `paper_artifacts/current/shild/private/tables/`, the fold assignment (checked against the folds), the fold scores
(compared with the held-out losses), and the PFI permutations of self-rated health and depressed mood. It takes about
12 minutes on one core. The results were produced with `mlr3autoiml` 0.3.0 (`sessionInfo.txt`).

## Files

| File | Content |
|---|---|
| `package_check.txt` | Printed fits, PFI, claims, checks, and assessments; held-out log loss per fold and mean PFI next to the primary run; ratios and differences per fold; the stronger claim on other permutations, with 500 permutations per fold, and with the held-out bootstrap; comparison with the article |
| `statement_a_comparisons.csv` | Content (G2) of statement (a): each of the 50 comparisons of self-rated health and depressed mood with another item (PFI of the named item minus twice that of the other), with corrected 95% interval, Monte Carlo threshold, and status, as `csdg_check()` computes them (`details$content$xgboost$comparisons`) |
| `check_statement_a.csv`, `check_stronger_claim.csv` | Check tables (`as.data.table()` of the `csdg_check()` results): `gate`, `gate_name`, `check`, `learner`, `role`, `property`, `criterion`, `observation`, `relevance`, `status`, `summary` |
| `importance_xgboost.csv` | Per item (`feature_group`): mean PFI over the 10 folds (`mean_importance`) with its corrected resampled 95% interval (`lower`, `upper`), standard deviation over folds, mean PFI as a multiple of the improvement over the model without predictors, `rank`, and `positive_folds` |
| `stronger_claim_permutations.csv` | The stronger claim under the rule of `csdg_check()` for each set of permutations: the stored permutations of the primary run, those of `csdg_importance()`, and 20 further sets of 20 permutations per fold of A2 and D2 (`csdg_fold_pfi()`); mean PFI of both items, mean difference with corrected interval, Monte Carlo threshold of the mean, standard deviation of the fold differences, number of folds in which A2 is larger, failing and undecided folds, statuses of content, minimum importance, and stability, and assessment |
| `stronger_claim_precise_folds.csv` | Per fold, with 500 permutations per item and fold (`csdg_fold_pfi()`) and with the 22 sets of 20 pooled (440 per fold): PFI of A2 and D2, their `difference`, its Monte Carlo standard error `mc_se` (square root of s_A^2/R + s_D^2/R), `threshold` (t(.975, R − 1) × `mc_se`), `holm_threshold` for the largest reversal over 10 folds (t(1 − .025/10, R − 1) × `mc_se`, as in Supplement D and in the rule of `csdg_check()` when every fold is required), and the `class` of the fold under the default criteria |
| `stronger_claim_precise_summary.csv` | For both estimates, under the default criteria and neighboring criteria varied one at a time (smallest relevant difference 0, 0.5%, and 2% of the improvement; 8 and 10 of 10 folds): the columns of `stronger_claim_permutations.csv` with the smallest relevant difference and the number of folds required |
| `stronger_claim_fold_bootstrap.csv` | Held-out sampling uncertainty per fold, with the fold model held fixed: 200 bootstrap resamples of the fold's held-out respondents, stratified by outcome, with the PFI difference computed from 50 permutations per item on each resample; held-out fold size and number of dissatisfied respondents, difference of the 500-permutation run, bootstrap mean, bootstrap standard error with and without the Monte Carlo error of 50 permutations, 95% percentile interval (`lower`, `upper`), `z` (difference of the 500-permutation run divided by the bootstrap standard error), and whether the absolute value of `z` exceeds 1.96 and 2.81 (Holm for 10 folds) |
| `comparison_with_article.csv` | Numbers and assessments reported in the article next to those of the script |
| `statement_a_csdg_assessment/`, `stronger_claim_csdg_assessment/` | Output of `csdg_export_assessment()`: claim card, criteria, check table, evidence records, gate plan, assessment, and claim log (the derivation, check, and assessment of both claims with the default criteria) |
| `sessionInfo.txt` | R session information |

## Results

The fold models are those of the article: in every fold, the held-out log loss of XGBoost and of the model without
predictors equals the stored fold score of the primary run (largest difference 4.7e-16).

| Quantity | Article | mlr3autoiml 0.3.0 |
|---|---|---|
| Held-out log loss of XGBoost and of the model without predictors | 0.122 and 0.229 | 0.122 and 0.229 |
| Statement (a): mean PFI of self-rated health and depressed mood; largest other item (M8) | 0.046 and 0.036; 0.0055 | 0.046 and 0.036; 0.0056 |
| Statement (a): corrected intervals of each item's excess over twice every other item, weakest of 25 comparisons each (M8 for both) | [0.027, 0.043] and [0.021, 0.030] | [0.028, 0.042] and [0.020, 0.029] |
| Statement (a): smallest ratio to the largest other item of a fold | 5.0 and 4.0 | 5.1 and 4.1 |
| Statement (a): folds that reproduce the claim | 10 of 10 | 10 of 10 |
| Statement (a): assessment | met (exploratory) | met (exploratory) |
| Stronger claim, 20 permutations per fold: mean difference A2 − D2 with corrected 95% interval | 0.0097 [0.0005, 0.0188] (stored draw) | 0.0105 [0.0016, 0.0195] (draw of `csdg_importance()`) |
| Stronger claim, 20 permutations per fold: content (G2) | depends on the draw | supported in this draw |
| Stronger claim: stability (G5) | 9 of 10 folds, supported | 9 of 10 folds, supported |
| Stronger claim, further sets of 20 permutations: claim met | 17 of 21 | 17 of 21 |
| Stronger claim, 500 permutations per fold: mean difference A2 − D2 with corrected 95% interval | 0.0103 [0.0014, 0.0192] | 0.0103 [0.0014, 0.0192] |
| Stronger claim, 500 permutations per fold: assessment | met (exploratory) | met (exploratory) |
| Stronger claim, every fitted model: reversal in fold 9 | 0.0024, 0.32 bootstrap standard errors (unresolved) | 0.0024, 0.32 bootstrap standard errors |

Statement (a) agrees with the article. All 50 comparisons of the two items with the other 25 items are supported; the
weakest is the comparison with satisfaction with current finances (M8) for both items. The PFI values differ from those of the article by Monte Carlo error only: the
package draws its own permutations, whereas the article pooled the two stored runs of 10 permutations of the primary
run, which used the same PFI code (`csdg_fold_pfi()` is unchanged since release 0.1.0). The mean PFI differs most for
self-rated health (by 0.0005), the order of the leading items is the same, and the largest fold-level difference is
3.5 times its Monte Carlo standard error.

With 20 permutations per fold, the assessment of the stronger claim depends on the permutations, not on the folds, the
preprocessing, the learner settings, or the rule: the rule of `csdg_check()` applied to the stored permutations of the
primary run gives 0.0097 [0.0005, 0.0188] and unresolved, the result of the initial analysis. The article therefore
decides the claim with 500 permutations per fold (below). The claim lies at the boundary of its criterion,
which requires the lower bound of the corrected interval to reach the smallest relevant difference of 0.0011 (1% of
the improvement of 0.106). With the permutations of `csdg_importance()`, the lower bound is 0.0016, so content (G2) is
supported and the claim is met. In fold 9, depressed mood leads by 0.0008, within Monte Carlo error, whereas it leads
by 0.0030, beyond Monte Carlo error, with the article's permutations; stability holds in 9 of 10 folds either way. In
20 further sets of 20 permutations per fold on the same fold models, the mean difference ranged from 0.0097 to 0.0108
and the lower bound from 0.0004 to 0.0022; the claim was met in 16 and unresolved in 4. The article's permutations
gave the smallest mean difference of all 22 sets.

With 500 permutations per item and fold, the Monte Carlo standard error is about 0.0002 for each fold's difference and
0.00007 for the mean difference, so the result no longer depends on the draw. The mean difference is 0.0103 [0.0014,
0.0192]; the lower bound exceeds the smallest relevant difference of 0.0011, self-rated health exceeds depressed mood
in 9 of 10 folds, and the stronger claim is met (exploratory). In fold 9, depressed mood leads by 0.0024 (Monte Carlo
standard error 0.0002), beyond the per-fold threshold of 0.0004, the Holm-adjusted threshold of 0.0006, and the
smallest relevant difference, so the fold fails under the rule of `csdg_check()`; the standard deviation of the fold
differences is 0.0086. The 22 sets of 20 permutations pooled (440 per fold) give the same result: 0.0103 [0.0015,
0.0191], 9 of 10 folds, met. Under neighboring criteria varied one at a time, the claim is met with a smallest
relevant difference of 0 or 0.5% of the improvement and with 8 of 10 folds, unresolved with 2% of the improvement
(0.0021, above the lower bound), and not met with 10 of 10 folds, because the reversal in fold 9 exceeds the
Holm-adjusted Monte Carlo threshold and the smallest relevant difference.

With the fold models held fixed, the bootstrap of the held-out respondents gives standard errors of the fold
differences of 0.0074 to 0.0088, about as large as the standard deviation of the fold differences (0.0086), and
removing the Monte Carlo error of 50 permutations changes them by less than 0.0001. The reversal in fold 9 is 0.32
bootstrap standard errors (standard error 0.0075; 95% percentile interval −0.0166 to 0.0112), below 1.96 and below
the Holm-adjusted 2.81 for 10 folds. In folds 1 and 5, self-rated health exceeds depressed mood by more than 2.81
bootstrap standard errors; in the other eight folds, the difference lies within 1.96 bootstrap standard errors. For
the reading of the stronger claim about every fitted model, the reversal in fold 9 thus exceeds permutation noise but
not the held-out sampling uncertainty.
