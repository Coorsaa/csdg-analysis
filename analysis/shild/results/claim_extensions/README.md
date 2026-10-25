# SHILD claim extensions: aggregate results

Written by `analysis/shild/claim_extensions.R`. All tables are aggregates over folds (about 1,700 respondents each),
deciles of predicted probability (30 cases each), all 300 evaluated cases, or the 16,971 respondents of the analytic
sample; none contains respondent- or case-level values. Intervals are corrected resampled t intervals over the 10
folds (Nadeau & Bengio, 2003): standard error `sqrt((1/10 + 1/9) * var)`, t quantile with 9 degrees of freedom;
`p_greater` is the one-sided p value for an estimate above zero. The simulation in `analysis/simulation/` shows that
these intervals are too lenient for K-fold cross-validation, so they are reported descriptively.

## Global importance of the XGBoost learner (`--part=importance`)

Stored held-out PFI of the primary run (two runs of 10 permutations per fold, pooled to 20). Loss: log loss.

| File | Columns |
|---|---|
| `importance_fold_pfi.csv` | `fold`, `item`, `pfi` (mean over 20 permutations), `mc_se` (Monte Carlo standard error), `n_permutations` |
| `importance_item_means.csv` | `item`, `mean_pfi` over folds, `min_fold`, `max_fold` |
| `importance_fold_ratios.csv` | per fold: PFI of self-rated health (`A2`) and depressed mood (`D2`) divided by the largest other item; `difference_A2_minus_D2` |
| `importance_learner_intervals.csv` | learner-level comparisons: `A2 - 2 x M8`, `D2 - 2 x M8` (M8 = largest other item on average), the same against the largest other item per fold, and `A2 - D2` |
| `importance_fold_A2_vs_D2.csv` | per fold: difference `A2 - D2`, its Monte Carlo standard error, the threshold t(.975, 19) x standard error, and whether the difference exceeds it |

## Population importance: leave-one-covariate-out (`--part=loco`)

Each learner (XGBoost, ridge logistic regression, random forest; settings of the primary run) refitted on the primary
folds with all 27 items and without each item in turn.

| File | Columns |
|---|---|
| `loco_fold_losses.csv` | `learner`, `design` (`all_items` or the omitted item), `fold`, `n_test`, held-out `log_loss` and `weighted_log_loss` (weighted by the survey weight), `brier` and `weighted_brier`, and `n_outcome_at_bound`: held-out respondents whose observed outcome received probability 0 (their log loss is set by the clipping bound of 1e-15 alone) |
| `loco_fold_importance.csv` | `loco` and `loco_weighted`: increase in held-out log loss after refitting without `item`; `loco_brier` and `loco_brier_weighted`: the same for the Brier score |
| `loco_summary.csv` | mean over folds with corrected interval and `rank` within learner and measure (`loco`, `loco_weighted`, `loco_brier`, `loco_brier_weighted`) |
| `loco_comparisons.csv` | for `focal` in A2, D2: difference to every `other` item with corrected interval; `p_holm` Holm-adjusted over the 50 comparisons of a learner and measure |

Random forest predicts probability 0 for some held-out dissatisfied respondents (`n_outcome_at_bound`); each such
respondent adds about 0.020 to a fold's mean log loss, which dominates its log-loss LOCO values. The Brier score is
bounded and serves as a sensitivity analysis.

## Conditional PFI on realistic inputs (`--part=conditional`)

XGBoost refitted on the primary folds with the settings of the primary run. For each item, a regression or
classification tree (rpart; maximum depth 4, at least 50 respondents per leaf, cp = 0.001) predicts the item from the
other 26 items on the training fold; in the held-out fold, the item is permuted within the leaves of this tree, so that
the permuted inputs stay close to realistic combinations (conditional subgroup approach, Molnar et al., 2024). Loss:
log loss; 20 permutations per item and fold.

| File | Columns |
|---|---|
| `conditional_fold_pfi.csv` | `fold`, `item`, `cpfi` (mean increase in held-out log loss), `mc_se`, `n_permutations`, `n_subgroups` (leaves) |
| `conditional_item_means.csv` | `item`, `mean_cpfi` over folds, `min_fold`, `max_fold`, `median_subgroups` |
| `conditional_summary.csv` | means of A2 and D2, the largest other item, the smallest per-fold ratios of A2 and D2 to the largest other item, and the number of folds in which A2 and D2 are the top two and each has at least twice the PFI of the third |

## Nested check of the predictor selection (`--part=selection`)

Recursive feature elimination with XGBoost (summed gain per item, 20% of the items dropped per step until 27 remain)
inside each training fold, from all candidate variables of the survey (use or selection flag in the dictionary, without
identifiers, outcome, and weight), compared with the fixed 27 items; both with the same simple preprocessing.

| File | Columns |
|---|---|
| `selection_fold_results.csv` | per fold: number of candidates, elimination steps, held-out AUC and log loss for the nested selection and the fixed items, Jaccard overlap of the selected items with the 27, whether A2 and D2 were selected |
| `selection_item_frequency.csv` | number of folds in which each item was selected |
| `selection_comparison.csv` | nested minus fixed, mean over folds with corrected interval |

## Repeated local surrogates (`--part=local`)

The 300 sampled cases of the primary local evaluation (30 per decile of predicted probability), 20 repeated surrogates
per case with the seeds of the primary evaluation, on the probability (`response`) and log-odds (`link`) scale. The
contribution of an item is its term contributions to the surrogate prediction, centered at the weighted neighborhood
mean; its magnitude is the weighted root mean square over the neighborhood. Per repetition the two items with the
largest magnitudes form the top pair; per case, the most frequent pair and its count (of 20) are recorded.

| File | Columns |
|---|---|
| `local_identity_check.csv` | number of replicate values compared with the stored primary evaluation and the maximum absolute difference in cross-fitted weighted R2 |
| `local_top_items_by_decile.csv` | per scale and decile: share of cases whose most frequent top pair occurs in at least 19 of 20 repetitions, median and minimum count, share of cases with median cross-fitted weighted R2 of at least .80, median R2 |
| `local_top_items_overall.csv` | the same over all 300 cases |
| `local_top_items_top_decile.csv` | the 30 cases of the highest decile, including the share meeting both criteria |

## Aggregates from the frozen primary outputs (`--part=export`)

Read from the private frozen outputs of the primary SHILD run and the evidence synthesis in `paper_artifacts/current/`
without data access or refitting. Every row summarizes at least 10 cases or respondents. Local fidelity refers to the
primary kernel width and the probability scale; each case enters with its median over the 20 repetitions, and
quartiles across cases are type-8 quantiles. A fold within a decile holds only 3 cases and is not exported.

| File | Columns |
|---|---|
| `export_local_fidelity_summary.csv` | over the 300 cases: `n_cases`, median and quartiles (`q25`, `q75`) of apparent and cross-fitted weighted R2 and of cross-fitted weighted RMSE, `n_apparent_above_crossfit` (cases whose apparent R2 exceeds the cross-fitted R2) |
| `export_local_fidelity_by_decile.csv` | the same per decile of predicted probability (30 cases each) |
| `export_local_fidelity_by_fold.csv` | the same per fold (30 cases each) |
| `export_item_missingness.csv` | per `stage` (before and after the documented recoding) and `item`: analytic sample size `n`, `n_missing`, `missing_share` |
| `export_importance_fold_pfi_by_run.csv` | per `run` (1 = `shild_gate_g5__pfi__raw.csv`, 2 = `shild_gate_g6a__model_generalization__pfi__xgboost__raw.csv`) and `fold`: held-out fold size `n_assessment`, `n_permutations` (10), PFI and Monte Carlo standard error of `A2` and `D2`, their difference and its standard error |
