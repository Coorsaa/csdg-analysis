# PISA confirmatory stage: claims and criteria fixed before the analysis

This file is committed before `analysis/pisa/ses_reanalysis.R --stage=confirmatory` is run. The claims were derived
from the exploratory stage (`analysis/pisa/results/ses_reanalysis/exploratory/`), which used the equal-country sample
of the primary PISA analysis. The confirmatory stage draws 1,500 further students per country who were not in that
sample (seed 20260929; students are disjoint, schools can overlap and their number is reported) and repeats the
exploratory stage exactly: same 41 predictors, preprocessing, school-grouped country-stratified five-fold splits, the
same four learners, held-out PFI with permutation within country (school-level predictors permuted between schools of
the same country), 10 permutations per fold, RMSE loss, and ALE on held-out folds.

## Learners and admission

XGBoost (reference), random forest, ridge regression with fixed penalty (lambda = 0.10), and ridge regression with
penalty chosen by inner five-fold cross-validation (lambda.1se). A comparison learner counts as similarly accurate if
its mean held-out RMSE across the 10 plausible values exceeds that of XGBoost by at most 5 points. Claims about "the
similarly accurate learners" cover the admitted learners; a learner that is not admitted is reported and excluded.

## Claims

All values are held-out PFI (increase in RMSE) of the fold-specific fits, averaged over the five folds, per plausible
value; "averaged over plausible values" means the mean of the 10 values (point estimate for multiple imputations).

**C1. Plausible values (XGBoost).** "The five most important predictors of XGBoost are assessing credibility
(METASPAM), summarizing strategies (METASUM), perceived reading competence (SCREADCOMP), enjoyment of reading
(JOYREAD), and perceived reading difficulty (SCREADDIFF), for every plausible value."
Criterion: in each of the 10 plausible values, these five predictors have the five largest PFI values among the 41
individual predictors.

**C2. Most important predictor.** "Assessing credibility is the most important predictor of every similarly accurate
learner."
Criterion: in each admitted learner, METASPAM has the largest PFI averaged over plausible values among the 41
individual predictors, and the largest PFI in at least 9 of the 10 plausible values.

**C3. Socioeconomic information.** "None of the similarly accurate learners relies more than twice as much on the
socioeconomic indices as another."
Criterion: grouped PFI of the complete family of socioeconomic indices (ESCS, HISEI, PARED, HOMEPOS, WEALTH, HEDRES,
CULTPOSS, ICTRES), averaged over plausible values: the largest value across the admitted learners is at most twice the
smallest; and the same holds in at least 9 of the 10 plausible values.

**C4. Choice of block (procedure).** "For the ridge regression with fixed penalty, permuting ESCS, parental
occupation, and home possessions together increases the error at least twice as much as permuting the complete family
of socioeconomic indices."
Criterion: grouped PFI of the block {ESCS, HISEI, HOMEPOS} is at least twice the grouped PFI of the complete family, in
each of the 10 plausible values.

**C5. Direction.** "In every similarly accurate learner, predicted reading proficiency rises with assessing
credibility." (A description of the models' predictions, not a causal claim.)
Criterion: the first-order ALE of METASPAM, computed on each held-out fold with its fold-specific fit and averaged over
the five folds, is at least 10 points (0.1 standard deviations of the PISA scale) higher at the 90th than at the 10th
percentile of METASPAM, in every admitted learner and every plausible value.

## Decision rule

Each claim is met if its criterion holds, not met if it fails. The criteria are applied exactly as written; results
are reported for all claims, whatever their outcome. Deviations from this plan, if any become necessary (for example,
a failed computation), are reported as deviations.
