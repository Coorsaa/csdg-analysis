# Preprocessing verification record

**Status:** `verified_by_responsible_coauthors`
**Verification date:** 2026-09-09
**Scope:** SHILD and PISA analytic cuts and preprocessing
**Verification authority:** group-level confirmation by the responsible co-authors who prepared the analytic data

This record confirms the scientific validity of the analytic cuts and preprocessing used for the frozen results.
It does not identify an individual signer, claim an independent rerun of the protected SHILD source, or claim an
independent reconstruction of the PISA compact extract.

## SHILD preprocessing definition

The analysis starts from the protected SHILD questionnaire export. A1 codes 1 and 2 map to `content`, codes 4 and 5
map to `discontent`, and neutral or non-substantive codes 3, 8, and 9 are excluded. The resulting analytic sample has
16,971 respondents aged 16-64: 15,943 content and 1,028 discontent.

F15 remains numeric on its documented 0-10 scale, with 98 and 99 treated as missing. The workflow records every
categorical mapping, source-code count, numeric sentinel recode, and sample-flow exclusion. It recodes `E1=999`,
`E2 in {998,999}`, `J17=999`, and `M6 in {99998,99999}` as audited high codes. E1 values below 100 cm and E2 values
below 20 kg are treated as implausible adult entries and set to missing; no alternative units are inferred. The
dictionary does not contain value labels, so the retained mappings are transparent in the exported recode ledger.

## PISA preprocessing scope

PISA uses the supplied compact extract and manifest. The verified workflow uses the declared equal-country sample,
school-clustered splits, all ten reading plausible values, questionnaire missing-code handling, and identifier
exclusion. It is an unweighted analytic sample and does not support a design-based PISA population claim.

## Verification checklist

| Item | SHILD | PISA | Evidence / limitation |
|---|---|---|---|
| Intended frozen input and checksum record | verified | verified | Run provenance records the supplied input identity. |
| Outcome and plausible-value definitions | verified | verified | A1 mapping and PV1READ-PV10READ are fixed in the analysis code. |
| Prespecified feature set | verified | verified | Feature and exclusion lists are explicit and exported. |
| Raw outcome, categorical, sentinel, and low-value handling | verified | n/a | The SHILD recode ledger remains the auditable record. |
| Audit-only variables and identifiers excluded from model features | verified | verified | Model tasks retain only declared predictors. |
| Fold-contained imputation, encoding, and scaling | verified | verified | Learned preprocessing is fitted within analysis folds. |
| School integrity and aligned plausible-value splits | n/a | verified | No school crosses a split; all ten PVs use aligned folds. |
| Held-out fold-specific PFI | verified | verified | Each fold model is queried only on its assessment rows. |
| SHILD local cases and held-out models | verified | n/a | Communication cases are post hoc and separated from the audit. |
| Complete-case estimand and split policy | verified | verified | Membership and changed composition are exported. |
| Model-class comparisons without fold tests | verified | verified | Comparisons are descriptive sensitivity analyses. |
| PISA observed-country refits | n/a | verified | All 320 country-by-PV LOCO fits are genuine refits. |
| Publication rather than quick-mode frozen results | verified | verified | Run-mode tables record publication settings. |
| Private and sanitized inventories | verified | verified | Structural output audits accompany the frozen bundles. |
| Recorded warnings and errors | verified | verified | Conditions and console logs remain part of provenance. |

## Scope statement

The responsible co-authors verified the analytic cuts and preprocessing; the article focuses on the interpretation
framework and downstream diagnostic analyses. This author-confirmed status is distinct from independent raw-data
reproduction and does not enlarge the inferential scope of either study.
