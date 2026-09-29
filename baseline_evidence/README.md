# Results of the primary study runs

Shareable aggregate outputs of the primary SHILD and PISA runs and of the evidence synthesis (`mlr3autoiml` 0.1.0,
September 2026). Scripts in `analysis/` read these tables. Fitted models, predictions, and case-level records are not
included.

- `STUDY_RESULTS/SHILD/`, `STUDY_RESULTS/PISA/`: outputs of `analysis/shild/analysis.R` and `analysis/pisa/analysis.R`.
- `EVIDENCE_SYNTHESIS/`: 300-case SHILD local-fidelity evaluation and cross-study summaries (`analysis/evidence_synthesis.R`).
- `provenance/`: session information of the three runs (`shild_sessionInfo.txt`, `pisa_sessionInfo.txt`,
  `evidence_synthesis_sessionInfo.txt`; R 4.5.1, `mlr3autoiml` 0.1.0), with local library paths redacted.

The claim cards in these outputs configure the diagnostic audit of the runs; they are not the claims of the article.
The G0b row of `STUDY_RESULTS/SHILD/tables/shild_report_card.csv` is `unresolved` because the run (September 8, 2026)
recorded the preprocessing verification as not checked; the verification was recorded afterward
(`analysis/preprocessing_review.md`, September 9, 2026), and the article assesses G0b for each claim, not from this row.
