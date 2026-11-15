# Computational supplement

This folder contains the computational supplement of the article *Which Conclusions Does an Explanation Support?
Claim-Scoped Diagnostic Gates for Interpretable Machine Learning in Psychology*: 30 monochrome figure components
and 27 aggregate diagnostic tables on calibration, subgroup comparisons, one- and two-dimensional ALE, aggregate ICE,
and prediction multiplicity for SHILD and PISA (15 components per study). `computational_supplement.pdf` (also
distributed as `publication.pdf`) assembles the components into a captioned reading copy with 28 figures, numbered
CS1–CS28 so that they are distinct from the figures of the online supplement (S1, S2, ...).

## Files

- `computational_supplement.pdf`: the assembled supplement; `publication.tex` is its LaTeX source, written by
  `assemble_publication.py`.
- `shild/`, `pisa/`: figure components (PDF and PNG) and their source tables.
- `plot_data/`: SHILD decile and kernel-width summaries of the 300-case local-fidelity evaluation (the decile summary
  underlies Figure S1 of the online supplement; the kernel-width summary is not shown as a figure in the article).
  Points are medians of within-case medians over 20 repetitions; whiskers are 25th and 75th percentiles across cases,
  not confidence intervals.
- `FIGURE_MANIFEST.csv`: components, panel order, dimensions, source tables, and uncertainty semantics.
- `SOURCE_TABLE_MANIFEST.csv`, `PUBLICATION_INDEX.csv`: table identities and the mapping of components to figures
  (CS1–CS28), pages, captions, and checksums.
- `PRIVATE_INPUT_PROVENANCE.csv`: basename, row count, and SHA-256 of restricted inputs, which are not distributed.
- `SHA256SUMS`: checksums of all files.

All tables are aggregates; the folder contains no respondent or student records, predictions, or fitted models.

## Build and verify

From the root of the analysis repository (`csdg-analysis`):

```sh
python3 supplement/build_supplement.py --verify
python3 supplement/assemble_publication.py --output build/supplement
```

The assembly requires XeLaTeX with `latexmk`, Times New Roman, and Arial. It writes `publication.tex` and
`PUBLICATION_INDEX.csv`, builds the PDF in `build/supplement/`, copies it to `supplement/computational_supplement.pdf`,
and refreshes and verifies `SHA256SUMS`. The PDF has a fixed creation date and a content-derived identifier, so a
rebuild with the same TeX installation and fonts reproduces the same bytes. To re-render the components from the
aggregate tables, run `Rscript analysis/render_supplement_figures.R` and then
`python3 supplement/build_supplement.py --refresh-checksums`. Neither step reruns the empirical analyses.
