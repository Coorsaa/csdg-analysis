# Computational supplement

This folder contains the computational supplement of the article: 30 monochrome figure components and 27 aggregate
diagnostic tables on calibration, subgroup comparisons, one- and two-dimensional ALE, aggregate ICE, and prediction
multiplicity for SHILD and PISA (15 components per study). `publication.tex` assembles the components into a
captioned reading copy with 28 figures.

## Files

- `shild/`, `pisa/`: figure components (PDF and PNG) and their source tables.
- `plot_data/`: SHILD decile and kernel-width summaries of the 300-case local-fidelity evaluation (Supplementary
  Figures S2 and S4a of the article). Points are medians of within-case medians over 20 repetitions; whiskers are
  25th and 75th percentiles across cases, not confidence intervals.
- `FIGURE_MANIFEST.csv`: components, panel order, dimensions, source tables, and uncertainty semantics.
- `SOURCE_TABLE_MANIFEST.csv`, `PUBLICATION_INDEX.csv`: table identities and the mapping of components to figures,
  pages, and captions.
- `PRIVATE_INPUT_PROVENANCE.csv`: basename, row count, and SHA-256 of restricted inputs, which are not distributed.
- `SHA256SUMS`: checksums of all files.

All tables are aggregates; the folder contains no respondent or student records, predictions, or fitted models. For
medicine and supplement spending (SHILD M6), the metadata identify only the recorded survey amount; its currency and
recall period are stated as author confirmations in the captions.

## Build and verify

From the repository root:

```sh
python3 supplement/build_supplement.py --verify
python3 supplement/assemble_publication.py --output build/supplement
```

The assembly requires XeLaTeX, Times New Roman, and Arial. To re-render the components from the aggregate tables, run
`Rscript analysis/render_supplement_figures.R` and then `python3 supplement/build_supplement.py --refresh-checksums`.
Neither step reruns the empirical analyses.
