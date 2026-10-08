# imedR 0.3.1

`imedR` is a manuscript-grade, guardrail-first R package for the Indian
Microplastics Evidence Database (IMED).

## Frozen v1.0 snapshot represented in this build

- Studies: 97
- Sites: 217
- Samples: 293
- Measurements: 313
- Comparability classes: 17

The bundled analytical snapshot is generated directly from
`IMED_v1.0_FROZEN.xlsx`; workflow and extraction-log sheets are excluded.

## Capabilities

- relational/scientific validation;
- measurement enrichment with IMED comparability metadata;
- guarded analysis-set construction;
- explicit HOLD handling;
- descriptive and conservative inferential statistics;
- meta-analysis eligibility checks;
- guarded random-effects raw-mean and prevalence meta-analysis helpers;
- adversarial guardrail tests and known-answer statistical tests;
- size-window diagnostics and evidence-synthesis sensitivity summaries;
- leave-one-study-out influence analysis and guarded meta-regression;
- forest/funnel diagnostics and a consolidated validation report;
- publication-oriented abundance, composition and QA/QC figures;
- coordinate-class-aware mapping;
- missingness and evidence-profile diagnostics;
- primary citation and field-level provenance retrieval;
- manuscript-oriented tables and result tidying;
- high-resolution figure export.

## Core rule

**Validate -> define a scientifically compatible analysis set -> analyse ->
visualize -> retrieve provenance/citations.**

The package is designed to stop analyses that look computationally possible but
are scientifically indefensible.

## Validation status

The test suite passes (27 tests, 0 failures, 0 warnings, 0 skipped), and the
IMED v1.0 relational integrity checks report no ERROR-level failures.
`R CMD check --as-cran` was run on the Windows build service
(win-builder, R 4.6.1, 8 October 2026) for version 0.3.1 and returned
no errors and no warnings; the only note was the standard "New submission"
note for a package not yet on CRAN.

## Release-normalization note (v0.3.1)

The bundled frozen IMED v1.0 source snapshot is preserved. At load time, imedR
removes wholly empty export rows and disambiguates the second genuine ST0064
size-method record from `SZ0064-01` to runtime ID `SZ0064-02`. This is an
identifier-only normalization; no scientific value is altered.

## Installation

```r
install.packages("path/to/imedR", repos = NULL, type = "source")
library(imedR)
imed_version()
```

## Citation

Naidu C (2026). imedR: Guarded Evidence Synthesis for the Indian
Microplastics Evidence Database. R package version 0.3.1.

Please also cite IMED/IMEA v1.0 (DOI 10.5281/zenodo.23180568) and the
original primary publications for any study-specific claim.

## License

Package code: MIT (see `LICENSE`). The IMED/IMEA data release is licensed
separately (see `LICENSE_AND_REUSE.md` in the data release). Third-party
source rights remain with the original rights holders.

## Release limitations

IMED v1.0 did not complete a second-review round, and no second-review PASS
is claimed. See `RELEASE_NOTES_v1.0.md` in the data release.