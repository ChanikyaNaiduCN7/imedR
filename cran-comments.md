## Test environments
* Windows (win-builder, R 4.6.1)
* GitHub Actions: Windows, macOS and Ubuntu (R release), Ubuntu (R devel);
  all jobs run with warnings treated as errors

## R CMD check results
0 errors | 0 warnings | 1 note

* This is a new submission.

## Notes
* The package bundles a frozen snapshot of the Indian Microplastics Evidence
  Database (IMED v1.0) as CSV files in inst/extdata. The data are factual
  observations compiled from published studies, with study-level provenance and
  citations retrievable from the package; the primary publications remain the
  authoritative sources. The database is archived at
  https://doi.org/10.5281/zenodo.23180568.
* The Rd files have no \examples; usage is demonstrated in two vignettes.
