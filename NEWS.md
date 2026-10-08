# imedR 0.3.1

## Integrity patch

- Drops wholly empty worksheet-export rows at runtime; this removes one empty METHODS_QAQC row and one empty PROVENANCE row without changing scientific records.
- Preserves both genuine ST0064 size-method observations while deterministically disambiguating the second runtime identifier as `SZ0064-02`; scientific content is unchanged and the frozen IMED v1.0 workbook is not modified.
- Adds regression tests for these release-representation defects.
- Removes an unimported `%>%` dependency from grouped descriptive statistics.
- No scientific abundance, method, coordinate, comparability, provenance content, or HOLD decision is changed.

## Validation status

The test suite passes (27 tests, 0 failures). `R CMD check --as-cran`
completed on win-builder (R 4.6.1, Windows) with no errors or warnings;
the only note was "New submission".

# imedR 0.3.1

## Integrity patch

- Drops wholly empty worksheet-export rows at runtime; this removes one empty METHODS_QAQC row and one empty PROVENANCE row without changing scientific records.
- Preserves both genuine ST0064 size-method observations while deterministically disambiguating the second runtime identifier as `SZ0064-02`; scientific content is unchanged and the frozen IMED v1.0 workbook is not modified.
- Adds regression tests for these release-representation defects.
- Removes an unimported `%>%` dependency from grouped descriptive statistics.
- Fixes `imed_check_size_window()` so it reads the actual `Operational_Reporting_Cutoff_um` column; previously the check for records with neither an analytical lower limit nor an operational cutoff did not run.
- Documents all function arguments.
- No scientific abundance, method, coordinate, comparability, provenance content, or HOLD decision is changed.

## Validation status

The test suite passes under R 4.5.2 on Windows (27 tests, 0 failures).
Package checks are being completed on external build infrastructure; no
`R CMD check` pass is claimed until that is confirmed.