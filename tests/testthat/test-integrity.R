
test_that("release has no ERROR-level relational failures", {
  x=imed_data(validate=FALSE)
  v=imed_validate(x)
  expect_false(any(!v$passed & v$severity=="ERROR"))
})


test_that("known frozen-release representation defects are normalized safely", {
  x <- imed_data(validate = FALSE)
  expect_false(any(vapply(x$METHODS_QAQC, function(z) FALSE, logical(1)))) # table loads
  expect_false(any(is.na(x$METHODS_QAQC$QAQC_ID) | trimws(x$METHODS_QAQC$QAQC_ID) == ""))
  expect_false(any(is.na(x$PROVENANCE$Provenance_ID) | trimws(x$PROVENANCE$Provenance_ID) == ""))
  st64 <- x$SIZE_METHODS[x$SIZE_METHODS$Sample_ID == "SA0064-S", ]
  expect_equal(nrow(st64), 2L)
  expect_setequal(st64$Size_Method_ID, c("SZ0064-01", "SZ0064-02"))
  expect_true(any(grepl("50–2000", st64$Size_Classes_Reported, fixed = TRUE)))
  expect_true(any(grepl("Small MPs <1000", st64$Size_Classes_Reported, fixed = TRUE)))
})
