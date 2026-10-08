
test_that("bundled release has frozen v1.0 cardinalities", {
  x=imed_data(validate=FALSE)
  expect_equal(nrow(x$STUDIES),97)
  expect_equal(nrow(x$SITES),217)
  expect_equal(nrow(x$SAMPLES),293)
  expect_equal(nrow(x$MEASUREMENTS),313)
  expect_equal(nrow(x$COMPARABILITY_CLASSES),17)
})
