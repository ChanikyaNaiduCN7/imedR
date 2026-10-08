
test_that("frozen release exposes, rather than hides, strict-synthesis holds", {
  x=imed_data(validate=FALSE)
  h=imed_flag_holds(x)
  expect_true(any(h$.imed_hold))
  expect_true(all(c(".imed_hold",".imed_hold_reason") %in% names(h)))
})
