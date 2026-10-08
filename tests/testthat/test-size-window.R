
test_that("size window checker returns diagnostics", {
  x=imed_data(validate=FALSE)
  z=imed_check_size_window(x,action="return")
  expect_true(all(c("compatible","issue") %in% names(z)))
})
