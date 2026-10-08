
test_that("descriptive statistics reproduce known values", {
  d=tibble::tibble(Comparability_Class="CMP-W-VOL",
    Reported_Unit="items/L",Mass_Basis=NA_character_,
    Strict_Synthesis_Eligibility="YES — WITHIN CLASS",
    Mean=c(1,2,3,4,5))
  z=imed_describe(d)
  expect_equal(z$mean,3)
  expect_equal(z$median,3)
  expect_equal(z$sd,sqrt(2.5))
  expect_equal(z$q1,2)
  expect_equal(z$q3,4)
})
