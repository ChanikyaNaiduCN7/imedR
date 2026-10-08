
test_that("unresolved comparability is blocked", {
  d=tibble::tibble(Comparability_Class=c("CMP-SED-DW","CMP-SED-UNRESOLVED"),
    Reported_Unit=c("items/kg","items/kg"),Mass_Basis=c("Dry",NA),
    Strict_Synthesis_Eligibility=c("YES — WITHIN CLASS","NO / DESCRIPTIVE"))
  expect_error(imed_check_comparability(d),"Unsafe analysis")
})
test_that("mixed classes are blocked", {
  d=tibble::tibble(Comparability_Class=c("CMP-W-VOL","CMP-SED-DW"),
    Reported_Unit=c("particles/L","particles/kg"),Mass_Basis=c(NA,"Dry"),
    Strict_Synthesis_Eligibility=c("YES — WITHIN CLASS","YES — WITHIN CLASS"))
  expect_error(imed_check_comparability(d),"Unsafe analysis")
})
