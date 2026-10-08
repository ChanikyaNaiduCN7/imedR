
test_that("abundance and prevalence classes cannot pool", {
  d=tibble::tibble(Comparability_Class=c("CMP-W-VOL","CMP-PREV"),
    Reported_Unit=c("particles/L","%"),Mass_Basis=NA_character_,
    Strict_Synthesis_Eligibility=c("YES — WITHIN CLASS","YES — WITHIN CLASS"))
  expect_error(imed_check_comparability(d),"Unsafe analysis")
})
test_that("wet and dry mass basis cannot pool", {
  d=tibble::tibble(Comparability_Class="CMP-SED-DW",
    Reported_Unit="items/kg",Mass_Basis=c("Dry","Wet"),
    Strict_Synthesis_Eligibility="YES — WITHIN CLASS")
  expect_error(imed_check_comparability(d),"Unsafe analysis")
})
test_that("different reported units cannot silently pool", {
  d=tibble::tibble(Comparability_Class="CMP-W-VOL",
    Reported_Unit=c("items/L","items/m3"),Mass_Basis=NA_character_,
    Strict_Synthesis_Eligibility="YES — WITHIN CLASS")
  expect_error(imed_check_comparability(d),"Unsafe analysis")
})
