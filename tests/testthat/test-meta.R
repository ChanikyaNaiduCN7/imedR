
test_that("meta eligibility rejects duplicate study effects", {
  d=tibble::tibble(Study_ID=c("ST1","ST1"),Mean=c(1,2),SD=c(1,1),N=c(10,10),
    Comparability_Class="CMP-W-VOL",Reported_Unit="particles/L",
    Mass_Basis=NA_character_,Strict_Synthesis_Eligibility="YES — WITHIN CLASS")
  e=imed_meta_eligibility(d)
  expect_false(all(e$eligible))
})
