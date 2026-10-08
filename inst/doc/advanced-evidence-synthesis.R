## ----eval=FALSE------------------------------------------------------------------------------
# library(imedR)
# db <- imed_data()
# audit <- imed_validation_report(db)
# 
# x <- imed_analysis_set(db, comparability_class="CMP-W-VOL")
# imed_check_comparability(x)
# imed_meta_eligibility(x)
# 
# # Only if eligibility succeeds:
# fit <- imed_meta_mean(x)
# imed_results_table(fit)
# imed_leave_one_out(x)
# imed_forest(fit)
# imed_funnel(fit)
# 
# # Moderator analysis is guarded by independent-effect count.
# # imed_meta_regression(x, moderators=c("Year"))

