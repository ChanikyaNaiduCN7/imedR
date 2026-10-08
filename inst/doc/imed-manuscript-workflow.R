## ----eval=FALSE------------------------------------------------------------------------------
# library(imedR)
# db <- imed_data()
# 
# # 1. Audit the frozen release.
# imed_validate(db)
# 
# # 2. Create one defensible comparison set.
# x <- imed_analysis_set(
#   db,
#   comparability_class = "CMP-W-VOL",
#   metric_type = "Abundance"
# )
# 
# # 3. Describe and visualize without mixing incompatible denominators.
# imed_describe(x)
# p <- imed_plot(x, type = "histogram")
# imed_export_figure(p, "figure1.tiff", dpi = 600)
# 
# # 4. Check whether formal evidence synthesis is justified.
# imed_meta_eligibility(x)
# 
# # 5. Retrieve the exact primary-study citation/provenance trail.
# imed_cite(db, unique(x$Study_ID))
# imed_provenance(db, study_id = unique(x$Study_ID))

