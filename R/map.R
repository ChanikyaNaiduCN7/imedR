
#' Coordinate-provenance-aware IMED map
#' @param x An `imed_db` object, as returned by `imed_data()`.
#' @param geo_min Character vector of coordinate classes to retain, chosen from `"GEO-4"`,
#'   `"GEO-3"` and `"GEO-2"` (default all three). Only sites whose stored class
#'   is in this set and that have numeric coordinates are drawn.
#' @param study_id Optional character vector of `Study_ID` values used to restrict the records.
#' @export
imed_map <- function(x,geo_min=c("GEO-4","GEO-3","GEO-2"),study_id=NULL) {
  if (!requireNamespace("sf",quietly=TRUE)) .imed_stop("Install 'sf' to use imed_map().")
  if (!inherits(x,"imed_db")) .imed_stop("imed_map() requires an imed_db.")
  s=x$SITES
  g=x$SITE_COORDINATE_CLASS
  d=dplyr::left_join(s,g[,c("Site_ID","GEO_Class","Permitted_Use")],by="Site_ID")
  if (!is.null(study_id)) d=d[d$Study_ID %in% study_id,,drop=FALSE]
  d=d[d$GEO_Class %in% geo_min & is.finite(.imed_num(d$Latitude)) &
      is.finite(.imed_num(d$Longitude)),,drop=FALSE]
  pts=sf::st_as_sf(d,coords=c("Longitude","Latitude"),crs=4326,remove=FALSE)
  ggplot2::ggplot(pts)+ggplot2::geom_sf()+ggplot2::theme_void()
}
