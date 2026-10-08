
#' Load the bundled frozen IMED v1.0 analytical snapshot
#'
#' The package exposes 19 core scientific/schema tables. Internal extraction,
#' workflow and QC-log sheets are intentionally not part of the analytical API.
#' @param tables Optional table names.
#' @param validate Run validation after loading.
#' @export
imed_data <- function(tables=NULL,validate=TRUE) {
  all <- c("STUDIES","SITES","SAMPLES","MEASUREMENTS","SIZE_METHODS","MATRIX_CONTEXT",
    "PARTICLE_CHARACTERISTICS","POLYMERS","BIOTA_CONTEXT","METHODS_PROCESSING",
    "METHODS_QAQC","DERIVED_METRICS","RISK_ASSESSMENTS","SOURCE_DISCREPANCIES",
    "PROVENANCE","COMPARABILITY_CLASSES","MEASUREMENT_COMPARABILITY",
    "SITE_COORDINATE_CLASS","DATA_DICTIONARY")
  tables <- tables %||% all
  bad <- setdiff(tables,all)
  if (length(bad)) .imed_stop("Unknown core table(s): ",paste(bad,collapse=", "))
  out <- stats::setNames(lapply(tables,function(nm) {
    p <- .imed_pkg_file(paste0(tolower(nm),".csv"))
    if (p=="") .imed_stop("Bundled IMED table missing: ",nm)
    d <- tibble::as_tibble(readr::read_csv(p,show_col_types=FALSE,na=c("","NA")))
    .imed_release_normalize(d, nm)
  }),tables)
  class(out) <- c("imed_db","list")
  attr(out,"imed_release") <- "1.0"
  if (validate) attr(out,"validation") <- imed_validate(out)
  out
}

#' List loaded IMED tables
#' @param x An `imed_db` object. Defaults to the bundled snapshot loaded without
#'   validation.
#' @export
imed_tables <- function(x=imed_data(validate=FALSE)) {
  if (!inherits(x,"imed_db")) .imed_stop("x must be an imed_db.")
  tibble::tibble(table=names(x),rows=vapply(x,nrow,integer(1)),
    columns=vapply(x,ncol,integer(1)))
}

#' Join measurement records to their scientific comparability metadata
#' @param x An imed_db.
#' @export
imed_enrich_measurements <- function(x) {
  m <- .imed_table(x,"MEASUREMENTS")
  c <- .imed_table(x,"MEASUREMENT_COMPARABILITY")
  dplyr::left_join(m,c,by=c("Measurement_ID"="Measurement_ID"),
    suffix=c("",".cmp"))
}
