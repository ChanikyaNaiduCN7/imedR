
`%||%` <- function(x,y) if (is.null(x)) y else x
.imed_stop <- function(...,call.=FALSE) stop(...,call.=call.)
.imed_warn <- function(...,call.=FALSE) warning(...,call.=call.)
.imed_nonmissing <- function(x) !is.na(x) & trimws(as.character(x))!="" &
  !tolower(trimws(as.character(x))) %in% c("not reported","nr","not determinable","nd","not applicable","na")
.imed_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
.imed_table <- function(x,name) {
  if (inherits(x,"imed_db")) {
    if (!name %in% names(x)) .imed_stop("Table not available: ",name)
    return(x[[name]])
  }
  if (is.data.frame(x)) return(x)
  .imed_stop("Expected an imed_db object or data frame.")
}
.imed_col <- function(x,candidates,required=TRUE) {
  z <- candidates[candidates %in% names(x)]
  if (length(z)) return(z[[1]])
  if (required) .imed_stop("Required column absent. Expected one of: ",paste(candidates,collapse=", "))
  NULL
}
.imed_pkg_file <- function(name) system.file("extdata",name,package="imedR")
#' Package and targeted data-release version
#' @export
imed_version <- function() c(package="0.3.1",imed_release="1.0")


# Remove rows that contain no information in any field. These can arise from
# formatted-but-empty worksheet rows in the frozen release export.
.imed_drop_empty_rows <- function(d) {
  if (!nrow(d)) return(d)
  informative <- vapply(seq_len(nrow(d)), function(i) {
    any(vapply(d[i, , drop = FALSE], .imed_nonmissing, logical(1)))
  }, logical(1))
  d[informative, , drop = FALSE]
}

# Apply narrowly scoped, documented runtime normalizations for known release
# representation defects. Scientific values are not changed.
.imed_release_normalize <- function(d, table) {
  d <- .imed_drop_empty_rows(d)
  if (identical(table, "SIZE_METHODS") &&
      all(c("Size_Method_ID", "Sample_ID", "Size_Classes_Reported") %in% names(d))) {
    hit <- which(
      d$Size_Method_ID == "SZ0064-01" &
      d$Sample_ID == "SA0064-S" &
      grepl("Small MPs <1000", as.character(d$Size_Classes_Reported), fixed = TRUE)
    )
    if (length(hit) == 1L) d$Size_Method_ID[hit] <- "SZ0064-02"
  }
  d
}
