
#' Filter IMED with scientific guard metadata retained
#' @param x An `imed_db` (HOLD flags are added automatically) or a measurement-level
#'   data frame.
#' @param matrix Optional `Matrix_Code` value(s) to keep.
#' @param comparability_class Optional `Comparability_Class` value(s) to keep.
#' @param mass_basis Optional `Mass_Basis` value(s) to keep.
#' @param metric_type Optional `Metric_Type` value(s) to keep.
#' @param study_id Optional character vector of `Study_ID` values used to restrict the records.
#' @param include_holds If `FALSE` (default), records flagged as scientific HOLDs are removed.
#' @export
imed_filter <- function(x,matrix=NULL,comparability_class=NULL,mass_basis=NULL,
                        metric_type=NULL,study_id=NULL,include_holds=FALSE) {
  d <- if (inherits(x,"imed_db")) imed_flag_holds(x) else tibble::as_tibble(x)
  f <- function(d,col,val) {
    if (is.null(val)) return(d)
    if (!col %in% names(d)) .imed_stop("Filter field absent: ",col)
    d[d[[col]] %in% val,,drop=FALSE]
  }
  d=f(d,"Matrix_Code",matrix)
  d=f(d,"Comparability_Class",comparability_class)
  d=f(d,"Mass_Basis",mass_basis)
  d=f(d,"Metric_Type",metric_type)
  d=f(d,"Study_ID",study_id)
  if (!include_holds && ".imed_hold" %in% names(d)) d=d[!d$.imed_hold,,drop=FALSE]
  tibble::as_tibble(d)
}

#' Check scientific comparability of an analysis subset
#' @param action error, warn, or return.
#' @param require_single_unit Require one reported unit. Convertible units should
#' first be explicitly standardized outside this function.
#' @param x An `imed_db`, or a measurement-level data frame that includes comparability
#'   metadata (for example from `imed_filter()`).
#' @export
imed_check_comparability <- function(x,action=c("error","warn","return"),
                                     require_single_unit=TRUE) {
  action=match.arg(action); d=.imed_table(x,"MEASUREMENTS")
  issues=character()
  if (!"Comparability_Class" %in% names(d))
    issues=c(issues,"Comparability_Class is absent; use imed_enrich_measurements() or imed_filter() on an imed_db.")
  else {
    cl=unique(as.character(d$Comparability_Class[.imed_nonmissing(d$Comparability_Class)]))
    if (length(cl)!=1) issues=c(issues,paste("Expected one comparability class; found",paste(cl,collapse=", ")))
    if (any(grepl("UNRESOLVED",cl,ignore.case=TRUE)))
      issues=c(issues,"Unresolved comparability class cannot enter strict synthesis.")
  }
  if ("Strict_Synthesis_Eligibility" %in% names(d)) {
    bad=grepl("^NO",as.character(d$Strict_Synthesis_Eligibility),ignore.case=TRUE)
    if (any(bad,na.rm=TRUE)) issues=c(issues,"Subset contains records explicitly ineligible for strict synthesis.")
  }
  if (require_single_unit && "Reported_Unit" %in% names(d)) {
    u=unique(as.character(d$Reported_Unit[.imed_nonmissing(d$Reported_Unit)]))
    if (length(u)>1) issues=c(issues,paste("Multiple reported units:",paste(u,collapse=", ")))
  }
  if ("Mass_Basis" %in% names(d)) {
    mass=grepl("/\\s*(kg|g)\\b",as.character(d$Reported_Unit),ignore.case=TRUE)
    if (any(mass,na.rm=TRUE)) {
      b=unique(as.character(d$Mass_Basis[mass & .imed_nonmissing(d$Mass_Basis)]))
      if (any(mass & !.imed_nonmissing(d$Mass_Basis),na.rm=TRUE))
        issues=c(issues,"Mass-normalized records include unresolved mass basis.")
      if (length(b)>1) issues=c(issues,paste("Mixed mass basis:",paste(b,collapse=", ")))
    }
  }
  ok=length(issues)==0
  diag=tibble::tibble(comparable=ok,issue=if(ok)"None" else issues)
  if (action=="return") return(diag)
  if (!ok && action=="error") .imed_stop("Unsafe analysis blocked:\n- ",paste(issues,collapse="\n- "))
  if (!ok && action=="warn") .imed_warn(paste(issues,collapse=" "))
  invisible(ok)
}

#' Create a manuscript/synthesis-ready analysis set
#'
#' This is the preferred entry point for quantitative analysis.
#' @param x An `imed_db` object, as returned by `imed_data()`.
#' @param comparability_class The single `Comparability_Class` to analyse (required).
#' @param matrix Optional `Matrix_Code` value(s) to keep.
#' @param mass_basis Optional `Mass_Basis` value(s) to keep.
#' @param metric_type `Metric_Type` to keep (default `"Abundance"`).
#' @param include_holds If `FALSE` (default), records flagged as scientific HOLDs are removed.
#' @export
imed_analysis_set <- function(x,comparability_class,matrix=NULL,mass_basis=NULL,
                              metric_type="Abundance",include_holds=FALSE) {
  d=imed_filter(x,matrix=matrix,comparability_class=comparability_class,
    mass_basis=mass_basis,metric_type=metric_type,include_holds=include_holds)
  imed_check_comparability(d,action="error")
  class(d)=c("imed_analysis_set",class(d))
  attr(d,"comparability_class")=comparability_class
  d
}
