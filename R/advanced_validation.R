#' Guard analytical size-window compatibility
#'
#' Uses linked SIZE_METHODS records to identify unresolved or heterogeneous
#' analytical/reporting lower bounds. It never substitutes mesh/filter pore
#' size for an analytical lower size limit.
#' @param x An imed_db.
#' @param sample_id Optional sample IDs.
#' @param action error, warn or return.
#' @export
imed_check_size_window <- function(x,sample_id=NULL,action=c("error","warn","return")) {
  action=match.arg(action)
  if (!inherits(x,"imed_db")) .imed_stop("Requires an imed_db.")
  z=x$SIZE_METHODS
  if (!is.null(sample_id)) z=z[z$Sample_ID %in% sample_id,,drop=FALSE]
  issues=character()
  al=.imed_num(z$Analytical_Lower_Limit_um)
  oc_col=.imed_col(z,c("Operational_Reporting_Cutoff_um","Operational_Cutoff_um"))
  oc=.imed_num(z[[oc_col]])
  known=unique(al[is.finite(al)])
  if (length(known)>1) issues=c(issues,paste("Multiple analytical lower limits:",paste(known,collapse=", "),"um"))
  if (any(!is.finite(al) & !is.finite(oc)))
    issues=c(issues,paste(sum(!is.finite(al)&!is.finite(oc)),"size-method record(s) have neither analytical lower limit nor operational cutoff."))
  ok=length(issues)==0
  out=tibble::tibble(compatible=ok,issue=if(ok)"None" else issues)
  if(action=="return") return(out)
  if(!ok && action=="error") .imed_stop("Size-window synthesis blocked:\n- ",paste(issues,collapse="\n- "))
  if(!ok && action=="warn") .imed_warn(paste(issues,collapse=" "))
  invisible(ok)
}

#' Sensitivity summary by evidence restriction
#'
#' Compares all eligible records with a stricter subset when evidence fields
#' are available. This is descriptive sensitivity analysis, not causal proof.
#' @param x An `imed_db`, or a measurement-level data frame such as the output of
#'   `imed_analysis_set()`. The comparability guard is applied, so the
#'   records must belong to one compatible comparability class.
#' @param value Name of the numeric measurement field to summarise (default `"Mean"`).
#' @export
imed_sensitivity <- function(x,value="Mean") {
  d=.imed_table(x,"MEASUREMENTS")
  imed_check_comparability(d,action="error")
  if(!value %in% names(d)) .imed_stop("Value field absent: ",value)
  y=.imed_num(d[[value]])
  base=tibble::tibble(stratum="All compatible",n=sum(is.finite(y)),
    mean=mean(y,na.rm=TRUE),median=stats::median(y,na.rm=TRUE))
  if("Strict_Synthesis_Eligibility" %in% names(d)) {
    keep=grepl("^YES",as.character(d$Strict_Synthesis_Eligibility),ignore.case=TRUE)
    yy=y[keep]
    strict=tibble::tibble(stratum="Strict synthesis eligible",n=sum(is.finite(yy)),
      mean=mean(yy,na.rm=TRUE),median=stats::median(yy,na.rm=TRUE))
    return(dplyr::bind_rows(base,strict))
  }
  base
}

#' Leave-one-study-out influence analysis for a guarded mean meta-analysis
#' @param x A guarded analysis set that passes `imed_meta_eligibility()`, as for
#'   `imed_meta_mean()`.
#' @param method Between-study variance estimator passed to `metafor` (default `"REML"`).
#' @export
imed_leave_one_out <- function(x,method="REML") {
  if(!requireNamespace("metafor",quietly=TRUE)) .imed_stop("Install 'metafor'.")
  fit=imed_meta_mean(x,method=method)
  z=metafor::leave1out(fit)
  tibble::as_tibble(as.data.frame(z),rownames="omitted")
}

#' Forest plot for an imedR meta-analysis
#' @param fit A fitted `metafor` `rma` object, for example from `imed_meta_mean()`.
#' @param ... Further arguments passed to `metafor::forest()`.
#' @export
imed_forest <- function(fit,...) {
  if(!inherits(fit,"rma")) .imed_stop("fit must be a metafor rma object.")
  metafor::forest(fit,...)
  invisible(fit)
}

#' Funnel plot for an imedR meta-analysis
#'
#' A funnel plot is diagnostic only and should not be interpreted as a direct
#' test of publication bias, especially with small or heterogeneous evidence.
#' @param fit A fitted `metafor` `rma` object, for example from `imed_meta_mean()`.
#' @param ... Further arguments passed to `metafor::funnel()`.
#' @export
imed_funnel <- function(fit,...) {
  if(!inherits(fit,"rma")) .imed_stop("fit must be a metafor rma object.")
  metafor::funnel(fit,...)
  invisible(fit)
}

#' Meta-regression with minimum-information guard
#'
#' Requires at least 10 independent effects by default and at least 5 effects
#' per model coefficient. These are guardrails, not guarantees of adequate power.
#' @param x A guarded analysis set that passes `imed_meta_eligibility()`.
#' @param moderators Character vector of field names in the measurement table to use as
#'   moderators.
#' @param min_k Minimum number of independent effects required (default 10). At least
#'   5 effects per model coefficient are also required.
#' @param method Between-study variance estimator passed to `metafor` (default `"REML"`).
#' @export
imed_meta_regression <- function(x,moderators,min_k=10,method="REML") {
  if(!requireNamespace("metafor",quietly=TRUE)) .imed_stop("Install 'metafor'.")
  d=.imed_table(x,"MEASUREMENTS")
  e=imed_meta_eligibility(d)
  if(!all(e$eligible)) .imed_stop("Meta-regression blocked by base eligibility.")
  moderators=as.character(moderators)
  miss=setdiff(moderators,names(d))
  if(length(miss)) .imed_stop("Moderator field(s) absent: ",paste(miss,collapse=", "))
  k=nrow(d); p=length(moderators)+1
  if(k<min_k || k<5*p) .imed_stop("Insufficient independent effects for guarded meta-regression: k=",k,", coefficients=",p,".")
  yi=.imed_num(d$Mean); vi=(.imed_num(d$SD)^2)/.imed_num(d$N)
  form=stats::as.formula(paste("~",paste(moderators,collapse="+")))
  metafor::rma.uni(yi=yi,vi=vi,mods=form,data=d,method=method,slab=d$Study_ID)
}

#' Validation report for manuscript/release audit
#'
#' Returns database integrity, HOLD burden, missingness and comparability counts.
#' @param x An `imed_db` object, as returned by `imed_data()`.
#' @export
imed_validation_report <- function(x) {
  if(!inherits(x,"imed_db")) .imed_stop("Requires an imed_db.")
  v=imed_validate(x)
  h=imed_flag_holds(x)
  cmp=x$MEASUREMENT_COMPARABILITY
  list(
    integrity=v,
    cardinalities=imed_tables(x),
    hold_summary=tibble::tibble(
      measurements=nrow(h),
      holds=sum(h$.imed_hold,na.rm=TRUE),
      eligible=nrow(h)-sum(h$.imed_hold,na.rm=TRUE)
    ),
    comparability=dplyr::count(cmp,Comparability_Class,Strict_Synthesis_Eligibility,sort=TRUE),
    measurement_missingness=imed_missingness(x,"MEASUREMENTS")
  )
}