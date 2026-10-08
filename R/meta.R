
#' Evaluate eligibility for quantitative evidence synthesis
#'
#' Checks class/unit/basis, usable uncertainty, and study-level independence.
#' @param require_independent_studies If TRUE, duplicate Study_ID is flagged.
#' @param x An `imed_db`, or a measurement-level data frame such as the output of
#'   `imed_analysis_set()`. The comparability guard is applied, so the
#'   records must belong to one compatible comparability class.
#' @export
imed_meta_eligibility <- function(x,require_independent_studies=TRUE) {
  d=.imed_table(x,"MEASUREMENTS")
  cmp=imed_check_comparability(d,action="return")
  issues=if (all(cmp$comparable)) character() else cmp$issue
  if (!all(c("Mean","SD","N") %in% names(d)))
    issues=c(issues,"Mean, SD and N fields are required for mean-based meta-analysis.")
  else {
    ok=is.finite(.imed_num(d$Mean)) & is.finite(.imed_num(d$SD)) & is.finite(.imed_num(d$N)) & .imed_num(d$N)>1
    if (!all(ok)) issues=c(issues,paste(sum(!ok),"record(s) lack usable Mean/SD/N."))
  }
  if (require_independent_studies) {
    if (!"Study_ID" %in% names(d)) issues=c(issues,"Study_ID absent; independence cannot be assessed.")
    else if (any(duplicated(d$Study_ID[.imed_nonmissing(d$Study_ID)])))
      issues=c(issues,"Multiple effect records occur within at least one study; select/aggregate a defensible independent effect before standard meta-analysis.")
  }
  tibble::tibble(eligible=length(issues)==0,issue=if(length(issues))issues else "None")
}

#' Random-effects meta-analysis of compatible raw means
#'
#' Intended only for one comparable outcome/unit/basis with independent study
#' effects. Requires metafor.
#' @param x An `imed_db`, or a measurement-level data frame such as the output of
#'   `imed_analysis_set()`. The comparability guard is applied, so the
#'   records must belong to one compatible comparability class.
#' @param method Between-study variance estimator passed to `metafor` (default `"REML"`).
#' @export
imed_meta_mean <- function(x,method="REML") {
  if (!requireNamespace("metafor",quietly=TRUE)) .imed_stop("Install 'metafor' to run meta-analysis.")
  d=.imed_table(x,"MEASUREMENTS")
  e=imed_meta_eligibility(d)
  if (!all(e$eligible)) .imed_stop("Meta-analysis blocked:\n- ",paste(e$issue,collapse="\n- "))
  yi=.imed_num(d$Mean); vi=(.imed_num(d$SD)^2)/.imed_num(d$N)
  fit=metafor::rma.uni(yi=yi,vi=vi,method=method,slab=d$Study_ID)
  fit
}

#' Random-effects meta-analysis of prevalence
#'
#' Uses BIOTA_CONTEXT N_Positive/N_Examined and a logit transformed proportion.
#' Multiple rows per Study_ID are blocked unless the user preselects one
#' independent effect per study.
#' @param x An `imed_db` (required if `study_id` is used) or a `BIOTA_CONTEXT`-style
#'   data frame with `N_Positive` and `N_Examined`.
#' @param study_id Optional character vector of `Study_ID` values used to restrict the records.
#' @param method Between-study variance estimator passed to `metafor` (default `"REML"`).
#' @export
imed_meta_prevalence <- function(x,study_id=NULL,method="REML") {
  if (!requireNamespace("metafor",quietly=TRUE)) .imed_stop("Install 'metafor' to run meta-analysis.")
  b=.imed_table(x,"BIOTA_CONTEXT")
  if (!is.null(study_id)) {
    # derive Study_ID through samples if database object supplied
    if (!inherits(x,"imed_db")) .imed_stop("study_id filtering requires an imed_db.")
    b=dplyr::left_join(b,x$SAMPLES[,c("Sample_ID","Study_ID")],by="Sample_ID")
    b=b[b$Study_ID %in% study_id,,drop=FALSE]
  } else if (inherits(x,"imed_db")) {
    b=dplyr::left_join(b,x$SAMPLES[,c("Sample_ID","Study_ID")],by="Sample_ID")
  }
  ok=is.finite(.imed_num(b$N_Examined)) & is.finite(.imed_num(b$N_Positive)) &
    .imed_num(b$N_Examined)>0
  b=b[ok,,drop=FALSE]
  if (!nrow(b)) .imed_stop("No usable N_Positive/N_Examined records.")
  if ("Study_ID" %in% names(b) && any(duplicated(b$Study_ID)))
    .imed_stop("Multiple prevalence records occur within a study. Preselect one independent effect or use an appropriate multilevel model.")
  esc=metafor::escalc(measure="PLO",xi=.imed_num(b$N_Positive),ni=.imed_num(b$N_Examined),data=b)
  metafor::rma.uni(yi=esc$yi,vi=esc$vi,method=method,slab=if("Study_ID"%in%names(b))b$Study_ID else b$Sample_ID)
}
