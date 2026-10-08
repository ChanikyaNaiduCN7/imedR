
#' Primary study citations for a subset
#' @param x An `imed_db` object, as returned by `imed_data()`.
#' @param study_id Optional character vector of `Study_ID` values used to restrict the records.
#' @export
imed_cite <- function(x,study_id=NULL) {
  s=.imed_table(x,"STUDIES")
  if (!is.null(study_id)) s=s[s$Study_ID %in% study_id,,drop=FALSE]
  s[,intersect(c("Study_ID","Authors","Year","Title","Journal","DOI"),names(s)),drop=FALSE]
}

#' Provenance records for studies or target records
#' @param x An `imed_db` object, as returned by `imed_data()`.
#' @param study_id Optional character vector of `Study_ID` values used to restrict the records.
#' @param record_id Optional character vector of `Target_Record_ID` values to retrieve.
#' @export
imed_provenance <- function(x,study_id=NULL,record_id=NULL) {
  p=.imed_table(x,"PROVENANCE")
  if (!is.null(study_id)) p=p[p$Study_ID %in% study_id,,drop=FALSE]
  if (!is.null(record_id)) p=p[p$Target_Record_ID %in% record_id,,drop=FALSE]
  tibble::as_tibble(p)
}

#' Study-characteristics table for manuscripts
#' @param x An `imed_db` object, as returned by `imed_data()`.
#' @param study_id Optional character vector of `Study_ID` values used to restrict the records.
#' @export
imed_manuscript_table <- function(x,study_id=NULL) {
  s=.imed_table(x,"STUDIES")
  if (!is.null(study_id)) s=s[s$Study_ID %in% study_id,,drop=FALSE]
  keep=intersect(c("Study_ID","Authors","Year","States_UTs","Matrices_Studied",
    "Journal","DOI","Entry_Status"),names(s))
  tibble::as_tibble(s[,keep,drop=FALSE])
}

#' Tidy a statistical result object for manuscript reporting
#' @param x A result object: an `imed_stats` result or a fitted `metafor` `rma` object.
#' @export
imed_results_table <- function(x) {
  if (inherits(x,"imed_stats")) return(dplyr::bind_rows(
    lapply(names(x),function(nm) {
      z=x[[nm]]
      if (is.data.frame(z)) dplyr::mutate(z,section=nm,.before=1) else NULL
    })))
  if (inherits(x,"rma")) return(tibble::tibble(
    estimate=as.numeric(x$b),se=x$se,ci_lb=x$ci.lb,ci_ub=x$ci.ub,
    z=x$zval,p_value=x$pval,tau2=x$tau2,I2=x$I2,H2=x$H2,k=x$k))
  .imed_stop("Unsupported result object.")
}

#' Evidence-quality/reporting profile
#' @param x An `imed_db` object, as returned by `imed_data()`.
#' @export
imed_evidence_profile <- function(x) {
  q=.imed_table(x,"METHODS_QAQC")
  tibble::tibble(
    n_records=nrow(q),
    procedural_blank_yes=sum(tolower(as.character(q$Procedural_Blank))=="yes",na.rm=TRUE),
    recovery_test_reported=sum(.imed_nonmissing(q$Recovery_Test) &
      !tolower(as.character(q$Recovery_Test)) %in% c("not reported","nr"),na.rm=TRUE),
    chemical_confirmation_reported=sum(.imed_nonmissing(q$Chemical_Confirmation),na.rm=TRUE)
  )
}
