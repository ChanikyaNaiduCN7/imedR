
#' Descriptive statistics for a guarded analysis set
#' @param x An `imed_db`, or a measurement-level data frame such as the output of
#'   `imed_analysis_set()`. The comparability guard is applied, so the
#'   records must belong to one compatible comparability class.
#' @param value Name of the numeric measurement field to summarise (default `"Mean"`).
#' @param group Optional grouping field (unquoted or quoted name).
#' @export
imed_describe <- function(x,value="Mean",group=NULL) {
  d=.imed_table(x,"MEASUREMENTS")
  imed_check_comparability(d,action="error")
  if (!value %in% names(d)) .imed_stop("Value field absent: ",value)
  d$.y=.imed_num(d[[value]])
  summarise_one=function(z) tibble::tibble(
    n=sum(is.finite(z$.y)),mean=mean(z$.y,na.rm=TRUE),sd=stats::sd(z$.y,na.rm=TRUE),
    median=stats::median(z$.y,na.rm=TRUE),q1=stats::quantile(z$.y,.25,na.rm=TRUE,names=FALSE),
    q3=stats::quantile(z$.y,.75,na.rm=TRUE,names=FALSE),min=min(z$.y,na.rm=TRUE),max=max(z$.y,na.rm=TRUE))
  if (is.null(group)) return(summarise_one(d))
  group=rlang::as_name(rlang::ensym(group))
  grouped <- dplyr::group_by(d, .data[[group]])
  modified <- dplyr::group_modify(grouped, ~summarise_one(.x))
  dplyr::ungroup(modified)
}

#' Grouped comparison summary
#' @param x An `imed_db`, or a measurement-level data frame such as the output of
#'   `imed_analysis_set()`. The comparability guard is applied, so the
#'   records must belong to one compatible comparability class.
#' @param group Grouping field (unquoted or quoted name).
#' @param value Name of the numeric measurement field to summarise (default `"Mean"`).
#' @export
imed_compare <- function(x,group,value="Mean") imed_describe(x,value=value,group={{group}})

#' Conservative inferential statistics
#'
#' Tests are explicit, never auto-selected from p-values or normality alone.
#' @param x An `imed_db`, or a measurement-level data frame such as the output of
#'   `imed_analysis_set()`. The comparability guard is applied, so the
#'   records must belong to one compatible comparability class.
#' @param value Name of the numeric measurement field to analyse (default `"Mean"`).
#' @param group Grouping field (unquoted or quoted name); required when `test` is not
#'   `"none"`.
#' @param test Test to run: `"none"` (default), `"welch"`, `"wilcoxon"` or `"kruskal"`.
#'   The test is never chosen automatically.
#' @export
imed_stats <- function(x,value="Mean",group=NULL,
                       test=c("none","welch","wilcoxon","kruskal")) {
  test=match.arg(test); d=.imed_table(x,"MEASUREMENTS")
  imed_check_comparability(d,action="error")
  y=.imed_num(d[[value]])
  out=list(descriptive=imed_describe(d,value=value))
  finite=is.finite(y)
  if (sum(finite)>=3 && sum(finite)<=5000) {
    sw=stats::shapiro.test(y[finite])
    out$normality=tibble::tibble(test="Shapiro-Wilk",W=unname(sw$statistic),p_value=sw$p.value,
      note="Diagnostic only; do not choose tests solely from this p-value.")
  }
  if (test!="none") {
    if (is.null(group)) .imed_stop("group is required.")
    gname=rlang::as_name(rlang::ensym(group)); g=as.factor(d[[gname]])
    keep=finite & !is.na(g); gg=droplevels(g[keep])
    if (test %in% c("welch","wilcoxon") && nlevels(gg)!=2) .imed_stop(test," requires exactly two groups.")
    z=if(test=="welch") stats::t.test(y[keep]~gg)
      else if(test=="wilcoxon") stats::wilcox.test(y[keep]~gg,exact=FALSE)
      else stats::kruskal.test(y[keep]~gg)
    out$test=tibble::tibble(method=z$method,statistic=unname(z$statistic),p_value=z$p.value)
  }
  class(out)=c("imed_stats","list"); out
}

#' Missingness profile for a table
#' @param x An `imed_db` object, as returned by `imed_data()`.
#' @param table Name of the table to profile (default `"MEASUREMENTS"`).
#' @export
imed_missingness <- function(x,table="MEASUREMENTS") {
  d=.imed_table(x,table)
  tibble::tibble(field=names(d),n=nrow(d),
    missing=vapply(d,function(z)sum(!.imed_nonmissing(z)),integer(1)),
    missing_pct=100*vapply(d,function(z)mean(!.imed_nonmissing(z)),numeric(1)))
}
