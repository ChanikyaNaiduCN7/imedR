
#' Publication-oriented abundance/distribution figure
#' @param x An `imed_db`, or a measurement-level data frame such as the output of
#'   `imed_analysis_set()`. The comparability guard is applied, so the
#'   records must belong to one compatible comparability class.
#' @param xvar Field to place on the x axis (unquoted or quoted name). Required for the
#'   box, violin and scatter types.
#' @param value Name of the numeric measurement field to plot (default `"Mean"`).
#' @param type Plot type: `"box"`, `"violin"`, `"scatter"` or `"histogram"`.
#' @param guard If `TRUE` (default), stop when the records fail the comparability check.
#' @export
imed_plot <- function(x,xvar=NULL,value="Mean",
                      type=c("box","violin","scatter","histogram"),guard=TRUE) {
  type=match.arg(type); d=.imed_table(x,"MEASUREMENTS")
  if (guard) imed_check_comparability(d,action="error")
  if (!value %in% names(d)) .imed_stop("Value field absent: ",value)
  d$.y=.imed_num(d[[value]])
  if (type=="histogram") return(ggplot2::ggplot(d,ggplot2::aes(x=.y))+
    ggplot2::geom_histogram(bins=30)+ggplot2::theme_classic()+ggplot2::labs(x=value,y="Count"))
  if (is.null(xvar)) .imed_stop("xvar is required.")
  xvar=rlang::as_name(rlang::ensym(xvar))
  p=ggplot2::ggplot(d,ggplot2::aes(x=.data[[xvar]],y=.y))
  p=if(type=="box") p+ggplot2::geom_boxplot()
    else if(type=="violin") p+ggplot2::geom_violin()+ggplot2::geom_boxplot(width=.15)
    else p+ggplot2::geom_point()
  p+ggplot2::theme_classic()+ggplot2::labs(x=xvar,y=value)
}

#' Plot polymer or particle-characteristic composition
#' @param what polymers or particles.
#' @param x An `imed_db` object, as returned by `imed_data()`.
#' @param study_id Optional character vector of `Study_ID` values used to restrict the records.
#' @param characteristic_type Optional character vector of `Characteristic_Type` values; used only when
#'   `what = "particles"`.
#' @export
imed_plot_composition <- function(x,what=c("polymers","particles"),study_id=NULL,
                                  characteristic_type=NULL) {
  what=match.arg(what)
  d=if(what=="polymers") .imed_table(x,"POLYMERS") else .imed_table(x,"PARTICLE_CHARACTERISTICS")
  if (!is.null(study_id)) d=d[d$Study_ID %in% study_id,,drop=FALSE]
  if (what=="particles" && !is.null(characteristic_type))
    d=d[d$Characteristic_Type %in% characteristic_type,,drop=FALSE]
  catcol=if(what=="polymers")"Polymer_Name" else "Category"
  d$.v=.imed_num(d$Value)
  d=d[is.finite(d$.v),,drop=FALSE]
  ggplot2::ggplot(d,ggplot2::aes(x=.data[[catcol]],y=.v))+
    ggplot2::geom_col()+ggplot2::coord_flip()+ggplot2::theme_classic()+
    ggplot2::labs(x=NULL,y="Reported value")
}

#' Plot method/QAQC reporting profile
#' @param x An `imed_db` object, as returned by `imed_data()`.
#' @export
imed_plot_methods <- function(x) {
  q=.imed_table(x,"METHODS_QAQC")
  fields=c("Procedural_Blank","Field_Blank","Airborne_Control","Filtered_Reagents",
    "Clean_Glassware","Nonplastic_Labware","Sample_Covered","Cotton_Labcoat",
    "Recovery_Test","Chemical_Confirmation")
  fields=intersect(fields,names(q))
  z=dplyr::bind_rows(lapply(fields,function(f) {
    tibble::tibble(field=f,status=as.character(q[[f]]))
  }))
  z$status[! .imed_nonmissing(z$status)]="Missing/NR"
  tab=dplyr::count(z,field,status)
  ggplot2::ggplot(tab,ggplot2::aes(x=field,y=n,fill=status))+
    ggplot2::geom_col()+ggplot2::coord_flip()+ggplot2::theme_classic()+
    ggplot2::labs(x=NULL,y="Studies/records",fill="Reported status")
}

#' Export a publication figure
#' @param plot A `ggplot` object, for example from `imed_plot()`.
#' @param filename Output file path; the format follows the file extension.
#' @param width Figure width in `units` (default 180).
#' @param height Figure height in `units` (default 120).
#' @param units Units for `width` and `height` (default `"mm"`).
#' @param dpi Resolution in dots per inch (default 600).
#' @export
imed_export_figure <- function(plot,filename,width=180,height=120,units="mm",dpi=600) {
  ggplot2::ggsave(filename,plot=plot,width=width,height=height,units=units,dpi=dpi)
  invisible(filename)
}
