
#' Validate IMED core scientific and relational integrity
#' @param x An imed_db.
#' @param strict Stop on ERROR-level failures.
#' @export
imed_validate <- function(x,strict=FALSE) {
  checks <- list()
  add <- function(check,severity,passed,n=0L,detail="") {
    checks[[length(checks)+1L]] <<- tibble::tibble(check=check,severity=severity,
      passed=isTRUE(passed),n_issues=as.integer(n),detail=detail)
  }
  keys <- c(STUDIES="Study_ID",SITES="Site_ID",SAMPLES="Sample_ID",
    MEASUREMENTS="Measurement_ID",SIZE_METHODS="Size_Method_ID",
    PARTICLE_CHARACTERISTICS="Characteristic_ID",POLYMERS="Polymer_Record_ID",
    BIOTA_CONTEXT="Biota_ID",METHODS_PROCESSING="Method_ID",METHODS_QAQC="QAQC_ID",
    DERIVED_METRICS="Derived_ID",RISK_ASSESSMENTS="Risk_ID",
    SOURCE_DISCREPANCIES="Discrepancy_ID",PROVENANCE="Provenance_ID")
  for (nm in intersect(names(keys),names(x))) {
    k <- keys[[nm]]; z <- x[[nm]][[k]]
    ndup <- sum(duplicated(z[.imed_nonmissing(z)]))
    nmiss <- sum(!.imed_nonmissing(z))
    add(paste0("PK_DUP_",nm),"ERROR",ndup==0,ndup,paste("Duplicate",k))
    add(paste0("PK_MISSING_",nm),"ERROR",nmiss==0,nmiss,paste("Missing",k))
  }
  fk <- list(
    c("SITES","Study_ID","STUDIES","Study_ID"),
    c("SAMPLES","Study_ID","STUDIES","Study_ID"),
    c("SAMPLES","Site_ID","SITES","Site_ID"),
    c("MEASUREMENTS","Sample_ID","SAMPLES","Sample_ID"),
    c("SIZE_METHODS","Sample_ID","SAMPLES","Sample_ID"),
    c("BIOTA_CONTEXT","Sample_ID","SAMPLES","Sample_ID"),
    c("METHODS_PROCESSING","Study_ID","STUDIES","Study_ID"),
    c("METHODS_QAQC","Study_ID","STUDIES","Study_ID"),
    c("DERIVED_METRICS","Study_ID","STUDIES","Study_ID"),
    c("RISK_ASSESSMENTS","Study_ID","STUDIES","Study_ID"),
    c("SOURCE_DISCREPANCIES","Study_ID","STUDIES","Study_ID"),
    c("PROVENANCE","Study_ID","STUDIES","Study_ID"),
    c("MEASUREMENT_COMPARABILITY","Measurement_ID","MEASUREMENTS","Measurement_ID"),
    c("SITE_COORDINATE_CLASS","Site_ID","SITES","Site_ID")
  )
  for (r in fk) if (all(c(r[1],r[3]) %in% names(x))) {
    a=x[[r[1]]][[r[2]]]; b=x[[r[3]]][[r[4]]]
    bad=.imed_nonmissing(a) & !a %in% b
    add(paste0("FK_",r[1],"_",r[2]),"ERROR",!any(bad),sum(bad),
        paste(r[1],r[2],"not found in",r[3],r[4]))
  }
  if ("STUDIES" %in% names(x)) {
    doi=tolower(trimws(as.character(x$STUDIES$DOI)))
    doi=doi[.imed_nonmissing(doi)]
    n=sum(duplicated(doi))
    add("DOI_COLLISION","ERROR",n==0,n,"Duplicate non-missing DOI")
  }
  if ("SITES" %in% names(x)) {
    la=.imed_num(x$SITES$Latitude); lo=.imed_num(x$SITES$Longitude)
    bad=(!is.na(la)&(la < -90|la > 90))|(!is.na(lo)&(lo < -180|lo > 180))
    add("COORDINATE_RANGE","ERROR",!any(bad),sum(bad),"Invalid latitude/longitude")
  }
  if ("MEASUREMENT_COMPARABILITY" %in% names(x)) {
    c=x$MEASUREMENT_COMPARABILITY
    hold=grepl("^NO",as.character(c$Strict_Synthesis_Eligibility),ignore.case=TRUE)
    add("STRICT_SYNTHESIS_HOLDS","HOLD",!any(hold),sum(hold),
        "Records explicitly marked non-eligible for strict synthesis")
    unresolved=grepl("UNRESOLVED",as.character(c$Comparability_Class),ignore.case=TRUE)
    add("UNRESOLVED_COMPARABILITY","HOLD",!any(unresolved),sum(unresolved),
        "Unresolved comparability-class records")
  }
  out=dplyr::bind_rows(checks)
  if (strict && any(!out$passed & out$severity=="ERROR"))
    .imed_stop("Validation ERROR(s) detected; inspect imed_validate().")
  out
}

#' Flag measurement-level scientific HOLDs
#' @param x An `imed_db`, or a measurement-level data frame that includes comparability
#'   metadata.
#' @export
imed_flag_holds <- function(x) {
  if (inherits(x,"imed_db")) d=imed_enrich_measurements(x) else d=tibble::as_tibble(x)
  d$.imed_hold=FALSE; d$.imed_hold_reason=NA_character_
  if ("Strict_Synthesis_Eligibility" %in% names(d)) {
    bad=grepl("^NO",as.character(d$Strict_Synthesis_Eligibility),ignore.case=TRUE)
    bad[is.na(bad)]=FALSE
    d$.imed_hold[bad]=TRUE
    d$.imed_hold_reason[bad]=as.character(d$Reason[bad])
  }
  unresolved=grepl("UNRESOLVED",as.character(d$Comparability_Class),ignore.case=TRUE)
  unresolved[is.na(unresolved)]=FALSE
  d$.imed_hold[unresolved]=TRUE
  d$.imed_hold_reason[unresolved]="Unresolved scientific comparability/mass basis"
  d
}
