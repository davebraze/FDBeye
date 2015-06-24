##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Generate an ET report, as data.frame.
##' @param gaze An object of class "ELascii".
##' @param type Which type of base report to generate. Ranges over c("FIX", "SACC", "TRIALVAR").
##' @return A data.frame containing the requested report.
##' @author Dave Braze \email{davebraze@@gmail.com}
makeReport <- function(gaze, type=c("FIX", "SACC", "TRIALVAR")) {
    type <- match.arg(type)
    if (!("ELascii" %in% class(gaze))) {
        stop("Argument 'gaze' must have class 'ELascii'.")
    }

    if ("FIX" == type) {
        fixLists <- lapply (gaze, function(ll) {ll$fix})
        trials <- names(fixLists)
        for (ff in fixLists) {
            if(!exists("retval")) retval <- ff
            else retval <- rbind(retval, ff)
        }
    } else if ("SACC" == type) {
    } else if ("TRIALVAR" == type) {
    } else {
        retval <- NULL
    }
    retval
}
