##' @title Make a fixation report from an ELascii object
##'
##' @description No description yet.
##'
##' @details
##' No details yet.
##'
##' @param gaze An object of class "ELascii", as created by readELascii().
##' @return A data.frame containing a fixation report with the following columns:
##'
##' \enumerate{
##'     \item subject: factor for subject ID, taken from the name of the source EDF file.
##'     \item trial: factor for trial ID.
##'     \item time0: integer indicating timestamp (ms) of first sample in recording block.
##'     \item event: factor for event type, always "FIX".
##'     \item eye: factor indicating eye for corresponding fixation, ranges over c("L", "R").
##'     \item stime: integer indicating start time for fixation (ms).
##'     \item etime: integer indicating end time for fixation (ms).
##'     \item dur: integer indicating duration of fixation (ms).
##'     \item xpos: integer, x position of fixation in screen coordinates (pixels).
##'     \item ypos: integer, y position of fixation in screen coordinates (pixels). Origin is upper left corner of screen.
##'     \item pupil: integer, pupil size (pixels), may be either area or radius depending on ET settings
##' }
##'
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
##' @examples
##' fname <- system.file("/extdata/1950006-RAN.asc.gz", package="FDBeye")
##' e <- readELascii(fname)
##' efix <- fixReport(e)
fixReport <- function(gaze) {
    if (!("ELascii" %in% class(gaze))) {
        stop("Argument 'gaze' must have class 'ELascii'.")
    }
    ##
    ## time0 is the time stamp on the first SAMPLE in a trial.
    ## x$samp[1,1] is crude, but fixing it requires first fixing contents of 'samp' list as built in readELascii()
    time0 <-  plyr::ldply(.data = gaze$trials, .fun = function(x) {as.integer(x$samp[1,1])},
                           .id = "trial")
    names(time0) <- c("trial", "time0")
    ##
    ## get subject ID from EDF source filename
    ## FIXME: strsplit() is ugly. Look for better option in stringr:: or stringi::
    subject <- strsplit(as.character(gaze$session$srcfile), "[.]")[[1]][1]
    ##
    ## pull fixation details and merge time0 and subject ID
    retval <- plyr::ldply(.data = gaze$trials, .fun = function(x) {d <- x$fix},
                          .id = "trial")
    ## FIXME: event column is pulled in as type 'character'. It should be 'factor'. Fix this in readELascii().
    retval <- dplyr::right_join(time0, retval)
    retval <- data.frame(subject, retval)
    retval
}

##' @title Generate an ET report, as data.frame.
##'
##' @description No description yet.
##'
##' @details
##' No details yet.
##'
##' @param gaze An object of class "ELascii".
##' @param type Which type of base report to generate. Ranges over c("FIX", "SACC", "TRIALVAR").
##' @return A data.frame containing the requested report.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
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
