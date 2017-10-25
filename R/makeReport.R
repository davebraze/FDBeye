##' @title Extract a fixation report from an ELascii object
##'
##' @description Extract a fixation report from an ELascii object
##'
##' @details Fixation reports include data from both eyes, if available, as well as subject ID,
##'     trial number, time stamp of first sample in each trial. By default, values for trial
##'     variables found in each trial are also include, although these may be suppressed.
##'
##' @param dat An object of class "ELascii", as created by readELascii().
##' @param trialvars Logical, defaults TRUE to include trial variables in fixation report.
##' @return A data.frame containing a fixation report with the following columns:
##'
##' \enumerate{
##'     \item subject: factor for subject ID, taken from the name of the source EDF file.
##'     \item trialn: factor for trial ID.
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
##' Returned data.frame will also include trial_vars from edf file if requested (TRUE by default).
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
##' @examples
##' fname <- system.file("/extdata/1950006-RAN.asc.gz", package="FDBeye")
##' e <- readELascii(fname)
##' efix <- reportFixations(e)
reportFixations <- function(dat, trialvars=TRUE) {
    if (!("ELascii" %in% class(dat))) {
        stop("Argument 'dat' must have class 'ELascii'.")
    }
    ## pull fixation details for each trial
    ## use id var name "trialn_" to avoid potential name conflicts with contents of data file.
    retval <- plyr::ldply(.data = dat$trials, .fun = function(x) {d <- x$fix},
                          .id = "trialn_")

    ## get time0 for each trial and merge it
    ## time0 is the time stamp on the first SAMPLE in a trial.
    ## x$samp[1,1] is crude, but fixing it requires first fixing contents of 'samp' list as built in readELascii()
    time0 <-  plyr::ldply(.data = dat$trials, .fun = function(x) {x$samp[1,1]},
                           .id = "trialn_")
    names(time0)[2] <- "time0"
    time0$time0 <- as.integer(time0$time0)
    retval <- dplyr::right_join(time0, retval,
                                by=c("trialn_" = "trialn_")) ## explicity 'by' to avoid accidental combinations

    if(trialvars) {        ## get trial variables for each trial and merge them
        tvs <- plyr::ldply(.data = dat$trials, .fun = function(x) {x$trialvar},
                           .id = "trialn_")
        retval <- dplyr::right_join(retval, tvs,
                                    by=c("trialn_" = "trialn_")) ## explicity 'by' to avoid accidental combinations
    }

    ## get subject ID from EDF source filename
    ## FIXME: strsplit() is ugly. Look for better option in stringr:: or stringi::
    subject <- strsplit(as.character(dat$session$srcfile), "[.]")[[1]][1]
    retval <- data.frame(subject, retval)

    ## cleanup
    if (sum(names(retval)=="trialn")==0) { ## check for a name collision before renaming 'trialn_'
        names(retval)[names(retval)=="trialn_"] <- "trialn"
    }

    retval
}

##' @title Extract a saccade report from an ELascii object
##'
##' @description Extract a saccade report from an ELascii object
##'
##' @details Saccade reports include data from both eyes, if available, as well as subject ID,
##'     trial number, time stamp of first sample in each trial. By default, values for trial
##'     variables found in each trial are also include, although these may be suppressed.
##'
##' @param dat An object of class "ELascii", as created by readELascii().
##' @param trialvars Logical, defaults TRUE to include trial variables in saccade report.
##' @return A data.frame containing a saccade report with the following columns:
##'
##' \enumerate{
##'     \item subject: factor for subject ID, taken from the name of the source EDF file.
##'     \item trialn: factor for trial ID.
##'     \item time0: integer indicating timestamp (ms) of first sample in recording block.
##'     \item event: factor for event type, always "SACC".
##'     \item eye: factor indicating eye for corresponding saccade, ranges over c("L", "R").
##'     \item stime: integer indicating start time for fixation (ms).
##'     \item etime: integer indicating end time for fixation (ms).
##'     \item dur: integer indicating duration of fixation (ms).
##'     \item xpos1: starting x position
##'     \item ypos1: starting y position
##'     \item xpos2: ending x position
##'     \item ypos2: ending y position
##'     \item ampl: amplitude (units?)
##'     \item peakvel: peak velocity (units?)
##' }
##'
##' Returned data.frame will also include trial_vars from edf file if requested (TRUE by default).
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
##' @examples
##' fname <- system.file("/extdata/1950006-RAN.asc.gz", package="FDBeye")
##' e <- readELascii(fname)
##' esacc <- reportSaccades(e)
reportSaccades <- function(dat, trialvars=TRUE) {
    if (!("ELascii" %in% class(dat))) {
        stop("Argument 'dat' must have class 'ELascii'.")
    }
    ## pull saccade details for each trial
    ## use id var name "trialn_" to avoid potential name conflicts with contents of data file.
    retval <- plyr::ldply(.data = dat$trials, .fun = function(x) {d <- x$sacc},
                          .id = "trialn_")

    ## get time0 for each trial and merge it
    ## time0 is the time stamp on the first SAMPLE in a trial.
    ## x$samp[1,1] is crude, but fixing it requires first fixing contents of 'samp' list as built in readELascii()
    time0 <-  plyr::ldply(.data = dat$trials, .fun = function(x) {x$samp[1,1]},
                           .id = "trialn_")
    names(time0)[2] <- "time0"
    time0$time0 <- as.integer(time0$time0)
    retval <- dplyr::right_join(time0, retval,
                                by=c("trialn_" = "trialn_")) ## explicity 'by' to avoid accidental combinations

    if(trialvars) {        ## get trial variables for each trial and merge them
        tvs <- plyr::ldply(.data = dat$trials, .fun = function(x) {x$trialvar},
                           .id = "trialn_")
        retval <- dplyr::right_join(retval, tvs,
                                    by=c("trialn_" = "trialn_")) ## explicity 'by' to avoid accidental combinations
    }

    ## get subject ID from EDF source filename
    ## FIXME: strsplit() is ugly. Look for better option in stringr:: or stringi::
    subject <- strsplit(as.character(dat$session$srcfile), "[.]")[[1]][1]
    retval <- data.frame(subject, retval)

    ## cleanup
    if (sum(names(retval)=="trialn")==0) { ## check for a name collision before renaming 'trialn_'
        names(retval)[names(retval)=="trialn_"] <- "trialn"
    }

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
