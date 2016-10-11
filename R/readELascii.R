##' @title Called by readELascii()
##'
##' @description Used by readELascii(). Not intended for end-users.
##'
##' @details Used by readELascii(). Not intended for end-users. Extract fixations, saccades, blinks
##'     and other data from an individual trial.
##'
##' @param bounds A numeric tuple. e1 is index marking beginning of
##'     trial. e2 is index indicating end of trial.
##' @param lines A vector of strings, each corresponding to 1 line of
##'     the EL ASCII file.
##' @param msgSet A character vector of regular expressions to
##'     identify eyelink MSG lines to catch.
##' @return A list of 6 elements, data.frames enumerating fixations,
##'     saccades, blinks, TRIAL_VARs, samples and messages for the
##'     trial.
##' @author Dave Braze \email{davebraze@@gmail.com}
getEyelinkTrialData <- function(bounds,
                                lines,
                                msgSet=NA) {

    requireNamespace("FDButils", quietly = TRUE)


    ## TODO: For each trial build a header to include
    ## o start time of eye movement recording, (timestamp from START event)
    ## o eyes recorded, LEFT, RIGHT, BINOC (START, RECCFG, !MODE)
    ## o sample rate (RECCFG, !MODE)
    ## o display resolution (GAZE_COORDS)
    ## o what about these? THRESHOLDS, EFIT_PARAMS, ELLIPSE

    ## Get EVENTS meta-data. TODO: check that events line exists and that there is only 1.
    eventsLine <- grep("^EVENTS", lines[bounds[1]:bounds[2]], value=TRUE)
    ## eventsLine <- "EVENTS	GAZE	RIGHT	RES	RATE	 250.00	TRACKING	CR	FILTER	2"
    Egaze <- grepl("GAZE", eventsLine)
    Eres <- grepl("RES", eventsLine)
    ## Evel <- grepl("VEL", eventsLine) ## this flag not valid for EVENTS, SAMPLES only
    Eleft <- grepl("LEFT", eventsLine)
    Eright <- grepl("RIGHT", eventsLine)
    Ebinoc <- (Eleft && Eright)
    Erate <- unlist(stringr::str_split(stringr::str_extract(eventsLine, "RATE\\W+[0-9.]+"), "[ \t]+"))[2]
    ## Maybe also get tracking mode (pupil, cr) and filter level

    ## Get SAMPLES meta-data
    samplesLine <- grep("^SAMPLES", lines[bounds[1]:bounds[2]], value=TRUE)
    ## samplesLine <- "SAMPLES	GAZE	RIGHT	VEL	RES	RATE	 250.00	TRACKING	CR	FILTER	2"
    Sgaze <- grepl("GAZE", samplesLine)
    Sres <- grepl("RES", samplesLine)
    Starget <- grepl("HTARGET", samplesLine)
    Svel <- grepl("VEL", samplesLine) ## this flag not valid for EVENTS, SAMPLES only
    Sleft <- grepl("LEFT", samplesLine)
    Sright <- grepl("RIGHT", samplesLine)
    Sbinoc <- (Sleft && Sright)
    Srate <- unlist(stringr::str_split(stringr::str_extract(samplesLine, "RATE\\W+[0-9.]+"), "[ \t]+"))[2]

    ## There is also an HTARGET flag that adds columns (three, to SAMPLE lines. Definitely need to
    ## deal with that. HTARGET only occurs with remote systems in head-free mode. One of it's
    ## columns seems to be camera-to-target distance in mm. Not sure about the others. An INPUT flag
    ## also adds a single column.

    ## Maybe a better way to handle sample data is to use the flags in the samplesLine to build a
    ## header for sample df directly. So
    ## hdr <- "time"
    ## if (grepl("LEFT", samplesLine)
    ##     hdr <- c(hdr, c("xL", "yL", "pL"))
    ## if (grepl("RIGHT", samplesLine)
    ##     hdr <- c(hdr, c("xR", "yR", "pR"))
    ## And so on.

    ## Get fixation events
    fix <- grep("^EFIX", lines[bounds[1]:bounds[2]], value=TRUE)
    fix <- stringr::str_split(fix, pattern="[ \t]+")
    if (length(fix) > 0) {
        fix <- data.frame(matrix(unlist(fix), ncol=length(fix[[1]]), byrow=TRUE), stringsAsFactors=FALSE)
        toN <- sapply(fix, function(v) all(FDButils::isNumeral(v)))
        fix <- data.frame(sapply(fix[!toN], as.factor, simplify=FALSE), sapply(fix[toN], as.numeric, simplify=FALSE))
        ## TODO Catch case where xRes and yRes are included in the output. Set names appropriately.
        names(fix) <- c('event', 'eye', 'stime', 'etime', 'dur', 'xpos', 'ypos', 'pupil')
        fix$event <- gsub("^E", "", fix$event)
    } else {
        fix <- NULL
    }

    ## Get saccade events
    sacc <- grep("^ESACC", lines[bounds[1]:bounds[2]], value=TRUE)
    sacc <- stringr::str_split(sacc, pattern="[ \t]+")
    if (length(sacc) > 0) {
        sacc <- data.frame(matrix(unlist(sacc), ncol=length(sacc[[1]]), byrow=TRUE), stringsAsFactors=FALSE)
        toN <- sapply(sacc, function(v) all(FDButils::isNumeral(v)))
        sacc <- data.frame(sapply(sacc[!toN], as.factor, simplify=FALSE), sapply(sacc[toN], as.numeric, simplify=FALSE))
        names(sacc) <- c('event', 'eye', 'stime', 'etime', 'dur', 'xpos1', 'ypos1', 'xpos2', 'ypos2', 'ampl', 'peakvel')
        sacc$event <- gsub("^E", "", sacc$event)
    } else {
        sacc <- NULL
    }

    ## Get blink events
    blink <- grep("^EBLINK", lines[bounds[1]:bounds[2]], value=TRUE)
    blink <- stringr::str_split(blink, pattern="[ \t]+")
    if (length(blink) > 0) {
        blink <- data.frame(matrix(unlist(blink), ncol=length(blink[[1]]), byrow=TRUE), stringsAsFactors=FALSE)
        toN <- sapply(blink, function(v) all(FDButils::isNumeral(v)))
        blink <- data.frame(sapply(blink[!toN], as.factor, simplify=FALSE), sapply(blink[toN], as.numeric, simplify=FALSE))
        names(blink) <- c('event', 'eye', 'stime', 'etime', 'dur')
        blink$event <- gsub("^E", "", blink$event)
    } else {
        blink <- NULL
    }


    ## Get trial variables
    if(FALSE) {

        tvblock <- c("MSG	93985635 !V TRIAL_VAR cohort_set 19",  # test cases
                     "MSG	93985635 !V TRIAL_VAR condition NCohort",
                     "MSG	93985637 !V TRIAL_VAR frame see",
                     "MSG	93985637 !V TRIAL_VAR image_1 nail.bmp",
                     "MSG	93985638 !V TRIAL_VAR image_2 pencil.bmp",
                     "MSG	93985639 !V TRIAL_VAR image_3 wand.bmp",
                     "MSG	93985640 !V TRIAL_VAR image_4 church.bmp",
                     "MSG	93985641 !V TRIAL_VAR location_1 (737, 608)",
                     "MSG	93985643 !V TRIAL_VAR location_2 (337, 608)",
                     "MSG	93985644 !V TRIAL_VAR location_3 (737, 208)",
                     "MSG	93985645 !V TRIAL_VAR location_4 (337, 208)",
                     "MSG	93985645 !V TRIAL_VAR sound_1 pencil.wav",
                     "MSG	93985646 !V TRIAL_VAR sound_2 pencil_see.wav",
                     "MSG	93985647 !V TRIAL_VAR wordonset 603",
                     "MSG	93985648 !V TRIAL_VAR sound_1_len 2000",
                     "MSG	93985649 !V TRIAL_VAR sound_2_len 2500")

    }

    tvblock <- grep("TRIAL_VAR", lines[bounds[1]:bounds[2]], value=TRUE)
    trialvar <- stringr::str_split(tvblock, pattern="[ \t]+", n=6)
    if (length(trialvar) > 0) {
        trialvar <- matrix(unlist(trialvar), ncol=length(tvblock))[5:6,]
        hdr <- trialvar[1,]
        trialvar <- data.frame(rbind(trialvar[2,]), stringsAsFactors=FALSE)
        names(trialvar) <- hdr
        ## toN <- sapply(trialvar, function(v) all(FDButils::isNumeral(v)))  ## maybe wait to do the conversion to numeric when building reports
        ## trialvar <- data.frame(sapply(trialvar[!toN], as.factor, simplify=FALSE),
        ##                        sapply(trialvar[toN], as.numeric, simplify=FALSE))
    } else {
        trialvar <- NULL
    }

    ## TODO: Get sample level data put in separate list item (data.frame).
    ## Get samples
    samp <- grep("^[0-9]+", lines[bounds[1]:bounds[2]], value=TRUE)
    samp <- stringr::str_split(samp, pattern="[ \t]+")
    if (length(samp) > 0) {
        samp <- data.frame(matrix(unlist(samp), ncol=length(samp[[1]]), byrow=TRUE), stringsAsFactors=FALSE)
        print(samp[1,])
        ## NEED SOME ADDITIONAL HANDLING here to take care of '...' (when either left or right eye is
        ## not tracked) and similar composite fields
        ## Problem: fields in sample lines are different depending on
        ## o recording mode is 'remote' or 'head mounted'
        ## o eye being recorded is 'left', 'right' or 'binocular'
        ## o crossing those paramenters leads to 6 different configurations
        ## o For SAMPLE lines, there are 4 cases that need to be handled (not counting
        ##   optional velocity and resolution fields). See section 4.92 of EL1000+ user manual.
        ##   . binoc/HM recording, 8 fields (time, xposL, yposL, pupilL, xposR, yposR, pupilL, CR)
        ##   . monoc/HM recording, 5 fields (time, xpos, ypos, pupil, CR)
        ##   . binoc/remote recording, Not known at present
        ##   . monoc/remote recording, 9 fields (time, xpos, ypos, pupil, CR, xtarg, ytarg, ztarg (distance), IP field)

        if (!(Sbinoc||Svel||Sres)) {           ## monocular data; no velocity; no resolution
            print("!(Sbinoc||Svel||Sres)")
            ## For monocular data determine which eye was measured, label columns
            ## accordingly. Within a study using monocular recording, some subjects may contribute R
            ## eye data while others L eye data. If this is the case then coordinate and pupil size
            ## columns will be labelled differently for the two groups. Will need a helper function
            ## to deal with that. Perhaps just by renaming columns. But in a more complicated
            ## situation where, e.g., most subjects contributed binoculard data, but some have L eye
            ## only and some R eye only, we'll need a more sophisticated means of choosing which eye
            ## to use in analysis. One approach might be something like "prefer left" meaning take
            ## left eye measurements if available, otherwise use right. Other options, for binoc
            ## data would include "prefer best" where we choose the better of R or L eye data
            ## according to some rubric, or "collapse" were we take the average of L & R for each
            ## time point.
            ##
            ## MAYBE: Also insert columns for unmeasured eye and fill them with NAs.

            if (Sleft) {
                ## 5 columns <time> <xpl> <ypl> <psl> ...  5th column (...) is only present in
                ## corneal reflection mode. This warning field is "..." if no warnings. First
                ## character is "I" if sample was interpolated. Second characters is "C" if CR is
                ## missing. Third character is "R" if CR recovery in progress. See page 122 of
                ## EyeLink 1000 plus Manual v1.0.6.
            } else {
                ## 5 columns        <time> <xpr> <ypr> <psr> ...
            }


        } else if(Svel && !(Sbinoc||Sres)) {   ## monocular; velocity; no resolution
            print("Svel && !(Sbinoc||Sres)")
            if (Sleft) {
                ## 7 columns        <time> <xpl> <ypl> <psl> <xv> <yv> ...
            } else {
                ## 7 columns        <time> <xpr> <ypr> <psr> <xv> <yv> ...
            }

        } else if(Sres && !(Sbinoc||Svel)) {   ## monocular; no velocity;  resolution
            print("Sres && !(Sbinoc||Svel)")
            if (Sleft) {
                ## 7 columns        <time> <xpl> <ypl> <psl> <xr> <yr> ...
            } else {
                ## 7 columns        <time> <xpr> <ypr> <psr> <xr> <yr> ...
            }

        } else if((Svel&&Sres) && !Sbinoc) {   ## monocular; velocity;  resolution
            print("(Svel&&Sres) && !Sbinoc")
            if (Sleft) {
                ## 7 columns        <time> <xpl> <ypl> <psl> <xv> <yv> <xr> <yr> ...
            } else {
                ## 7 columns        <time> <xpr> <ypr> <psr> <xv> <yv> <xr> <yr> ...
            }

        } else if (Sbinoc && !(Svel||Sres)) {  ## binocular data; no velocity; no resolution
            print("Sbinoc && !(Svel||Sres)")
            ## 8 columns        <time> <xpl> <ypl> <psl> <xpr> <ypr> <psr> ...
            ##
            ## 8th column (.....) is only present in corneal reflection mode. This warning field is
            ## "....." if no warnings. First character is "I" if sample was interpolated. Second
            ## characters is "C" if LEFT CR is missing. Third character is "R" if LEFT CR recovery
            ## in progress. Fourth characters is "C" if RIGHT CR is missing. Fifth character is "R"
            ## if RIGHT CR recovery in progress. See page 122 of EyeLink 1000 plus Manual v1.0.6.
        } else if((Sbinoc && Svel) && !Sres) { ## binocular; velocity; no resolution
            print("(Sbinoc && Svel) && !Sres")
            ## 10 columns        <time> <xpl> <ypl> <psl> <xpr> <ypr> <psr> <xv> <yv> ...
        } else if(Sres && !(Sbinoc||Svel)) {   ## binocular; no velocity;  resolution
            print("Sres && !(Sbinoc||Svel)")
            ## 10 columns        <time> <xpl> <ypl> <psl> <xpr> <ypr> <psr> <xr> <yr> ...
        } else if((Svel&&Sres) && !Sbinoc) {   ## binocular; velocity;  resolution
            print("(Svel&&Sres) && !Sbinoc")
            ## 12 columns        <time> <xpl> <ypl> <psl> <xpr> <ypr> <psr> <xv> <yv> <xr> <yr> ...
        }

    } else {
        samp <- NULL
    }

    ## Get message events
    if(length(msgSet)>1 || !is.na(msgSet)) {
        ## All messages caught by this routine should have the same number of fields, or else.
        ## TODO: add error check for field count.
        ## TODO: add code to pick up groups of msgs, where across groups the field count is different.
        ##  1. Set of REs, each uniquely matches line in trial (e.g., "ARECSTART")
        ##  2. For each matched line, get timestamp, and offset if present. If no offset, set offset to 0.
        ##  3. Get label/event-type (e.g., ARECSTART).
        ##  4. Get value if present, otherwise set value to NA.
        msgRE <- paste0("^MSG.*(", paste0(msgSet, "", collapse="|"), ")") ## FIXME: Don't paste the REs together. Handle them 1 at a time.
        msg <- grep(msgRE, lines[bounds[1]:bounds[2]], value=TRUE)
        msg <- stringr::str_split(msg, pattern="[ \t]+")
        if (length(msg) > 0) {
            msg <- data.frame(matrix(unlist(msg), ncol=length(msg[[1]]), byrow=TRUE), stringsAsFactors=FALSE)
        } else {
            msg <- NULL
        }
    } else {
        msg <- NULL
    }

    retval <- list(fix=fix, sacc=sacc, blink=blink, trialvar=trialvar, samp=samp, msg=msg)
    retval
}

##' @title Get events from SR Research ASCII data files.
##'
##' @description Convenience function to call SRR utility edf2asc from
##'     inside R.
##'
##' @details SR Research provides a utility (EDF2ASC.exe) that dumps
##'     ASCII renderings of their proprietary EDF data file
##'     format. This function reads those ASCII files and extracts
##'     eye-movement events (fixations, saccades, blinks), specified
##'     MSG events, and TRIAL_VARs from them.
##'
##' @param file A string giving path/fname to input file (ELascii
##'     file).
##' @param tStartRE A string containing regular expression that
##'     uniquely identifies beginnings of trials.
##' @param tEndRE A string containing regular expression that uniquely
##'     identifies ends of trials. If an experiment is aborted
##'     prematurely, then the *edf file (and so the *asc file) may not
##'     have a proper trial end event. TODO: test for that case and
##'     handle it while throwing a warning.
##' @param msgSet A character vector. Each element identifies a MSG
##'     event to recover from the data file.
##' @param subjID If NULL (default), use filename as subject
##'     ID. Otherwise use specified string.
##' @return List with two elements, one for session information, and
##'     one containing a list of trials. Each trial element is itself
##'     a list of 6 elements: data.frames enumerating fixations,
##'     saccades, blinks, samples, TRIAL_VARs and MSGs for the trial.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
readELascii <- function(file,
                        tStartRE="TRIALID",
                        tEndRE="TRIAL_RESULT",
                        msgSet=NA,
                        subjID=NULL) {
    ## TODO: maybe change default tStartRE to "Prepare_sequence"
    f <- file(file, "r", blocking=FALSE)
    lines <- readLines(f, warn=TRUE, n=-1)
    close(f)

    ## use filename as subject ID, unless otherwise specified
    if(is.null(subjID)) {
        subj <- unlist(stringr::str_split(tools::file_path_sans_ext(file), .Platform$file.sep))
        subj <- subj[length(subj)]
    } else {
        subj <- subjID
    }

    ## get session information from file header
    ## FIXME: Also need to capture version of edfapi/edf2asc used for file conversion.
    header <- grep("^[*][*] ", lines, value=TRUE)
    script <- unlist(stringr::str_split(grep("RECORDED BY", header, value=TRUE), "[ \t]+"))[4]
    sessdate <- unlist(stringr::str_split(grep("DATE:", header, value=TRUE), ": "))[2]
    srcfile <- unlist(stringr::str_split(grep("CONVERTED FROM", header, value=TRUE), " (FROM|using) "))[2]
    srcfile <- basename(srcfile)
    session <- data.frame(subj, script, sessdate, srcfile)

    ## get start and end lines for each trial block
    tStart <- grep(tStartRE, lines)
    tEnd <- grep(tEndRE, lines)
    stopifnot (length(tStart) == length(tEnd)) ##TODO: something more elegant of there's a mismatch here.
    trialidx <- cbind(tStart, tEnd)

    ## get trial IDs
    trialids <- unlist(stringr::str_split(grep("TRIALID", lines, value=TRUE), " TRIALID "))
    trialids <- trialids[seq(2, length(trialids), 2)]

    ## get events for each trial
    trials <- apply(trialidx, 1, getEyelinkTrialData, lines=lines, msgSet=msgSet)
    names(trials) <- trialids

    retval <- list(session=session, trials=trials)
    class(retval) <- c("ELascii", class(retval))
    retval
}


if(FALSE) {
    fname <- system.file("/extdata/1950006-RAN.asc.gz", package="FDBeye")
    ## TODO: need to compress (gzip??) the ELascii files provided with this package. readLines()
    ## seems to handle gzipped files transparently.

#    debug(readELascii)
    e <- readELascii(fname)
#    undebug(readELascii)

    names(e$session)
    names(e$trials)
    names(e$trials$'2')
    head(e$trials$'2'$fix)
    head(e$trials$'2'$sacc)
    head(e$trials$'2'$blink)
    dim(e$trials$'2'$trialvar)
    names(e$trials$'2'$trialvar)

}

