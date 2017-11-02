## TODO: It will be worth looking at other tools with similar goals to
## these functions, listed here:
## https://github.com/davebraze/FDBeye/issues/45

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
##' @return A list of 6 elements, data.frames enumerating fixations,
##'     saccades, blinks, TRIAL_VARs, samples and messages for the
##'     trial.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @author Monica Li \email{monica.yc.li@@gmail.com}
getEyelinkTrialData <- function(bounds,
                                lines) {

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
    Starget <- grepl("HTARGET", samplesLine) # remote recording mode
    Scr <- grepl("CR", samplesLine) # CR recording mode
    Svel <- grepl("VEL", samplesLine) ## this flag not valid for EVENTS, SAMPLES only
    Sleft <- grepl("LEFT", samplesLine)
    Sright <- grepl("RIGHT", samplesLine)
    Sbinoc <- (Sleft && Sright)
    Srate <- unlist(stringr::str_split(stringr::str_extract(samplesLine, "RATE\\W+[0-9.]+"), "[ \t]+"))[2]

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
        names(sacc) <- c('event', 'eye', 'xpos1', 'ypos1', 'stime', 'etime', 'dur', 'xpos2', 'ypos2', 'ampl', 'peakvel')
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

    ## Get samples
    samp_tmp <- grep("^[0-9]+", lines[bounds[1]:bounds[2]], value=TRUE)
    samp_tmp <- stringr::str_split(samp_tmp, pattern="[ \t]+")

    samp <- setNames(data.frame(matrix(ncol = 18, nrow = length(samp_tmp))),
                     c("time",
                       "xpl","ypl","psl", # position and pupil size for left eye
                       "xpr","ypr","psr", # position and pupil size for right eye
                       "xvl","yvl","xvr","yvr", # velocity for left and right eyes
                       "xr","yr", # resolution
                       "CR_warn", # corneal reflection mode warning
                       "target_x","target_y","target_dis","remote_warn" # remote mode info
                       ))
    # # SAMPLE LINE FORMAT
    #   * Monocular: <time> <xp> <yp> <ps>
    #   * Monocular, with velocity: <time> <xp> <yp> <ps> <xv> <yv>
    #   * Monocular, with resolution: <time> <xp> <yp> <ps> <xr> <yr>
    #   * Monocular, with velocity and resolution: <time> <xp> <yp> <ps> <xv> <yv> <xr> <yr>
    #   * Binocular: <time> <xpl> <ypl> <psl> <xpr> <ypr> <psr>
    #   * Binocular, with velocity: <time> <xpl> <ypl> <psl> <xpr> <ypr> <psr> <xvl> <yvl> <xvr> <yvr>
    #   * Binocular, with and resolution: <time> <xpl> <ypl> <psl> <xpr> <ypr> <psr> <xr> <yr>
    #   * Binocular, with velocity and resolution: <time> <xpl> <ypl> <psl> <xpr> <ypr> <psr> <xvl> <yvl> <xvr> <yvr> <xr> <yr>
    # 
    # ## CORNEAL REFLECTION
    #   * one extra column after all standard fields and before remote fields
    #   * 3 characters for monocular and 5 characters for binocular
    #     * if binocular, the second and third characters are for the left eye, and the forth and the fifth are for the right eye
    #   * "..." (monocular) or "....." (binocular) if no warning for sample
    #	  * first character is "I" if sample was interpolated
    #   * second character is "C" if CR missing
    #   * third character is "R" if CR recovery in progress
    # 
    # ## REMOTE
    #   * extra columns when using remote mode
    #   * <target x> <target y> <target distance> <warning messages>
    #   * warning messages contain:
    #   * 13 characters for monocular and 17 characters for binocular
    #     * first 9 characters for the target (M,A,N,C,F,T,B,L,R)
    #     * then 4 characters for each eye (T,B,L,R), left eye first if binocular
    #   * "............." (monocular) or "................." (binocular) if no warning for target and eye image
    #   * first character is "M" if target is missing
    #   * second character is "A" if extreme target angle occurs
    #   * third character is "N" if target is near eye so that the target window and eye window overlap
    #   * fourth character is "C" if target is too close
    #   * fifth character is "F" if target is too far
    #   * sixth character is "T" if target is near top edge of the camera image
    #   * seventh character is "B" if target is near bottom edge of the camera image
    #   * eighth character is "L" if target is near left edge of the camera image
    #   * ninth character is "R" if target is near right edge of the camera image
    #   * tenth character is "T" if eye is near top edge of the camera image
    #   * eleventh character is "B" if eye is near bottom edge of the camera image
    #   * twelfth character is "L" if eye is near left edge of the camera image
    #   * thirteenth character is "R" if eye is near right edge of the camera image
    
    if (length(samp_tmp) > 0) {
        samp_tmp <- data.frame(matrix(unlist(samp_tmp), ncol=length(samp_tmp[[1]]), byrow=TRUE), stringsAsFactors=FALSE)
        # print(samp_tmp[1,])
        
        ## recording mode (corneal reflection mode and/or remote mode)
        if (Starget) {
          samp$target_x <- samp_tmp[[ncol(samp_tmp)-3]]
          samp$target_y <- samp_tmp[[ncol(samp_tmp)-2]]
          samp$target_dis <- samp_tmp[[ncol(samp_tmp)-1]]
          samp$remote_warn <- samp_tmp[[ncol(samp_tmp)]]
          if (Scr) {
            samp$CR_warn <- samp_tmp[[ncol(samp_tmp)-4]]
          }
        } else {
          if (Scr) {
            samp$CR_warn <- samp_tmp[[ncol(samp_tmp)]]
          }
        }
        
        ## monocular data
        if (!Sbinoc) {
          ## time, posistion, pupil size, velocity
          if (Sleft) { # left eye
            samp$time <- samp_tmp[[1]]
            samp$xpl <- samp_tmp[[2]]
            samp$ypl <- samp_tmp[[3]]
            samp$psl <- samp_tmp[[4]]
            if (Svel){
              samp$xvl <- samp_tmp[[5]]
              samp$yvl <- samp_tmp[[6]]
            } 
          } else { # right eye
            samp$time <- samp_tmp[[1]]
            samp$xpr <- samp_tmp[[2]]
            samp$ypr <- samp_tmp[[3]]
            samp$psr <- samp_tmp[[4]]
            if (Svel){
              samp$xvr <- samp_tmp[[5]]
              samp$yvr <- samp_tmp[[6]]
            }
          }
          ## resolution
          if (Sres) {
            if (Svel){
              samp$xr <- samp_tmp[[7]]
              samp$yr <- samp_tmp[[8]]
            } else {
              samp$xr <- samp_tmp[[5]]
              samp$yr <- samp_tmp[[6]]
            }
          }
        ## binocular data
        } else if (Sbinoc) {
          samp$time <- samp_tmp[[1]]
          samp$xpl <- samp_tmp[[2]]
          samp$ypl <- samp_tmp[[3]]
          samp$psl <- samp_tmp[[4]]
          samp$xpr <- samp_tmp[[5]]
          samp$ypr <- samp_tmp[[6]]
          samp$psr <- samp_tmp[[7]]
          if (Svel) {
            samp$xvl <- samp_tmp[[8]]
            samp$yvl <- samp_tmp[[9]]
            samp$xvr <- samp_tmp[[10]]
            samp$yvr <- samp_tmp[[11]]
            if (Sres) {
              samp$xr <- samp_tmp[[12]]
              samp$yr <- samp_tmp[[13]]
            }
          } else {
            if (Sres) {
              samp$xr <- samp_tmp[[8]]
              samp$yr <- samp_tmp[[9]]
            }
          }
        }

    } else {
        samp <- NULL
    }

    ## Get message events
    msgRE <- "^MSG.*"
    msg_tmp <- grep(msgRE, lines[bounds[1]:bounds[2]], value=TRUE)
    if (length(msg_tmp) > 0) {
        msg_tmp <- subset(msg_tmp, !grepl(".*TRIAL_VAR.*", msg_tmp)) # drop trial variables
        msg_tmp <- stringr::str_match(msg_tmp, "^MSG\t([:digit:]+?)[:space:]([-]*[:digit:]*)[:space:]*(.*)")[,2:4]
        msg_tmp[msg_tmp==""] <- NA
        msg <- setNames(data.frame(msg_tmp, stringsAsFactors=FALSE),
                        c("time","offset","message"))
    } else {
        msg <- NULL
    }

    retval <- list(fix=fix, sacc=sacc, blink=blink, trialvar=trialvar, samp=samp, msg=msg)
    retval
}

##' @title Get events from SR Research ASCII data files.
##'
##' @description Read data from SR Research ASCII files (samples,
##'     fixations, saccades, blinks, etc).
##'
##' @details SR Research provides a utility (EDF2ASC.exe) that dumps
##'     ASCII renderings of their proprietary EDF data file
##'     format. This function reads those ASCII files and extracts
##'     eye-movement events (fixations, saccades, blinks), specified
##'     MSG events, and TRIAL_VARs from them.
##'
##' @param file A string giving path/fname to input file (ELascii
##'     file).
##' @param tStartRE A string containing a regular expression that
##'     uniquely identifies beginnings of trials.  It will be the
##'     first line for each trial that will be passed to
##'     \code{link{getEyelinkTrialData}} for processing.
##'
##'     The default value, "TRIALID", is a MSG that occurs immediately
##'     before an ET recording block. We use this as the default
##'     because it is guaranteed to be present.  But, it may not
##'     capture information recorded during a trial before that
##'     point. A case in point is where a DRIFTCORRECT (drift check)
##'     event is present right before the recording block. TRIALID
##'     will occur \strong{\emph{after}} the drift correct event,
##'     meaning that the drift correct offset values captured during
##'     the event will not be available. We do not use DRIFTCORRECT as
##'     the default value to tStartRE, because it is not guaranteed to
##'     be present; not every experimentor chooses to include this
##'     event in each trial. Other reasonable choices for this
##'     argument may target the EB generated "PREPARE_SEQUENCE" MSG,
##'     or even a user generated MSG.
##' @param tEndRE A string containing regular expression that uniquely
##'     identifies ends of trials. It will be the last line for each
##'     trial that will be passed to
##'     \code{\link{getEyelinkTrialData}}.
##'
##'     The default value, "TRIAL_RESULT", is always the last line to
##'     occur in a well-formed trial; the block of "TRIAL_VAR" lines
##'     appears right before it. But, if an experiment is aborted
##'     prematurely, then the the last trial in the *edf file (and so
##'     the *asc file) may not have a proper trial end event for the
##'     last trial.
##'
##'     TODO: Test for the case where tStarteRE and TEndRE are
##'     mismatched and handle it more gracefully, while throwing a
##'     warning.
##' @param subjID If NULL (default), use filename as subject
##'     ID. Otherwise use specified string.
##' @return List with two elements, one for session information, and
##'     one containing a list of trials. Each trial element is itself
##'     a list of 6 elements: data.frames enumerating fixations,
##'     saccades, blinks, samples, TRIAL_VARs and MSGs for the trial.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @author Monica Li \email{monica.yc.li@@gmail.com}
##' @export
readELascii <- function(file,
                        tStartRE="TRIALID",
                        tEndRE="TRIAL_RESULT",
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
    header <- grep("^[*][*] ", lines, value=TRUE)
    script <- unlist(stringr::str_split(grep("RECORDED BY", header, value=TRUE), "[ \t]+"))[4]
    sessdate <- unlist(stringr::str_split(grep("DATE:", header, value=TRUE), ": "))[2]
    srcfile <- unlist(stringr::str_split(grep("CONVERTED FROM", header, value=TRUE), " (FROM|using) "))[2]
    srcfile <- basename(srcfile)
    conversion <- unlist(stringr::str_split(grep("CONVERTED FROM", header, value=TRUE), " (FROM|using|on) "))[3]
    session <- data.frame(subj, script, sessdate, srcfile, conversion)

    ## get start and end lines for each trial block
    tStart <- grep(tStartRE, lines)
    tEnd <- grep(tEndRE, lines)
    stopifnot (length(tStart) == length(tEnd)) ##TODO: something more elegant of there's a mismatch here.
    trialidx <- cbind(tStart, tEnd)

    ## get trial IDs
    trialids <- unlist(stringr::str_split(grep("TRIALID", lines, value=TRUE), " TRIALID "))
    trialids <- trialids[seq(2, length(trialids), 2)]

    ## get events for each trial
    trials <- apply(trialidx, 1, getEyelinkTrialData, lines=lines)
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

