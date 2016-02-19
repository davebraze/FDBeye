##' @title Adjust y coordinates for gaze data.
##'
##' @description Adjust y coordinates for gaze data, such as might occur in reading multi-line text
##'     or other tasks with a similar visual scanning component. While "ground truth" for fixation
##'     locations is not easily obtained, consensus among reading researchers who use eye tracking
##'     to understand on-line reading processes holds that data for y coordinates obtained through
##'     most (all) eye tracking devices is imperfect and often in need of adjustment prior to
##'     computing regional summaries of gaze data. This function serves that purpose, using an
##'     optimization approach similar to that used in aligning neurimaging data with standard
##'     templates.
##'
##' @details
##'
##' @param data A data.frame containing gaze data (fixations or samples), possibly from multiple subjects/trials.
##' @param FUN A function to optimize in order to compute adjusted y-values for a single trial.
##' @param lines A vector of known y positions (centroids) of text lines for each trial contained in
##'     \code{data}. This argument is passed to \code{FUN}. [maybe this should be a data.frame]
##' @param ... Additional arguments passed to \code{FUN}.
##' @return A copy of data enriched with adjusted y values.
##' @author
adjust_y <- function(data,
                     FUN,
                     lines,
                     ...) {

    ## See fix_align.R, here: http://www.psych.umass.edu/PACLab/resources/
    ## And description of same in BRMIC: http://people.umass.edu/eyelab/CohenBRM.pdf
    ##
    ## Our version should differ from that one in
    ## 1. We take a data.frame rather than an SRR *ASC file as input.
    ## 2. We ADD modified y values, rather than replace existing ones.
    ## 3. We accomidate both sample data and fixation data.
    ## 4. Specific function used for optimisation is swappable.


}
