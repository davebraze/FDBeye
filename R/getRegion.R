##' @title Find the region containing each fixation.
##'
##' @description For each fixation in a fixation report, this function
##'     will determine the Region of Interest that contains it, if any.
##'
##' @details For each fixation in a fixation report, this function
##'     will find the Region of interest that contains it, if any.
##'
##'     Regions are assumed to be rectangular. Region bounds labeled as
##'      'left', 'right', 'top', 'bottom'.
##'
##'     The value of \code{supplement} determines whether to return
##'     the region values within an enriched version of \code{fixReport},
##'     or as a simple two column data.frame.
##'
##' @param fixReport A data.frame containing a fixation Report as may
##'     be created by fixReport(). Must contain columns \code{xpos}
##'     and \code{ypos}.
##' @param regionList A data.frame containing regions of
##'     interest. Must have columns with region bounds labeled 'left',
##'     'right', 'top', 'bottom'. Regions are assumed to be
##'     rectangular.
##' @param noRegnum A region number to assign fixations not contained
##'     within any of the regions defined in \code{regions}
##'     argument. Defaults to \code{NA}. Specified value should be
##'     \code{\%in\% regions$regnum}.
##' @param noReglabel The label assigned to fixations outside of any
##'     region defined in \code{regions} argument.
##' @param supplement If TRUE, getRegion() will return a data.frame
##'     consisting of fixReport suplemented with columns regnum and
##'     reglabel. If FALSE, return a 2 column data.frame with just
##'     those two columns.
##'
##' @return A data.frame including columns regnum and reglabel.
##' @author Dave Braze <davebraze@@gmail.com>
##' @export
getRegion <- function(fixReport, regionList, noRegnum=NA, noReglabel="", supplement=TRUE) {
    f <- function(fix, regionList, noRegnum=noRegnum, noReglabel=noReglabel) {
        ## Find containing Region for a single fixation.
        regs <- 1:nrow(regions)
        for (ii in regs) {
            if (fix$xpos >= regions[[ii,"left"]] && fix$xpos <= regions[[ii,"right"]] &&
                fix$ypos >= regions[[ii,"top"]] && fix$ypos <= regions[[ii,"bottom"]])
            {
                retval <- data.frame(regnum=as.integer(regions[[ii,"regnum"]]),
                                     reglabel=regions[[ii,"reglabel"]])
            } else {
                retval <- data.frame(regnum=as.integer(noRegnum), reglabel=noReglabel)
            }
            if (retval$regnum %in% regions$regnum) return(retval)
        }
        return(retval)
    }
    retval <- purrrlyr::by_row(fixReport, f, regionList=regionList, noRegnum=noRegnum, noReglabel=noReglabel,
                            .collate="row", .labels=supplement)
    select(retval, -matches("^.row$"))
}
