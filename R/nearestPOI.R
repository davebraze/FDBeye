##' @title Find the nearest Point of Interest for each fixation.
##'
##' @description For each fixation in a fixation report, this function
##'     will determine the nearest Point of Interest and the
##'     fixation's distance from that Point of Interest.
##'
##' @details For each fixation in a fixation report, this function
##'     will determine the nearest Point of Interest and the
##'     fixation's distance from that Point of Interest.
##'
##'     The value of \code{supplement} determines whether to return
##'     the POI values within an enriched version of \code{fixReport},
##'     or as a simple two column data.frame.
##'
##' @param fixReport A fixation Report as may be created by
##'     fixReport(). Must contain columns \code{xpos} and \code{ypos}.
##' @param poiList A data.frame containing x,y coordinates for Points
##'     of Interest. The data.frame must contain exactly two columns,
##'     named \code{xpos} and \code{ypos}, in that order.
##' @param supplement If TRUE, nearestPOI() will return a data.frame
##'     consisting of fixReport suplemented with columns nearestpoi
##'     and dist2poi. If FALSE return a 2 column data.frame with just
##'     those two columns.
##' @return A data.frame including columns nearestpoi and dist2poi.
##' @author Dave Braze <davebraze@@gmail.com>
##' @export
nearestPOI <- function (fixReport, poiList, supplement=TRUE) {
    f <- function(fix, poiList) {
        ## Find nearest poi for one fixation.
        tmp <- rbind(fix[c("xpos", "ypos")], poi) ## should merge rather than rbind
        dst <- as.matrix(dist(tmp))[-1,1]
        .nearestpoi <- which.min(dst)
        names(.nearestpoi) <- NULL
        .dist2poi <- min(dst)
        retval <- data.frame(nearestpoi=.nearestpoi, dist2poi=.dist2poi)
    }
    retval <- purrrlyr::by_row(fixReport, f, poiList=poiList, .collate="row",
                            .labels=supplement)
    select(retval, -matches("^.row$"))
}
