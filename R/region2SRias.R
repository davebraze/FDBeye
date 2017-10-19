##' @title Convert region file to SR interest area file
##'
##' @description Convert a region file to an SR interest area file. We
##'     only handle static IAS files.
##'
##' @details
##'
##' TODO: Need to incorporate some flexibility as to which columns in
##'     the 'region' file will be accessed for which bits of
##'     information. Right now, column names are hard-coded.
##'
##' @param regfile Region file to read, formatted per Tao Gong's
##'     Python code.
##' @param iasfile If NULL (default) then create the output filename
##'     by stripping ext from regfile and adding ".ias". If not NULL,
##'     then this should be a string specifying output filename.
##' @param size Extract "big" or "small" interest areas from region
##'     file.
##' @param yoffsets If NULL (default), use y coordinates as read from
##'     region file for Interest Areas. If not NULL, must be a
##'     2-vector of integers indicating extent above and below
##'     baseline for Interest areas (in pixels). Both values should be
##'     positive. If yoffsets is not NULL, then the 'size' argument is
##'     ignored.
##' @param xpad If NULL (default), use x coordinates for Interest
##'     Areas as read from region file. If not NULL, must be a
##'     2-vector of integers indicating extent leftward and rightward
##'     to expand the Interest Areas for the first and last words on
##'     each line (in pixels). Both values should be positive.
##' @return Invisibly returns a data.frame containing a set of
##'     Interest Areas, but typically called for its side effect of
##'     writing an "*.ias" file.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @import stringr
##' @export
##' @examples
##' library(ggplot2)
##' reg <- system.file("/extdata/target01A.region.csv", package="FDBeye")
##' stim <- system.file("/extdata/target01A.png", package="FDBeye")
##' ias <- region2SRias(reg, yoffsets=c(50,30), xpad=c(18,18))
##' fp <- fixPlot(data = data.frame(x=-1, y=-1),
##'               bgImage = stim, bgAlpha=1,
##'               xyMap = ggplot2::aes_string(x='x', y='y'),
##'               pointMap=ggplot2::aes_string(alpha=0)
##'               )
##'
##' fp + geom_rect(data=ias, inherit.aes=FALSE,
##'                aes(xmin=x1+1, xmax=x2, ymin=y1, ymax=y2,
##'                fill=NULL, color=as.factor(WordID%%2)), alpha=0) +
##'      guides(color=FALSE)
##'
region2SRias <- function(regfile,
                         iasfile=NULL,
                         size=c("big", "small"),
                         yoffsets=NULL,
                         xpad=NULL) {
    size <- match.arg(size, c("big", "small"))
    type="RECTANGLE" # region shape; always RECTANGLE for print stimuli
    reg <- read.csv(regfile, as.is=T)

    ## replace " " with "_" ## because of DV issues with spaces in IAS labels.
    ## replace """ with "'" ## because of DV issues with double quotes in IAS labels.
    reg$Word <- stringr::str_replace_all(reg$Word, " ", "_")
    reg$Word <- stringr::str_replace_all(reg$Word, '"', "'")

    if(is.null(iasfile)) {
        outfile <- tools::file_path_sans_ext(regfile)
        outfile <- paste(outfile, "ias", sep=".")
    } else {
        outfile <- iasfile
    }

    if (size=="big") {
        ias <- data.frame(type, reg[c("WordID", "b_x1", "b_y1", "b_x2", "b_y2", "Word")] )
    } else {
        ias <- data.frame(type, reg[c("WordID", "x1_pos", "y1_pos", "x2_pos", "y2_pos", "Word")] )
    }
    names(ias)[3:6] <- str_extract(names(ias)[3:6],"[xy][12]")

    if (!is.null(yoffsets)) {
        if(length(yoffsets)!=2) stop("yoffsets must contain exactly 2 integers")
        yoffsets <- as.integer(yoffsets)
        yoffsets <- abs(yoffsets) ## in case someone sends negative values.
        ias$y1 <- reg$baseline - yoffsets[1]
        ias$y2 <- reg$baseline + yoffsets[2]
    }

    if (!is.null(xpad)) {
        ## TODO: EXPAND FEATURE to work with multiline texts. Probably calls for ddply()
        ## At present this only works for single line texts.
        if(length(unique(reg$line_no))!=1) stop("xpad feature implemented for singly line texts only.")
        if(length(xpad)!=2) stop("xpad must contain exactly 2 integers.")
        xpad <- as.integer(xpad)
        xpad <- abs(xpad) ## in case someone sends negative values.
        ias$x1[1] <- ias$x1[1] - xpad[1]
        last <- length(ias$x1)
        ias$x2[last] <- ias$x2[last] + xpad[2]
    }

    write.table(ias, outfile, quote=FALSE, qmethod=NULL, sep="\t", row.names=F, col.names=F)
    invisible(ias)
}
