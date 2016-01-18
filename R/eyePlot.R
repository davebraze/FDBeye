
##' A convenience wrapper around ggplot2::ggplot()
##'
##' @details This is a convenience function for using ggplot() to lay fixation data over a
##'     background bitmap.
##'
##' @title Plot Fixations over bitmap using ggplot2.
##' @param bgimage Path to background image for the plot. Usually will be the visual stimulus
##'     subjects viewed will eye movements were recorded.
##' @param bgalpha Alpha level for background layer. Currently not working.
##' @param mar A 4 vector for margin adjustment: top, right, bottom, left. Use positive values move
##'     margins toward center of plot (trim the display area). Use negative values to expand the
##'     display area beyond the bitmap.
##' @param data A data.frame containing fixations. Must include x and y positions with column names
##'     "x" and "y".
##' @return A ggplot2 object.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @import ggplot2
##' @export
fixPlot <- function(bgimage, bgalpha=.5, mar=c(0,0,0,0), data) {
    if (!is.null(bgimage)) {
        # bgimage <- "d:/braze/R/development/FDBeye/inst/extdata/story01.png"
        bg <- png::readPNG(bgimage) # maybe use EBImage::readImage() instead
        ysize <- dim(bg)[1]
        xsize <- dim(bg)[2]
        bg <- grid::rasterGrob(bg) # figure out how to set alpha here (EBImage??)
    }
    # mar=c(50,250,180,200) # margin adjustment

    p <- ggplot(data=data, aes(x=x, y=y)) + geom_blank()
    p <- p + coord_equal()
    p <- p + scale_x_continuous(limits=c(0+mar[4],xsize-mar[2]), ## need to make better choices for tic positions when margins are in place.
                                expand=c(0,0))
    p <- p + scale_y_continuous(limits=c(ysize-mar[3], 0+mar[1]),
                                trans="reverse",
                                expand=c(0,0))
    p <- p + annotation_custom(bg,
                               xmin=0, ymin=-ysize,
                               xmax=xsize, ymax=0)
    p <- p + geom_point(size=5, color="blue", alpha=.5)
    p

}


if(FALSE) {

  data <- data.frame(x=seq(10, (xsize-10), length.out=10),
                  y=seq(10, (ysize-10), length.out=10))

}

