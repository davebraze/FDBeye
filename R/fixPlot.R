##' @title Plot Gaze Fixation Data
##'
##' @description Plot gaze fixation data overlayed on a bitmap image. This is a convenience wrapper
##'     around ggplot2::ggplot().
##'
##' @details This is a convenience function for using ggplot() to lay fixation data over a
##'     background bitmap. At present, only PNG files are supported for background images. Usually,
##'     these will correspond to the visual stimulus viewed by subjects while gaze was
##'     recorded. Function returns a ggplot2 object which can be further modified by ggplot2 geoms.
##'
##' @param data A data.frame containing fixations. Must include x and y positions with column names
##'     "x" and "y".
##' @param bgimage Path to background image. Must be a PNG file. Usually this will correspond to the visual stimulus
##'     being viewed by subjects while their eye movements were recorded.
##' @param bgalpha Alpha level for background layer. Currently not working.
##' @param xyMap Aesthetic mapping (ggplot2::aes_string()) for x and y coordinates. Passed to ggplot().
##' @param pointMap Additional aesthetics specific to points. Passed to geom_point().
##' @param pointAlpha Set point transparency.
##' @param mar A 4 vector for margin adjustment: top, right, bottom, left. Use positive values move
##'     margins toward center of plot (trim the display area). Use negative values to expand the
##'     display area beyond the bitmap.
##' @param showRug Logical indicating whether to include rug plots for x and y margins. Defaults is
##'     FALSE.
##' @param rugMap Additional aesthetics specific to rugs. Passed to geom_rug().
##' @param rugColor Set rug color.
##' @param rugAlpha Set rug transparency.
##' @param rugSize Set rug size.
##' @param ... Additional arguments passed to ggplot2::geom_point().
##' @param showPlot Logical indicating whether to display the plot. Defaults to TRUE.
##' @return A ggplot2 object.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @import ggplot2
##' @export
fixPlot <- function(data,
                    bgimage,
                    bgalpha=.33,
                    xyMap=ggplot2::aes_string(x="xpos", y="ypos"),
                    pointMap=ggplot2::aes_string(size="dur"),
                    pointAlpha=.5,
                    mar=c(0,0,0,0),
                    showRug=FALSE,
                    rugMap=NULL,
                    rugColor="black",
                    rugAlpha=.33,
                    rugSize=2,
                    ...,
                    showPlot=TRUE) {

    if (!is.null(bgimage)) {
        bg <- png::readPNG(bgimage) # maybe use EBImage::readImage() instead ?
        ysize <- dim(bg)[1]
        xsize <- dim(bg)[2]
        zsize <- dim(bg)[3]
        bgalpha <- matrix(bgalpha, ysize, xsize)
        if(zsize==3) {                  ## add an alpha layer if none is present
            bg <- abind::abind(bg, bgalpha)
        } else if (zsize==4) {          ## or over-write existing alpha layer
            bg <- abind::abind(bg[,,1:3], bgalpha)
        }
        bg <- grid::rasterGrob(bg)
    }

    p <- ggplot(data=data, mapping=xyMap) + geom_blank()
    p <- p + coord_equal()
    p <- p + scale_x_continuous(limits=c(0+mar[4],xsize-mar[2]), ## need to make better choices for tic positions when margins are in place.
                                expand=c(0,0))
    p <- p + scale_y_continuous(limits=c(ysize-mar[3], 0+mar[1]),
                                trans="reverse",
                                expand=c(0,0))
    p <- p + annotation_custom(bg,
                               xmin=0, ymin=-ysize,
                               xmax=xsize, ymax=0)
    p <- p + geom_point(mapping=pointMap, alpha=pointAlpha, ...)

    if(showRug) {
        if(is.null(rugMap)) {
            p <- p + geom_rug(color=rugColor, alpha=rugAlpha, size=rugSize)
        } else {
            p <- p + geom_rug(mapping=rugMap, color=rugColor, alpha=rugAlpha, size=rugSize)
        }
    }

    ## show the fixation plot
    if (showPlot) print(p)

    ## return a ggplot2 object
    invisible(p)

}


if(FALSE) {

  data <- data.frame(x=seq(10, (xsize-10), length.out=10),
                  y=seq(10, (ysize-10), length.out=10))

}

