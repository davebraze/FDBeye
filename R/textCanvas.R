## Let's refer to a bitmap image of text used for display of text
## stimuli as a "text canvas".

##' @title Counts 'inked' values in a vector.
##'
##' @description Counts number of values in a vector that are different from the 'background'.
##'
##' @details Counts number of values in a vector that are different from the 'background'. Intended
##'     to be apply()ed to the rows or columns of a matrix (a color plane in a text canvas).
##'
##'     Not necessarily intended to be called by the end user.
##' @param v A vector of pixels. It will generally be either a row or column from a text canvas.
##' @param bg The background value.
##' @param tol A tolerance around the background value. Defaults to 1/(2^32).
##' @return A numeric value corresponding to the number of non-background values in \code{v}.
##' @author Dave Braze <davebraze@@gmail.com>
##' @export
inked <- function(v,
                  bg,
                  tol=1/(2^32)) {
    sum(abs(bg-v)>tol)
}

##' @title Finds 'background' value in a matrix.
##'
##' @description Finds the most common value in a matrix and returns it.
##'
##' @details Finds the 'background' value in a matrix, defined as the most common value in a matrix.
##'
##'     Not necessarily intended to be called by the end user.
##' @param cnvs A matrix representing a text canvas.
##' @return A numeric value corresponding to the the most common value in \code{cnvs}.
##' @author Dave Braze <davebraze@@gmail.com>
##' @export
getBGcol <- function(cnvs) {
    cols <- table(cnvs) # table is inefficient; find something better
    bgcol <- as.numeric(names(which.max(cols)))
    bgcol


    ## median(cnvs) ## This may work in general, but have to think it through. What happens if the
    ## background value is found in fewer (or many fewer) than half the elements of cnvs? Maybe
    ## check to see if bg is a low proportion of values in cnvs and issue a warning() in that case.
}

##' @title Estimate locations of text lines within text canvases.
##'
##' @description None yet
##'
##' @details This function, and it's companions getChars() and GetMargins(), are utilities designed to
##'     extract details of text position in a bitmap file, a text canvas, as we'll call it here. A
##'     text canvas is simply a bitmap that contains one or more lines of text. The text is assumed
##'     to be organized as it would be on a printed page.
##'
##'     In our own lab we generate text canvases programmatically to use as stimulus items in the
##'     context of experiment control software (ECS) like SRR's Experiment Builder, or PST's
##'     E-Prime. The Python scripts we use for the purpose also generate region files (AKA area of
##'     interest files, etc). Because we build the text canvases ourselves, we have precise control
##'     over the locations of words within them.
##'
##'     Another approach to displaying text in an experiment is to let the ECS build the text
##'     canvases from plain text representations of the stimuli. That's the standard way of
##'     presenting text (sentences, paragraphs, whatever) in SRR Experiment Builder, for
##'     example. However, EB will generate text canvases for each trial based on the text provided
##'     to it. These are pre-compiled and stored in a specific directory under the deployed EB
##'     script. So, if your EB script is named "SentenceReading", then it will live in a directory
##'     with that same name. Any screen displayed in the execution of the script, including text
##'     canvases, will be in a sub-folder called "runtime/images".
##'
##'     Sadly, these files will have unhelpful names consisting of a long string of numbers, like
##'     "307641742100813627.png". And, as noted "runtime/images" will also also contain png files
##'     for screens other than the text canvases used as experimental stimuli, with no hint in the
##'     file names as to which are which. So, the only way to separate out the text canvases of
##'     interest is to go through the files manually and sort them out. (To be clear, I think this
##'     is a problem that is not specific to EB. I guess any experiment control software will have a
##'     similar issue.)
##'
##'     But, once you have those stimulus files, you may want to parse out the locations of text in
##'     the files so you can do some reanalysis based on regions of interest other than those that
##'     were specified prior to data acquisition. Certainly, this could be done by making manual
##'     measurements of each text canvas, but the functions, getLines(), getChars(), and
##'     getMargins() are designed to at least partially automate the process.
##'
##' @note TODO: Work on getting top and bottom bounds for each line, as well as estimated baseline.
##'
##'     TODO: Find out if there is a way to interpret the filenames of bitmaps located in SRR EB
##'     script subdirectory "runtime/images" in a meaningful way. Most helpful would be to get a
##'     list of those files that SRR DataViewer knows to used as background images for a specific
##'     EDF file.
##' @param canvas A N dimensional matrix (product of png::readPNG()) corresponding to a bitmap image
##'     containing rasterized text.
##' @return A data frame containing 1 row for each line of text in the canvas, and the following
##'     columns:
##'
##' \enumerate{
##'     \item line: Integer indicating text line number, counting from the top of the canvas.
##'     \item top: Integer indicating upper extent of line in pixels.
##'     \item baseline: Integer indicating location of line's baseline (estimated) in pixels. NOT YET IMPLEMENTED.
##'     \item bottom: Integer indicating lower extent of line in pixels.
##' }
##'
##' @author Dave Braze <davebraze@@gmail.com>
##' @export
##' @examples
##'     cnvs <- system.file("extdata/story01.png", package="FDBeye")
##'     cnvs <- png::readPNG(cnvs)
##'     fcnvs <- apply(cnvs, c(1,2), sum) # flatten to a single plane for convenience
##'
##'     ## get lines
##'     getLines(fcnvs)
##'
getLines <- function(canvas){
    bgcol <- getBGcol(canvas)
    ink <- apply(canvas, 1, inked, bg=bgcol)

    inkb <- ink>0
    inki <- FDB1::series(as.integer(inkb), step=0) ## find rows with ink
    erun <- apply(inki[,2:3], 1, sum)-1
    lines <- cbind(inki, erun)
    lines <- lines[lines[,1]==1,]
    colnames(lines) <- c("line", "top", "baseline", "bottom")
    lines[,'baseline'] <- NA
    lines[,'line'] <- 1:dim(lines)[1]
    lines
}

if (FALSE) {
    library(FDBeye)
    cnvs <- system.file("extdata/story01.png", package="FDBeye")
    cnvs <- png::readPNG(cnvs) # Look into imager::
    fcnvs <- apply(cnvs, c(1,2), sum) # flatten the into a single plane

    ## get margins
    getMargins(fcnvs)

    ## get lines
    getLines(fcnvs)

}

##' @title Sweep each line of text in a text cavas to estimate
##'     character locations.
##'
##' @description None yet
##'
##' @details None yet
##'
##' @param canvas A N dimensional matrix (product of png::readPNG())
##'     corresponding to bitmap image.
##' @param lines Data.frame generated by applying guessLines() to
##'     canvas.
##' @return A data frame containing 1 row for each character in each
##'     line of canvas, and the following columns:
##'
##' \enumerate{
##'     \item line: Integer indicating line number, counting from the top of the canvas.
##'     \item char: Integer indicating the character number, counting from the left most.
##'     \item left: Integer indicating left-most extent of character in pixels.
##'     \item right: Integer indicating right-most extent of character in pixels.
##' }
##'
##' @author Dave Braze <davebraze@@gmail.com>
getChars <- function(canvas, lines){}

##' @title Find the margins in a text canvas
##'
##' @description None yet
##'
##' @details None yet
##'
##' @param canvas A N dimensional matrix (product of png::readPNG())
##'     corresponding to bitmap image.
##' @return A named 4 vector containing top, right, bottom and left margins
##' @author Dave Braze <davebraze@@gmail.com>
##' @export
##' @examples
##'     cnvs <- system.file("extdata/story01.png", package="FDBeye")
##'     cnvs <- png::readPNG(cnvs)
##'     fcnvs <- apply(cnvs, c(1,2), sum) # flatten to a single plane for convenience
##'
##'     ## get margins
##'     getMargins(fcnvs)
getMargins <- function(canvas){
    bgcol <- getBGcol(canvas)
    inkr <- apply(canvas, 1, inked, bg=bgcol)
    top <- min(which(inkr > 0))-1 ## top margin
    bottom <- max(which(inkr > 0))+1 ## bottom margin

    inkc <- apply(canvas, 2, inked, bg=bgcol)
    left <- min(which(inkc > 0))-1 ## left margin
    right <- max(which(inkc > 0))+1 ## right margin

    c(top=top, right=right, bottom=bottom, left=left)
}
