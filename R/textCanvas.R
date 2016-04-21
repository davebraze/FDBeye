## Let's refer to a bitmap image of text used for display of text
## stimuli as a "text canvas".

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
##'     \item line: Integer indicating line number, counting from the top of the canvas.
##'     \item top: Integer indicating upper extent of line in pixels.
##'     \item baseline: Integer indicating location of line's baseline (estimated) in pixels.
##'     \item bottom: Integer indicating lower extent of line in pixels.
##' }
##'
##' @author Dave Braze <davebraze@@gmail.com>
getLines <- function(canvas){

}
if (FALSE) {
    cnvs <- system.file("extdata/story01.png", package="FDBeye")
    cnvs <- png::readPNG(cnvs) # Look into imager::, maybe also see what EBImage:: has to offer?
    cnvs <- cnvs[,,1] # for simplicity just grab 1 layer, later need to work with all layers

    ## get background color; table() is not very efficient; find something better
    system.time( bg <- table(cnvs) )
    bg <- as.numeric(names(which.max(bg)))

    ## find rows with pixels that are NOT the background color, within some tolerance
    tol <- 1/(2^32) # 32 bits per plane?
    (bg - cnvs[1]) > tol
    length(which((bg - cnvs) > tol))

    ## wrap it into a function that can be apply()ed.
    nbg <- function(v, bg, tol) {
        sum((bg-v)>tol)
    }
    cbind(apply(cnvs, 1, nbg, bg=bg, tol=tol))

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
##'
getMargins <- function(canvas){}
