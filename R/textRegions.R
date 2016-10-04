## Work toward a flexible method of defining scoring regions for multi-line texts.
## Also see yaml-test.txt system.file("extdata/yaml-test.txt")

##' @title Convert region file to region definition file.
##'
##' @description Convert a file containing full region description into a region definition
##'     file. The latter is suitable for hand editing and can be used to generate alternative region
##'     specifications (e.g., multi word regions) for text stimuli.
##'
##' @details We aspire to handle input files (region files) for multi line texts, but at present
##'     only region files for single line texts are handled.
##'
##'     File parameter values are used to fill in parameters written to the yaml block of the region
##'     definition file. Note that few of these parameters are important when the region definition
##'     file is used to create a new region file. Two parameters are critical to proper region definitions.
##'
##'     The chrW parameter is used to translate region boundaries in x dimension from letter
##'     positions (as specified in the region definition file) to pixel positions (as required for
##'     the region or ias file). chrW is estimated from contents of region.csv file, and will
##'     probably be correct most of the time. Regardless, it should be checked and, if necessary,
##'     manually edited in the resulting region definition file.
##'
##'     Accurate baseline positions are also critical to determining the y positionins of
##'     regions. Baselines are read directly from the region.csv file and should be accurate. Note
##'     that baseline positions, in pixels, are measured from the TOP of the screen.
##'
##'     TODO: Make to work with region files for multi-line text stims.
##'
##'     TODO: Make to read/parse SRR IAS files and build region defs based on them.
##'
##' @param reg A data.frame containing region specifications, as read from a region file
##'     ("*.region.csv").
##' @param scrnW Screen width in pixels (integer).
##' @param scrnH Screen height in pixels (integer).
##' @param fnt.name Font name used for stimulus text.
##' @param fnt.size Nominal font size in points for text display.
##' @param chrW Letter width in pixels.
##' @param chrH Nominal letter height in pixels.
##' @param ln.space Line spacing in pixels for multi line texts. Multi line texts are not currently
##'     supported.
##' @param baseline Baseline positions for each line of text. Measured in pixels from the top of the
##'     screen. Multi line texts are not currently supported.
##' @param mrgn.top Top margin in pixels.
##' @param mrgn.left Left margin in pixels.
##' @param mrgn.bottom Bottom margin in pixels.
##' @param mrgn.right Right margin in pixels.
##' @param rgn.maxH Extent of regions of interest above baseline in pixels.
##' @param rgn.minH Extent of regions of interest below baseline in pixels.
##' @param rgn.padL Expand leftmost region on each line leftward by this amount in pixels.
##' @param rgn.padR Expand rightmost region on each line rightward by this amount in pixels.
##' @return A vector of strings containing the region definition. The vector includes a yaml block
##'     with values for each of the function parameters except for "reg". In addition to the yaml
##'     block, the vector will include a pair of lines for each line of text in the stimulus. The
##'     first element of each pair is the text displayed on that line. The second element is a
##'     regioning string made up of spaces (" "), and pipe ("|") characters. Pipes indicate the
##'     beginnings of regions. By default, the region definition file will specify that each line be
##'     exhaustively dividied into space delimited regions (i.e. there will be a pipe character
##'     corrponding to each space character in the paired text line.
##'
##'     This vector can be written to file and hand edited to add or correct information in the yaml
##'     block, or to re-specify region placements.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
##' @examples

reg2regdef <- function(reg, scrnW=NA, scrnH=NA,
                       fnt.name=NA, fnt.size=NA,
                       chrW=NA, chrH=NA,
                       ln.space=NA, baseline=NA,
                       mrgn.top=NA, mrgn.left=NA, mrgn.bottom=NA, mrgn.right=NA,
                       rgn.maxH=NA, rgn.minH=NA, rgn.padL=NA, rgn.padR=NA) {
    ##### build yaml block #####
    if (is.na(baseline)) baseline <- unique(reg$baseline)
    if (is.na(chrH)) chrH <- unique(reg$height)
    if (is.na(chrW)) {
        chrW <- diff(reg$x1_pos)
        chrW <- FDButils::gcd(chrW)
    }
    scrn <- list(screen=list(width=as.integer(scrnW), height=as.integer(scrnH))) # likely defaults
    fnt <- list(font=list(name=fnt.name, size=fnt.size)) # no default; not important to region definitions
    chr <- list(character=list(width=as.integer(chrW), height=as.integer(chrH)))
    lns <- list(lines=list(spacing=ln.space, baseline=as.integer(baseline)))
    mrg <- list(margins=list(top=as.integer(mrgn.top), left=as.integer(mrgn.left),
                             bottom=as.integer(mrgn.bottom), right=as.integer(mrgn.right)))
    rgns <- list(regions=list(maxH=as.integer(rgn.maxH), minH=as.integer(rgn.minH),
                              padL=as.integer(rgn.padL), padR=as.integer(rgn.padR)))
    hdr <- c("---\n",
             sapply(list(scrn, fnt, chr, lns, mrg, rgns), yaml::as.yaml),
             "---\n")

    ## build regdef block
    txt <- str_c(reg$Word, collapse="")
    idx <- str_locate_all(txt, " ")[[1]][,1]
    regmarks <- rep(" ", str_length(txt))
    regmarks[idx] <- "|"
    regmarks <- paste(regmarks, collapse="")
    ln <- c(paste0("\n", txt, "\n", regmarks, "\n"))

    retval <- c(hdr, ln)
    retval
}

if(FALSE) {
    library(yaml)
    library(FDBeye)

    # l <- readLines(system.file("extdata/yaml-test.txt"), package="FDBeye")
    l <- readLines("../inst/extdata/yaml-test.txt")

    ## Find the "---" demarcated yaml block
    ## of parameters and read it.
    pblock <- grep("$---", l) # yaml blocks begin and end with a line starting with 3 hyphens.
    pblock <- (min(pblock)+1):(max(pblock)-1)
    pblock <- paste(l[pblock], collapse="\n")
    parms <- yaml.load(pblock)
    ## The parameters that matter most are
    ## font$baselines
    ## margins$left
    ## character$width
    ## need to determine region H somehow.
    ## 1. based on proportion of baseline to baseline distance
    ## 2. or allow setting a parameter

    ## Find the text to be regioned
    ## together with region markers.
    ## Ignore blank lines.
    tstart <- max(grep("---", l))+1
    tend <- length(l)
    tblock <- l[tstart:tend]
    tblock <- tblock[-grep("^[ \t]*$", tblock)]

}
