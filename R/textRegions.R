## Work toward a flexible method of defining arbitrary scoring regions for multi-line texts.
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
##'     ToDo: Make to work with region files for multi-line text stims.
##'
##'     ToDo: Make to read/parse SRR IAS files and build region defs based on them.
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


##' @title Convert region file to region definition file.
##'
##' @description Convert a file containing full region description
##'     into a region definition file. The latter is suitable for hand
##'     editing and can be used to generate alternative region
##'     specifications (e.g., multi word regions) for text stimuli.
##'
##' @details We aspire to handle region definitions for multi line
##'     texts, but at present only region files for single line texts
##'     are handled.
##'
##'     File parameter values are used to fill in parameters written
##'     to the yaml block of the region definition file. Note that few
##'     of these parameters are important when the region definition
##'     file is used to create a new region file.
##'
##'     A region definition file contains 2 parts. The first is a yaml
##'     block with values for each of the function parameters except
##'     for "reg".  Two parameters in the yaml block are critical to
##'     proper region definitions.  The chrW parameter is used to
##'     translate region boundaries in x dimension from letter
##'     positions (as specified in the region definition file) to
##'     pixel positions (as required for the region or ias file). chrW
##'     is estimated from contents of region.csv file, and will
##'     probably be correct most of the time. Regardless, it should be
##'     checked and, if necessary, manually edited in the resulting
##'     region definition file.
##'
##'     Accurate baseline positions are also critical to determining
##'     the y positionins of regions. Baselines are read directly from
##'     the region.csv file and should be accurate. Note that baseline
##'     positions, in pixels, are measured from the TOP of the screen.
##'
##'     The second part of a region definition file is the region
##'     block. This block contains a pair of lines for each line of
##'     text in the stimulus. The first element of each pair is the
##'     text displayed on that line. The second element is a regioning
##'     string made up of spaces (" "), and pipe ("|")
##'     characters. Pipes indicate the beginnings of regions. By
##'     default, the region definition file will specify that each
##'     line be exhaustively dividied into space delimited regions
##'     (i.e. there will be a pipe character corrponding to each space
##'     character in the paired text line.
##'
##'     The region definition file can be hand edited to add or
##'     correct information in the yaml block, or to re-specify region
##'     placements.
##'
##'     ToDo: Make to work with region definition files for multi-line
##'     text stims.
##'
##' @param fname A string containing the name of a "region definition"
##'     file, such as might be created by reg2regdef(). See Details.
##'
##' @return A vector of strings containing the region definition. The
##'     vector includes a yaml block with values for each of the
##'     function parameters except for "reg". In addition to the yaml
##'     block, the vector will include a pair of lines for each line
##'     of text in the stimulus. The first element of each pair is the
##'     text displayed on that line. The second element is a regioning
##'     string made up of spaces (" "), and pipe ("|")
##'     characters. Pipes indicate the beginnings of regions. By
##'     default, the region definition file will specify that each
##'     line be exhaustively dividied into space delimited regions
##'     (i.e. there will be a pipe character corrponding to each space
##'     character in the paired text line.
##'
##'     This vector can be written to file and hand edited to add or correct information in the yaml
##'     block, or to re-specify region placements.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
regdef2ias <- function(fname) {
    l <- readLines(fname)

    ## get parameters from yaml block
    yidx <- which(stringr::str_detect(l, "^---$"))
    yml <- (min(yidx)+1):(max(yidx)-1)
    yml <- paste(l[yml], collapse="\n")
    parms <- yaml::yaml.load(yml)

    ## get regdef block
    tstart <- max(yidx)+1
    tend <- length(l)
    tblock <- l[tstart:tend]
    tidx <- stringr::str_detect(tblock, "^ *$") ## find blank lines

    ## drop leading blank lines from regdef block
    if (min(which(tidx))==1) {
        tmp <- FDButils::series(which(tidx), minseries=1)
        tstart <- tmp[1,1] + tmp[1,3]
        tend <- length(tblock)
        tblock <- tblock[tstart:tend]
    }

    ## find the (vertical) beginnings and ends of regions, in character units
    txt <- tblock[1]
    mrk <- tblock[2]
    midx <- stringr::str_locate_all(mrk, "[|]")[[1]][,1]
    x1_char <- c(1, midx)-1
    x2_char <- c(midx-1, str_length(txt))

    ## find the (vertical) beginnings and ends of regions, in pixel units
    x1 <- (x1_char * 12) + (parms$margins$left - 1) ## translate char to pix
    if (!is.na(parms$regions$padL)) x1[1] <- x1[1] - parms$regions$padL
    x2 <- (x2_char * 12) + (parms$margins$left - 1) ## translate char to pix
    if (!is.na(parms$regions$padR)) x1[length(x1)] <- x1[length(x1)] - parms$regions$padR

    ## get the upper and lower y coordinates (pixels) of regions
    y1 <- parms$lines$baseline[1] - parms$regions$maxH
    y2 <- parms$lines$baseline[1] + parms$regions$minH

    ## other columns of ias file
    type <- "RECTANGLE"         ## region type
    regnum <- 1:length(x1)      ## region numbers
    labs <- txt                 ## region labels
    labs <- stringr::str_replace_all(labs, " ", "_")
    labs <- stringr::str_replace_all(labs, '"', "'")
    labs <- stringr::str_sub(labs, x1_char+1, x2_char)

    ias <- data.frame(type, regnum, x1, y1, x2, y2, labs)
    ias
}
