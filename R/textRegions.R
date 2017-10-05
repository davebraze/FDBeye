## Work toward a flexible method of defining arbitrary scoring regions for multi-line texts.
## Also see yaml-test.txt system.file("extdata/yaml-test.txt")

##' @title Convert region file to region definition file.
##'
##' @description Convert a file containing full region description
##'     into a region definition file. The latter is suitable for hand
##'     editing and can be used to generate alternative region
##'     specifications (e.g., multi word regions) for text stimuli.
##'
##' @details We aspire to handle input files (region files) for multi
##'     line texts, but at present only region files for single line
##'     texts are handled.
##'
##'     File parameter values are used to fill in parameters written
##'     to the yaml block of the region definition file. Note that few
##'     of these parameters are important when the region definition
##'     file is used to create a new region file. Three parameters are
##'     critical to proper region definitions.
##'
##'     \describe{
##'
##'     \item{chrW}{This parameter is used to translate region
##'     boundaries in x dimension from letter positions (as specified
##'     in the region definition file) to pixel positions (as required
##'     for the region or ias file). If not specified in the function
##'     call, chrW is estimated from contents of region.csv file, and
##'     will probably be correct most of the time. Regardless, it
##'     should be checked and corrected if necessary (i.e., specified
##'     in the function call).}
##'
##'     \item{baseline}{Accurate baseline positions are also critical
##'     to determining the y positions of regions. Baselines are
##'     read directly from the region.csv file and should be
##'     accurate. Note that baseline positions, in pixels, are
##'     measured from the TOP of the screen.}
##'
##'     \item{mrgn.left}{The left margin is an x offset that will be
##'     applied to all regions. There is no easy way to read this from
##'     a region file, so it will need to be specified in the function
##'     call. In most cases, it will be the same for all stimulus
##'     items.}
##'
##'     }
##'
##'     Four optional parameters (rgn.minH, rgn.maxH, rgn.padL,
##'     rgn.padR) can be used to control various aspects of region
##'     extent.
##'
##' \strong{To Do:}
##'
##'     ToDo: This function presently does not work for regioning
##'     multi-line text stimuli. Fix that.  stims.
##'
##'     ToDo: Make a simuliar function to read/parse SRR IAS files and
##'     build region defs based on them.
##'
##' @param reg A data.frame containing region specifications, as read
##'     from a region file ("*.region.csv").
##' @param scrnW Screen width in pixels (integer).
##' @param scrnH Screen height in pixels (integer).
##' @param fnt.name Font name used for stimulus text.
##' @param fnt.size Nominal font size in points for text display.
##' @param chrW Letter width in pixels.
##' @param chrH Nominal letter height in pixels.
##' @param ln.space Line spacing in pixels for multi line texts. Multi
##'     line texts are not currently supported.
##' @param baseline Baseline positions for each line of text. Measured
##'     in pixels from the top of the screen. Multi line texts are not
##'     currently supported.
##' @param mrgn.top Top margin in pixels.
##' @param mrgn.left Left margin in pixels.
##' @param mrgn.bottom Bottom margin in pixels.
##' @param mrgn.right Right margin in pixels.
##' @param rgn.maxH Extent of regions of interest above baseline in
##'     pixels.
##' @param rgn.minH Extent of regions of interest below baseline in
##'     pixels.
##' @param rgn.padL Expand leftmost region on each line leftward by
##'     this amount in pixels.
##' @param rgn.padR Expand rightmost region on each line rightward by
##'     this amount in pixels.
##' @return A vector of strings containing the region definition. The
##'     vector includes a yaml block with values for each of the
##'     function parameters except for "reg". In addition to the yaml
##'     block, the vector will include a pair of lines for each line
##'     of text in the stimulus. The first element of each pair is the
##'     text displayed on that line. The second element is a regioning
##'     string made up of dots ("."), and pipe ("|")
##'     characters. Pipes indicate the beginnings of regions. By
##'     default, the region definition file will specify that each
##'     text line be exhaustively dividied into space delimited regions
##'     (i.e. there will be a pipe character corresponding to each space
##'     character in the paired text line.
##'
##'     This vector can be written to file and hand edited to add or
##'     correct information in the yaml block, or to re-specify region
##'     placements.
##' @seealso \code{\link{regdef2ias}}
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
    txt <- stringr::str_c(reg$Word, collapse="")
    idx <- stringr::str_locate_all(txt, " ")[[1]][,1]
    regmarks <- rep(".", stringr::str_length(txt))
    regmarks[idx] <- "|"
    regmarks <- paste(regmarks, collapse="")
    ln <- c(paste0("\n", txt, "\n", regmarks, "\n"))

    retval <- c(hdr, ln)
    retval
}


##' @title Convert region definition file to SRR Interest Area file.
##'
##' @description Convert a region definition file to an SRR Interest
##'     Area file (*.ias). The latter can be hand edited to specify
##'     alternative region specifications (e.g., multi word regions)
##'     for text stimuli.
##'
##' @details \code{regdef2ias} can handle region definitions for
##'     either single-line or multi-line texts.
##'
##'     Parameter values are read from the yaml block of the region
##'     definition file. Note that a few of these parameters are
##'     important when translating region definitions to interest
##'     areas.
##'
##'     A region definition file contains 2 parts. The first is a yaml
##'     block, which is followed by a region block.
##'
##' \strong{Yaml Block:}
##'
##'     Three parameters in the yaml block are critical to proper
##'     region definitions. Four others are also useful, but optional.
##'
##'     \describe{
##'
##'     \item{character$width}{This parameter is used to translate
##'     region boundaries in x dimension from letter positions (as
##'     specified in the region definition file) to pixel positions
##'     (as required for the region or ias file). Character width (in
##'     pixels) is not explicitly encoded in a region file, but is
##'     typically estimated from contents of region.csv file, and will
##'     probably be correct most of the time. Regardless, it should be
##'     checked and, if necessary, manually edited in the resulting
##'     region definition file.}
##'
##'     \item{lines$baseline}{Accurate baseline positions are also
##'     critical to determining the y positions of regions. Baselines
##'     are read directly from the region.csv file and should be
##'     accurate. Note that baseline positions, in pixels, are
##'     measured from the TOP of the screen.}
##'
##'     \item{margins$left}{The left margin is an x offset that will be
##'     applied to all regions. There is no easy way to read this from
##'     a region file, so it will need to be specified in the function
##'     call. In most cases, it will be the same for all stimulus
##'     items.}
##'
##'     }
##'
##'     Four additional parameters in the yaml block of the region
##'     definition file will be used to modify regions. They are:
##'     regions$maxH, regions$minH, regions$padL, and regions$padR.
##'
##'     Before running \code{regdef2ias} on a file, its yaml block can
##'     be hand edited to add or correct information. However, the
##'     easiest way to fill information in the yaml block will
##'     probably be to specify it in the form of parameters to
##'     \code{reg2regdef} or similar function used to create the
##'     region definition file in the first place.
##'
##' \strong{Region Block:}
##'
##'     The second part of a region definition file is the region
##'     block. This block contains a pair of lines for each line of
##'     text in the stimulus. The first element of each pair is the
##'     text displayed on that line. The second element is a regioning
##'     string made up of square brackets ("[", "]"), and pipe ("|")
##'     characters. An opening bracket ("[") indicates the start of a
##'     line and a closing bracket ("]) indicates the end of a line.
##'     Pipes ("|") indicate region boundaries within a line. By
##'     default, the region definition file will specify that each
##'     text line be exhaustively divided into space delimited regions
##'     (i.e. there will be a pipe character corrponding to each space
##'     character in the paired text line.)
##'
##'     Pairs of lines are required to be separated by at least one
##'     blank line for \code{regdef2ias} to parse multi-line texts.
##'
##'     Before running \code{regdef2ias} on a file, its region block
##'     can be hand edited to add or correct information to specify
##'     region placements.
##'
##' @param fname A string containing the name of a "region definition"
##'     file, such as might be created by reg2regdef(). See Details.
##'
##' @return A data.frame specifying the content of an SRR interest
##'     area file (*.ias).  Use \code{readr::write_delim(...,
##'     delim="\n", col_names=FALSE)} to save the interest area
##'     specification to file.
##' @seealso \code{\link{reg2regdef}}
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @author Monica Li \email{monica.yc.li@@gmail.com}
##' @export
##' @examples
##' regdef_story1 <- system.file("extdata/story01-regdef.txt", package="FDBeye")
##'
##' ## Peek at regdef file.
##' peek <- readLines(regdef_story1)
##'
##' ## Generate an SRR DV compatible IAS file from the regdef file.
##' ias_story1 <- regdef2ias(regdef_story1)


regdef2ias <- function(fname) {
    l <- readLines(fname)

    ## get parameters from yaml block
    yidx <- which(stringr::str_detect(l, "^---$"))
    yml <- (min(yidx)+1):(max(yidx)-1)
    yml <- paste(l[yml], collapse="\n")
    parms <- yaml::yaml.load(yml)

    ## check if crucial parameters are in the yaml block
    parms_check <- list(parms$margins$left,
                        parms$regions$maxH,
                        parms$regions$minH,
                        parms$lines$baseline)
    if (sum(sapply(parms_check, is.null, simplify = TRUE)) > 0) {
      stop(paste("At least one of the following parameters are missing in the yaml block:",
                 "margins: left, regions: maxH, regions: minH, lines: baselines",
                 sep = "\n"))
    }
    
    ## get regdef block
    tstart <- max(yidx)+1
    tend <- length(l)
    tblock <- l[tstart:tend]

    ## drop leading and ending blank lines from regdef block
    tblock <- c("",tblock,"") # pad blank lines to regdef block
    tidx <- stringr::str_detect(tblock, "^ *$") ## find blank lines
    tmp <- FDButils::series(which(tidx), minseries=1)
    tstart <- tmp[1,1] + tmp[1,3]
    tend <- tmp[nrow(tmp)]-1
    tblock <- tblock[tstart:tend]

    ## find line numbers for text
    tidx <- stringr::str_detect(tblock, "^ *$") ## find blank lines
    sep <- FDButils::series(which(tidx), minseries=1)
    text_line <- c(1, sep[,1]+sep[,3])

    ## check if number of lines matches between yaml-block defined baselines and regdef-block text
    if (length(parms$lines$baselines)!=length(text_line)) {
      stop("The number of baselines (defined in the yaml block) and the number of lines in text do not match.")
    }
    
    ## set up ias data.frame to store regdef info
    ias <- data.frame()

    ## iterate over text lines
    for (i in seq(length(text_line))) {
        ## find the (vertical) beginnings and ends of regions, in character units
        txt <- tblock[text_line[i]]
        mrk <- tblock[text_line[i]+1]
        if(stringr::str_length(txt) != stringr::str_length(mrk)) {
            tt <- paste0("\n  [", txt, "]")
            mm <- paste0("\n  [", mrk, "]")
            ww <- paste("Warning! region mark line is not same length as text line in" ,
                        fname, tt, mm)
            warning(ww)
        }
        midx <- stringr::str_locate_all(mrk, "[|]")[[1]][,1]
        x1_char <- c(1, midx)-1
        x2_char <- c(midx-1, stringr::str_length(txt))

        ## find the (vertical) beginnings and ends of regions, in pixel units
        x1 <- (x1_char * 12) + (parms$margins$left - 1) ## translate char to pix
        if (!is.na(parms$regions$padL)) x1[1] <- x1[1] - parms$regions$padL
        x1 <- as.integer(x1)
        x2 <- (x2_char * 12) + (parms$margins$left - 1) ## translate char to pix
        if (!is.na(parms$regions$padR)) x2[length(x2)] <- x2[length(x2)] + parms$regions$padR
        x2 <- as.integer(x2)

        ## get the upper and lower y coordinates (pixels) of regions
        y1 <- as.integer(parms$lines$baseline[i] - parms$regions$maxH)
        y2 <- as.integer(parms$lines$baseline[i] + parms$regions$minH)

        ## other columns for current text/mark line
        type <- "RECTANGLE"         ## region type
        regnum <- as.integer(1:length(x1))+nrow(ias)      ## region numbers
        labs <- txt                 ## region labels
        labs <- stringr::str_replace_all(labs, " ", "_")
        labs <- stringr::str_replace_all(labs, '"', "'")
        labs <- stringr::str_sub(labs, x1_char+1, x2_char)

        ## append data to ias data.frame
        ias <- rbind(ias, data.frame(type, regnum, x1, y1, x2, y2, labs))
    }
    ias
}


##' @title Convert SR Research IAS (Interest Area Set) file to region
##'   definition file.
##' @param ias.file A SR Research IAS ("*.ias") containing region
##'   specifications. Columns in file from left to right must be:
##'   Interest Area Type (Rectangular), Region Number, x1 (left
##'   boundary), y1 (top boundary), x2 (right boundary), y2 (bottom
##'   boundary), Region Label.
##' @author Monica Li \email{monica.yc.li@@gmail.com}
##' @export

ias2regdef <- function(ias.file, reg.sep=NA,
                       scrnW=NA, scrnH=NA,
                       fnt.name=NA, fnt.size=NA,
                       chrW=NA, chrH=NA,
                       ln.space=NA, baseline=NA,
                       mrgn.top=NA, mrgn.left=NA, mrgn.bottom=NA, mrgn.right=NA,
                       rgn.maxH=NA, rgn.minH=NA, rgn.padL=NA, rgn.padR=NA) {
    ##### read in ias file #####
    ias <- read.table(ias.file, header = FALSE, sep = "\t", comment.char = "#",
                    col.names = c("type","regnum","x1","y1","x2","y2","labs"))
    
    ##### build yaml block #####
    if (is.na(baseline)) baseline <- unique(ias$y2)
    if (is.na(chrH)) chrH <- mean(ias$y2-ias$y1)
    if (is.na(chrW)) {
      chrW <- diff(ias$x1)
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

    ##### build regdef block #####
    # create an empty vector to store text and region definition lines
    ln <- c()
    # loop over each line of text
    for (y in unique(ias$y2)){
        # use y2 to identify text on the same line
        ias_line <- subset(ias, y2==y)
        
        # read in text and determine where the separators are
        # replace separators with spaces
        if (is.na(reg.sep)){
          txt <- stringr::str_c(ias_line$labs, collapse="\t")
          idx <- stringr::str_locate_all(txt, "\t")[[1]][,1]
          txt <- gsub("\t", " ", txt)
        } else {
          txt <- stringr::str_c(ias_line$labs, collapse="")
          idx <- stringr::str_locate_all(txt, reg.sep)[[1]][,1]
          txt <- gsub(reg.sep, " ", txt)
        }
        
        # create corresponding region marks for each line of text
        regmarks <- rep(" ", stringr::str_length(txt))
        regmarks[idx] <- "|"
        regmarks[1] <- "["
        regmarks[length(regmarks)] <- "]"
        regmarks <- paste(regmarks, collapse="")
        
        # stack text and region definition lines
        ln <- c(paste0(ln, "\n", txt, "\n", regmarks, "\n"))
    }
    
    retval <- c(hdr, ln)
    retval
}