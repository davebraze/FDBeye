##' @title Fortify SRR interest areas.
##'
##' @description Fortify SRR interest areas for text stimuli with
##'     interest area heights, first word locations, and line numbers.
##'
##' @details This function takes a data.frame containing details of
##'     SRR compliant rectangular interest areas and fortifies them
##'     with interest area heights, first word locations, and line
##'     numbers. The assumption is that these interest areas are
##'     defined over for a text stimulus (e.g., each interest area
##'     contains a word from a multi-line text).
##'
##' @seealso patchIAy
##'
##' @param .data A data.frame containing a set of rectangular interest
##'     areas. Typically these will contain at least 7 columns
##'     for: type, index x_left, y_top, x_right, y_bottom, label.
##' @param y1 Label of column in .data containing y values for upper
##'     bound of interest areas.
##' @param y2 Label of column in .data containing y values for lower
##'     bound of interest areas.
##' @return A data.frame containing interest areas, now enriched with
##'     columns for:
##'
##' \describe{
##'     \item{iaHeight}{Interest area height in integer pixels.}
##'     \item{firstWordInLine}{Logical flag picking out the first word (IA) on each line.}
##'     \item{lastWordInLine}{Logical flag picking out the last word (IA) on each line.}
##'     \item{lineNum}{Integer indicating line number for word (IA), ranging from 1 to n where n is the number of lines.}
##' }
##'
##' @author Dave Braze <davebraze@@gmail.com>
##' @export
##' @examples
##' \dontrun{
##'
##' library(dplyr)
##' library(readr)
##'
##' ias0 <- read_tsv(system.file()) ## add an actual SRR IA file to /inst/extdata
##' glimpse(ias0)
##'
##' ias1 <- fortifySRias(ias0, y1=y_top, y2=y_bottom)
##' glimpse(ias1)
##'
##' }
fortifySRias <- function(df, y1=y1, y2=y2) {
    y1 <- enquo(y1)
    y2 <- enquo(y2)
    retval <- df %>%
        mutate(iaHeight = abs((!! y2) - (!! y1)))  %>%
        mutate(lineStart = c(TRUE, diff(!! y1) > min(iaHeight)))
    ## Maybe take an argument to tune the value of iaHeight
    ## used to ID first ia in each line.

    fwidx <- which(retval$lineStart) ## indices of ias at beginnings of lines
    lines <- order(fwidx) ## line numbers

    ## expand line numbers to each ias
    repet <- c(diff(fwidx), length(retval$lineStart)-sum(diff(fwidx)))
    lineNum <- rep(lines, repet) ## line number for each interest area

    retval <- add_column(retval, lineNum)
    retval <- retval %>%
        group_by(lineNum) %>%
        mutate(nWordInLine = 1:n()) %>%
        mutate(lineEnd = nWordInLine == max(nWordInLine))

    retval
}
