##' @title Modify upper and lower limits of Interest Areas.
##'
##' @description Modify upper and lower limits of Interest Areas.
##'
##' @details Modify upper and lower limits of Interest
##'     Areas. Modification is done in place in the sense that the
##'     returned data.frame has the same columns (and same number of
##'     columns) as the one passed to the function, but previously
##'     existing upper and lower bounds for interest areas are
##'     over-written with new values.
##'
##' @seealso fortifySRias
##'
##' @param df A data.frame containing a set of rectangular interest
##'     areas conforming to SRR standards. It will contain 7 columns
##'     for: type, index x_left, y_top, x_right, y_bottom, label. Note
##'     that SRR *ias files are TSV files without column
##'     labels. Labels should be added when reading the file.
##' @param y1 Label of column in df containing y values for upper
##'     bound of interest areas.
##' @param y2 Label of column in df containing y values for lower
##'     bound of interest areas.
##' @param lineNum Label of column in df indicating line number for
##'     each Interest Area.
##' @return A data.frame of interest areas with modified upper and
##'     lower limits.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @import dplyr
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
##' ias2 <- patchIAy(ias1)
##' glimpse(ias2)
##'
##' }
patchIAy <- function(df, y1=y1, y2=y2, lineNum=lineNum, stdize=TRUE, ytop=0, ybottom=0){
    y1 <- enquo(y1)
    y2 <- enquo(y2)
    lineNum <- enquo(lineNum)
    retval <- df

    if (stdize){
        retval <- retval %>%
            group_by(lineNum) %>%
            mutate(!!quo_name(y1) := as.integer(min((!! y1))),
                   !!quo_name(y2) := as.integer(max((!! y2))))
    }

    if ((ytop!=0) || (ybottom!=0)){
        retval <- retval %>%
            group_by(lineNum) %>%
            mutate(!!quo_name(y1) := as.integer((!! y1) - ytop),
                   !!quo_name(y2) := as.integer((!! y2) + ybottom))
    }
    ## Should update values of iaHeight, if they exist, before returning.
    retval
}
