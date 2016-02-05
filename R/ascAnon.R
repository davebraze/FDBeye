##' @title Remove potentially identifying information from SRR *ASC file.
##'
##' @description Remove potentially identifying information from header of an SRR *ASC file. Write
##'     the new file to newName.asc in the same directory as the source file. This is a superficial
##'     method of anonymization, but it should do for most cases.
##'
##' @details No Details.
##'
##' @param file Path to SRR *ASC file.
##' @param newName
##' @return Filename and path to anonymized file.
##' @author Dave Braze <davebraze@@gmail.com>
ascAnon <- function(file,
                    newName=NULL) {
    ## Make these changes to file header.
    ## o set all dates to Jan 01 2000
    ## o set all times to 12:34:56
    ## o set 'CONVERTED FROM' fname to /a/b/c/newName.edf. If NULL use basename of EDF file.

}
