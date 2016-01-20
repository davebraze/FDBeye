##' @title Call "edf2asc" command line utility to do some work.
##'
##' @description No description yet.
##'
##' @details Call SR Research "edf2asc" command line utility to convert files.
##'
##' \enumerate{
##'     \item The best way to get the edf2asc utility is to install the Eyelink Developers Kit: https://www.sr-support.com/showthread.php?6-EyeLink-Developers-Kit-for-Windows-%28Windows-Display-Software%29.
##'     \item Documentation is in the EL1000+ manual, section 4.8 "Using ASC files".
##'     \item Make sure edfapi library (e.g., edfapi.dll) is somewhere on the PATH.
##' }
##'
##' @param edffiles Vector of *edf file names to be converted.
##' @param e2afname Name of edf2asc executable (e.g., "edf2asc.exe", "edf2asc64.exe").
##' @param e2apath Path to edf2asc executable. It will probably look something like
##' "D:/winbin/SR Research/EyeLink/EDF_Access_API/Example" from "SR Research" on down.
##' @return A character vector listing output files.
##' @author Dave Braze \email{davebraze@@gmail.com}
edf2asc <- function(edffiles, e2afname="edf2asc64.exe", e2apath="D:/winbin/SR Research/EyeLink/EDF_Access_API/Example") {
    ## o use R function system() to make the call, but also see shell(), shell.exec() and, possibly, system2()
    ## o see R function shQuote() for help building the command line string.

    ## maybe use > where edf2asc*.exe
    ## to determine if the utility is on the path (under Windows).
    ## With wildcard will find both edf2asc.exe and edf2asc64.exe if present.
    ##
    ## Similar command under *nix is "which"
}
