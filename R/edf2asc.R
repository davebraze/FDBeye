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
##' @param e2afname Name of edf2asc executable (e.g., "edf2asc.exe", "edf2asc64.exe"). FIXME!!! See e2apath.
##' @param e2apath Path to edf2asc executable. It will probably look something like
##'     "D:/winbin/SR Research/EyeLink/EDF_Access_API/Example" from "SR Research" on down.  FIXME!!!
##'     Best not to specifiy the path as an argument. Rather, implement 2 parallel solutions:
##'
##'     1. Assume that executable is on PATH. But check before calling.
##'
##'     2. Use s.t. have user set an option using options(), options(FDBeye_edf2asc_exec =
##'     "fully_qualified_path_to_executable") in .First function within .Rprofile.
##' @return A character vector listing output files.
##' @author Dave Braze \email{davebraze@@gmail.com}
edf2asc <- function(edffiles,
                    e2afname="edf2asc64.exe",
                    e2apath="D:/winbin/SR Research/EyeLink/EDF_Access_API/Example") {
    ## o use R function system() to make the call, but also see shell(), shell.exec() and, possibly, system2()
    ## o see R function shQuote() for help building the command line string.

    ## First check options() to see if an exec is specified there. If so use it. Otherwise, use
    ## Sys.which("edf2asc") and Sys.which("edf2asc64") to determine if either exec is available in
    ## the PATH.  If both are present, use edf2asc64, if more than 1 copy is present, check versions
    ## and use the most current. If more than 1 copy of the most current is available, use the
    ## first.
}
