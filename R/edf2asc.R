##' @title Call SRR "edf2asc" command line utility to do some work.
##'
##' @description A convenience wrapper around the SR Research edf2asc file conversion utility.
##'
##' @details Call SR Research "edf2asc" command line utility to convert files. This function expect
##'     to find the full path to the edf2asc executable via getOption("FDBeye_edf2asc_exec"). The
##'     user can set the option by calling option(FDBeye_edf2asc_exec = "/path/to/edf2asc.exe"),
##'     either in the script itself or, more permanently, in their .Rprofile file. Before calling
##'     the utility, this function will check to see that the specified file exists and is
##'     executable. As a meager security check, we also ensure that the basename of the file
##'     includes the string "edf2asc".
##'
##' \enumerate{
##'
##'     \item The best way to get the edf2asc utility is to install the Eyelink Developers Kit:
##'             https://www.sr-support.com/showthread.php?6-EyeLink-Developers-Kit-for-Windows-%28Windows-Display-Software%29.
##'     \item Documentation is in the EL1000+ manual, section 4.8 "Using ASC files".
##'     \item Make sure edfapi library (e.g., edfapi.dll) is somewhere on the PATH.
##'
##' }
##'
##' This function captures the output of the edf2asc untility and writes it to a logfile,
##' "edf2asc.log", in the current working directory.
##'
##' @param edffiles Character vector of *edf file names to be converted.
##' @return A character vector listing output files.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
edf2asc <- function(edffiles) {

    exe <- getOption("FDBeye_edf2asc_exec")
    opts <-  getOption("FDBeye_edf2asc_opts")
    if(is.null(opts)) {
        warning("FDBeye_edf2asc_opts not set. Using factory defaults.")
        opts <- ""
    }

    if(is.null(exe)){
        stop("You must set option(FDBeye_edf2asc_exec = '/path/to/edf2asc') before calling this function.")
    } else {
        ## First check to be sure the file actually exists and is executable
        if(!file_test("-f", exe))        # test whether file exists and is executable. Should I use base::file.exists() instead?
            stop(paste(exe, "File either does not exist or is not executable.", sep="\n"))
        ## parse the specified path and make sure the fname includes "edf2asc"
        if(!grepl("edf2asc", basename(exe)))
            stop(paste(exe, "File does not appear to be an edf2asc utility.", sep="\n"))
    }

    for (ff in edffiles) {
        ## see R function shQuote() for help building the command line string.
        log <- system(paste(shQuote(exe), opts, shQuote(ff)),
                      intern=TRUE)
        if(exists("logfile")) logfile <- c(logfile, log)
        else logfile <- log
    }

    ## should wrap this in a 'try' block.
    logfile <- logfile[-grep("^Processed", logfile)]
    h <- file("./edf2asc.log", "wb")
    cat(logfile, file=h, sep="\n")
    close(h)

    retval <- gsub("\\.edf$", ".asc", edffiles)
    retval
}
