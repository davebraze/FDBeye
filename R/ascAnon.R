##' @title Remove potentially identifying information from SRR *ASC file.
##'
##' @description Remove potentially identifying information from header of an SRR *ASC file. Write
##'     the new file to the current working directory. This is a superficial
##'     method of de-identification or anonymization, but it should do for most cases.
##'
##' @details By default, newly created anonymous *ASC file will will use the same name as the source
##'     *ASC file, but with "-anon" appended. In this case, the same base filename will be used as
##'     the name of the *EDF filename in the "CONVERTED FROM" line of the anonynmized *ASC
##'     file. This is problematic in that, under our usual operating procedures, the filename itself
##'     usually consists of an identifier that is uniquely traceable to the participant. In order to
##'     make the file truly anonymous, the newName argument must be used.
##'
##'     If newName is provided, that will be used as the basename for the EDF file in the CONVERTED
##'     FROM line, as well as the output file name.
##'
##' @param file Path to SRR *ASC file.
##' @param newName Name to use for newly anonymized *ASC file. If an extension is included it will
##'     be ignored and replaced with "asc". If NULL (the default) will use the same name as the
##'     source *ASC file, but with "-anon" appended. In either case the same base filename will be
##'     used as the name of the *EDF filename in the "CONVERTED FROM" line of the anonynmized *ASC
##'     file.
##' @return Filename and path to anonymized file.
##' @author Dave Braze <davebraze@@gmail.com>
##' @export
##' @examples
##' file <- system.file("extdata/1950168.asc", package="FDBeye")
##' ascAnon(file)
##' ascAnon(file, newName="fred")
ascAnon <- function(file,
                    newName=NULL) {

    ## open the *ASC file and read it
    h <- file(file, "r", blocking=FALSE)
    asc <- readLines(h, warn=TRUE, n=-1)
    close(h)

    ## replace the file name in CONVERTED FROM
    convertedLine <- grep("^[*]{2} CONVERTED FROM", asc)
    fname <- basename(sub("^([*]{2} CONVERTED FROM )(.+)( using edf.+)$", "\\2", asc[convertedLine]))
    if (is.null(newName)) {
        print("WARNING: Filename may contain potentially identifying information.")
        print("         Consider calling this function with a non-NULL 'newName' argument.")
        n <- tools::file_path_sans_ext(fname) # TODO add option to remove PART of fname via regex
        e <- tools::file_ext(fname)
        fn <- paste0(n, "-anon.", e)
        outfile <- paste0(n, "-anon.asc")
    } else {
        n <- tools::file_path_sans_ext(basename(newName))
        fn <- paste0(n, ".edf")
        outfile <- paste0(n, ".asc")
    }
    fnRE <- paste0("\\1",  file.path(".", "anonymous", fn), "\\3")
    asc[convertedLine] <- sub("^([*]{2} CONVERTED FROM )(.+)( using edf.+)$",
                              fnRE,
                              asc[convertedLine]) # replace path/fname.edf

    ## replace date in CONVERTED FROM
    wday <- "(Mon|Tue|Wed|Thu|Fri|Sat|Sun)"
    month <- "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
    mday <- "[0-9]{2}"
    time <- "[0-9]{2}:[0-9]{2}:[0-9]{2}"
    year <- "[0-9]{4}"
    asc[convertedLine] <- sub(paste(wday, month, mday, time, year, sep=" "),
                              "Sat Feb 30 12:34:56 2001",
                              asc[convertedLine])

    ## replace data in DATE:
    dateLine <- grep("^[*]{2} DATE:", asc)
    asc[dateLine] <- sub(paste(wday, month, mday, time, year), "Sat Feb 30 12:34:56 2001", asc[dateLine])


    ## open new text file and write it. Append "-ascAnon" to the name
    ## build path to outfile, including fname
    outfile <- file.path(".", outfile)
    h <- file(outfile, "w", blocking=FALSE)
    asc <- writeLines(text=asc, con=h)
    close(h)

    return(outfile)
}
