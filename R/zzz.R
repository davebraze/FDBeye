# Use .onLoad for side effects of package.
.onLoad <- function(lib,pkg) {
    ## See HW page for thoughts on what to do here: http://r-pkgs.had.co.nz/r.html
}

# Use .onAttach for informative messages.
.onAttach <- function(lib, pkg) {

    packageStartupMessage("Welcome to ", pkg, ".")

    ## Better to build the startupMessage by reading the Description file, as in:
    ## ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
    ## ver <- as.character(ver)
    ## packageStartupMessage(paste("Welcome to",  pkg, ", version ", ver, "."))

    packageStartupMessage("    (c) 2014-2016, Dave Braze, and others.")
    packageStartupMessage("    Released under the MIT license.\n")

    ## check here that path to edf2asc is set. If not, caution user
    ## and point to documentation for FDBeye::edf2asc() for help.
    edf2asc <- getOption("FDBeye_edf2asc_exec")
    if(is.null(edf2asc)){
        packageStartupMessage(paste("    Option 'FDBeye_edf2asc_exec' is not set.",
                                    "    It must be set before calling FDBeye::edf2asc().",
                                    "    See help for that function.",
                                    sep="\n"))
    } else {
        ## First check to be sure the file actually exists and is executable
        if(!utils::file_test("-f", edf2asc)){        # test whether file exists and is executable. Should I use base::file.exists() instead?
            packageStartupMessage(paste(edf2asc,
                                        "... File either does not exist or is not executable.",
                                        sep="\n"))
            packageStartupMessage(paste("Option 'FDBeye_edf2asc_exec' must point to an executable file.",
                                        "See help for FDBeye::edf2asc().",
                                        sep="\n"))
            }
    }

    invisible()
}

