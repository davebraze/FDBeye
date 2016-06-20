.onload <- function(lib, pkg) {

    packageStartupMessage("Welcome to ", pkg, ".")

    ## Better to build the startupMessage by reading the Description file, as in:
    ## ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
    ## ver <- as.character(ver)
    ## packageStartupMessage(paste("Welcome to",  pkg, ", version ", ver, "."))

    packageStartupMessage("(c) 2016 Dave Braze, and others.")
    packageStartupMessage("Released under the MIT license.")

    ## check here that path to edf2asc is set. If not, caution user
    ## and point to documentation for FDBeye::edf2asc() for help.

    ## What else? See HW page for thoughts: http://r-pkgs.had.co.nz/r.html

    invisible()
}

