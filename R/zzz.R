# Use .onLoad for side effects of package.
.onLoad <- function(lib,pkg) {
    ## See HW page for thoughts on what to do here: http://r-pkgs.had.co.nz/r.html
}

# Use .onAttach for informative messages.
.onAttach <- function(lib, pkg) {

    packageStartupMessage("    Welcome to ", pkg, ".")

    ## Better to build the startupMessage by reading the Description file, as in:
    ## ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
    ## ver <- as.character(ver)
    ## packageStartupMessage(paste("Welcome to",  pkg, ", version ", ver, "."))

    packageStartupMessage("    (c) 2014-2016, Dave Braze, and others.")
    packageStartupMessage("    Released under the MIT license.\n")

    ## check here that path to edf2asc is set. If not, caution user
    ## and point to documentation for FDBeye::edf2asc() for help.
    
    # detect operating system
    info <- sessionInfo()
    
    # retrieve the path to the edf2asc utility
    if (grepl('mac', info$running, ignore.case = TRUE)) {
      edf2asc_dir <- system2("which", "edf2asc", stdout = TRUE)
      edf2asc_exe <- edf2asc_dir[1]
    } else if (grepl('win', info$running, ignore.case = TRUE)) {
      edf2asc_dir <- system2("where", "edf2asc.exe", stdout = TRUE)
      edf2asc_exe <- edf2asc_dir[1]
    } else {edf2asc_exe <- NA}
    
    if(!grepl("edf2asc", edf2asc_exe)){
      packageStartupMessage(paste("    SR Research's edf2asc utility is not on PATH.",
                                  "    It must be set before calling FDBeye::edf2asc().",
                                  "    See help for that function.",
                                  sep="\n"))
    } else {
      # Check if the edf2asc utility exists and is executable
      # base::file.access() returns values 0 for success and -1 for failure
      if(unname(base::file.access(edf2asc_exe, mode=0))!=0){
        packageStartupMessage(paste(edf2asc_exe, 
                                    "... File does not exist.", 
                                    "See help for FDBeye::edf2asc().",
                                    sep="\n"))
        }
      if(unname(base::file.access(edf2asc_exe, mode=1))!=0){
        packageStartupMessage(paste(edf2asc_exe, 
                                    "... File is not executable.", 
                                    "See help for FDBeye::edf2asc().",
                                    sep="\n"))
        }
    }

    ## maybe call citation("FDBeye")

    invisible()
}

