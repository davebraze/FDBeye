##' @title Call SRR "edf2asc" command line utility to do some work.
##'
##' @description A convenience wrapper around the SR Research edf2asc file conversion utility.
##'
##' @details Call SR Research "edf2asc" command line utility to convert *edf files to *asc
##'     files. Each *asc file is placed in the same directory as the *edf file that it is derived
##'     from. Existing *asc files will NOT be over-written by default, because that is the default
##'     for the SRR edf2asc utility.
##'
##'     To prepare edf2asc() to work with the edf2asc command line utility provided by SR Research,
##'     follow these steps:
##'     \itemize{
##'         \item{Mac OS X
##'             \enumerate{
##'                 \item{Download and install the Eyelink Developers Kit from SR Research: 
##'                     \url{https://www.sr-support.com/forum/downloads/eyelink-display-software/45-eyelink-developers-kit-for-mac-os-x-mac-os-x-display-software?15-EyeLink-Developers-Kit-for-Mac-OS-X=}
##'                     \item Documentation for using edf2asc utility is in the EyeLink 1000 User
##'                     Manual, section 4.8 "Using ASC files".
##'                 }
##'                 \item Identify the path to the edf2asc utility; something like
##'                 "/Applications/Eyelink/EDF_Access_API/Example"
##'                 \item In R, check if the path to the edf2asc utility is in the R environment by
##'                 running Sys.getenv("PATH"). If yes, you may skip the following steps. If not,
##'                 do the following.
##'                 \item Open (or create) a file called ".Renviron" in the home directory (applied
##'                 system-wise) or current directory (applied project-wise)
##'                 \item Add the path to the edf2asc utility to PATH in ".Renviron", followed by
##'                 the output from Sys.getenv("PATH"); for example, 
##'                 PATH="/Applications/Eyelink/EDF_Access_API/Example:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin"
##'                 \item You may need to restart R for the changes to apply.
##'             }
##'         } 
##'         \item{Windows
##'             \enumerate{
##'                 \item{Download and install the Eyelink Developers Kit from SR Research: 
##'                     \url{https://www.sr-support.com/forum/downloads/eyelink-display-software/39-eyelink-developers-kit-for-windows-windows-display-software?6-EyeLink-Developers-Kit-for-Windows-=}
##'                     \item Documentation for using edf2asc utility is in the EyeLink 1000 User
##'                     Manual, section 4.8 "Using ASC files".
##'                 }
##'                 \item Identify the path to the edf2asc utility; something like "C:/Program Files
##'                 (x86)/SR Research/Eyelink/EDF_Access_API/Example"
##'                 \item Make sure "edfapi.dll" and "edfapi.lib" are somewhere on the path, for
##'                 example "C:\\Windows\\System32" (see /path/to/EDF_Access_API/readme.txt for more
##'                 detail)
##'                 \item In R, check if the path to the edf2asc utility is in the R environment by 
##'                 running Sys.getenv("PATH"). If yes, you may skip the following steps. If not, do
##'                 the following.
##'                 \item Go to Control Panel > System and Security > Advanced System Settings > 
##'                 Environmental Variables...
##'                 \item{Under System Variables, choose Path, click Edit..., add the path to the
##'                 edf2asc utility to the list, and click OK (For more detail, see
##'                     \url{https://www.howtogeek.com/118594/how-to-edit-your-system-path-for-easy-command-line-access/})}
##'                 \item You may need to restart R for the changes to apply.
##'             }
##'         }
##'     }
##'
##'     Before calling the utility, this function will check to see that the specified file exists 
##'     and is executable. However, if the selected version of the edf2asc executable is in some way
##'     incompatible with your platform, then this function will fail with a cryptic error. The best
##'     way to guard against this is to check that your edf2asc executable file works as expected
##'     from the command line before attempting to use it from within FDBeye.
##'     
##'     The function edf2asc() also checks getOption("FDBeye_edf2asc_opts"). If this option exists, 
##'     it should be a valid string of command line options to pass to the SRR edf2asc utility 
##'     (e.g., "-y -ns"). See the SRR documentation for details. We recommend to use the "-y" option
##'     to overwrite existing *asc files; otherwise, edf2asc() may not work properly.
##'     
##'     In addition to creating the requested *asc files, this function will write a log file 
##'     ('edf2asc.log') of messages captured from the stdout of SRR edf2asc utility and place it in 
##'     the current working directory.
##'
##' @param edffiles Character vector of *edf file names to be converted. File names should include
##'     paths relative to the current working directory, or fully qualified paths.
##' @return Called for the side effect of converting SRR *edf files to *asc files. Returns a
##'     character vector listing output files (*asc files).
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @author Monica Li \email{monica.yc.li@@gmail.com}
##' @export
##' @examples
##' \dontrun{
##' fin <- list.files(".", pattern="edf$", recursive=TRUE)
##' fout <- edf2asc(fin)
##' fout
##' }

edf2asc <- function(edffiles) {
  # detect operating system
  info <- sessionInfo()
  
  # retrieve the path to the edf2asc utility
  if (grepl('mac', info$running, ignore.case = TRUE)) {
    edf2asc_dir <- system2("which", "edf2asc", stdout = TRUE)
    exe <- edf2asc_dir[1]
  } else if (grepl('win', info$running, ignore.case = TRUE)) {
    edf2asc_dir <- system2("where", "edf2asc.exe", stdout = TRUE)
    exe <- edf2asc_dir[1]
  } else {
    stop("Only Mac OSX and Windows are supported currently.")
  }
  
  if(!grepl("edf2asc", exe)){
    stop("You must add the edf2asc utility to PATH before calling this function.")
  } else {
    # Check if the file exists and is executable
    # base::file.access() returns values 0 for success and -1 for failure
    if(unname(base::file.access(exe, mode=0))!=0){stop(paste(exe, "... File does not exist.", sep="\n"))}
    if(unname(base::file.access(exe, mode=1))!=0){stop(paste(exe, "... File is not executable.", sep="\n"))}
  }

  # retrieve options
  opts <-  getOption("FDBeye_edf2asc_opts")
  
  if(is.null(opts)) {
    warning("FDBeye_edf2asc_opts not set. Using factory defaults.")
    opts <- ""
  }
  
  if(!grepl("-y", opts)) {
    warning(paste("Including option -y in FDBeye_edf2asc_opts is recommended to overwrite existing files.",
                  "Otherwise, program might not run properly.", sep="\n"))
  }

  # check if any file in edffiles is missing
  for (ff in edffiles) {
    if(!file.exists(ff)) {
      warning(paste("The following file does not exist: ", ff))
      edffiles <- edffiles[edffiles != ff]
    }
  }

  for (ff in edffiles) {
    if (grepl('mac|win', info$running, ignore.case = TRUE)) {
      ## see R function shQuote() for help building the command line string.
      log <- system2(exe,
                     args = shQuote(paste(opts,ff), type = "cmd2"),
                     stdout = TRUE)
    } else {
      stop("Only Mac OSX and Windows are supported currently.")
    }

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
