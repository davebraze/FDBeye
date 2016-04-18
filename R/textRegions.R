## Work toward a flexible method of defining scoring regions for multi-line texts.
## Also see yaml-test.txt system.file("extdata/yaml-test.txt")

if(FALSE) {
    library(yaml)
    library(FDBeye)

    # l <- readLines(system.file("extdata/yaml-test.txt"), package="FDBeye")
    l <- readLines("../inst/extdata/yaml-test.txt")

    ## Find the "---" demarcated yaml block
    ## of parameters and read it.
    pblock <- grep("$---", l) # yaml blocks begin and end with a line starting with 3 hyphens.
    pblock <- (min(pblock)+1):(max(pblock)-1)
    pblock <- paste(l[pblock], collapse="\n")
    parms <- yaml.load(pblock)
    ## The parameters that matter most are
    ## font$baselines
    ## margins$left
    ## character$width
    ## need to determine region H somehow.
    ## 1. based on proportion of baseline to baseline distance
    ## 2. or allow setting a parameter

    ## Find the text to be regioned
    ## together with region markers.
    ## Ignore blank lines.
    tstart <- max(grep("---", l))+1
    tend <- length(l)
    tblock <- l[tstart:tend]
    tblock <- tblock[-grep("^[ \t]*$", tblock)]

}
