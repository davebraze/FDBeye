## Work toward a flexible method of defining scoring regions for multi-line texts.
## Also see yaml-test.txt system.file("extdata/yaml-test.txt")

if(FALSE) {
    library(yaml)

    l <- readLines(system.file("extdata/yaml-test.txt")

    ## Find the "---" demarcated yaml block
    ## of parameters and read it.
    pblock <- grep("---", l)
    pblock <- (min(pblock)+1):(max(pblock)-1)
    pblock <- paste(l[pblock], collapse="\n")
    parms <- yaml.load(yml)
    ## The parameters that matter most are
    ## font$baselines
    ## margins$left
    ## character$width
    ## need to determine region H somehow.
    ## 1. based on proportion of baseline to baseline distance
    ## 2. or allow setting a parameter

    ## Documentation of parameters
    ## screen:
    ##   width: <int> screen width in pixels
    ##   height: <int> screen height in pixels
    ## font:
    ##   name: <char> font name
    ##   size: <float> nominal font size in points, as displayed on screen
    ## character:
    ##   width: <int> font width in pixels
    ##   height: <int> font height in pixels
    ## lines:
    ##   spacing: <int> line spacing in pixels
    ##   baselines: <int sequence> locations of baselines for each line of text given in pixels from top of screen.
    ## margins:
    ##   top: <int> distance from top of screen to top of 1st line of text in pixels
    ##   left: <int> distance from left edge of screen to left edge of 1st character in line of text, in pixels.
    ##   bottom: <int>
    ##   right: <int>
    ## regions:
    ##   maxH: <int> extent of scoring region above baseline in pixels. Good default probably 1/2(lines$spacing)
    ##   minH: <int> extent of scoring region below baseline in pixels. Good default probably 1/2(lines$spacing)
    ##   padL: <int> extent to pad 1st region on line to left of 1st character, in pixels.
    ##   padR: <int> extent to pad last region on line to right of last character, in pixels.

    ## Find the text to be regioned
    ## together with region markers.
    ## Ignore blank lines.
    tstart <- max(grep("---", l))+1
    tend <- length(l)
    tblock <- l[tstart:tend]
    tblock <- tblock[-grep("^[ \t]*$", tblock)]

}
