##' @title Convert region file to SR interest area file
##'
##' @description Convert a region file to an SR interest area file.
##'
##'@details No details yet.
##'
##' @param regfile Region file to read, formatted per Tao Gong's Python code.
##' @param iasfile If not FALSE (default), then this is a string specifying output filename. If
##' FALSE then create the output filename by stripping ext from regfile and adding ".ias".
##' @param size Extract "big" or "small" interast areas from region file.
##' @return Called for its side effect of writing an "*.ias" file.
##' @author Dave Braze \email{davebraze@@gmail.com}
##' @export
region2SRias <- function(regfile,
                         iasfile=FALSE,
                         size=c("big", "small")) {

    reg <- read.csv(regfile, as.is=T)
    if(!iasfile) {
        outfile <- tools::file_path_sans_ext(regfile)
        outfile <- paste(outfile, "ias", sep=".")
    } else {
        outfile <- iasfile
    }

    if (size=="big") {
        ias <- data.frame(type, reg[c("WordID", "b_x1", "b_y1", "b_x2", "b_y2", "Word")] )
    } else {
        ias <- data.frame(type, reg[c("WordID", "x1_pos", "y1_pos", "x2_pos", "y2_pos", "Word")] )
    }

     write.table(ias, outfile, qmethod="double", sep="\t", row.names=F, col.names=F)
}
