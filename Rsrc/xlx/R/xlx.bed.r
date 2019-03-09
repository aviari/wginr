# -------------------------------------------------
# $Id: xlx.bed.r 321 2017-08-18 11:10:19Z viari $
# xlx bed format
#

# =================================================
# API
# =================================================

# -------------------------------------------------
#' read bed regions from file
#' @description
#' read file in bed (0-based) format and return a dataframe
#' @param filename bed file name
#' @return nx3 dataframe with colnames: "chr" (character) "from" (integer), "to" (integer)
#' @note
#' \code{from, to} coordinates are 0-based
#' @seealso \link{bed2clocs}
#' @examples
#' bed <- bed.read(lx.system.file("samples/test.bed", "xlx"))
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' clocs <- bed2clocs(fh, bed)
#' basta.close(fh)
#'
bed.read <- function(filename) {

    lx.out("  reading bed lines: ", filename)
    bed <- read.table(filename, header=FALSE,
                                stringsAsFactors=FALSE,
                                col.names=.xlx.colnames(),
                                colClasses=c("character", "integer", "integer"))
    lx.out("  read ", nrow(bed), " bed regions")
    bed
}

# -------------------------------------------------
#' convert bed regions to matrix of clocations
#' @description
#' see \link{HELP.COORD} for help on coordinates systems
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open}
#' @param bed dataframe (as returned by \link{bed.read})
#' @param check check that boundaries are correct
#' @return a nx3 matrix of (1-based) clocations
#' @examples
#' bed <- bed.read(lx.system.file("samples/test.bed", "xlx"))
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' clocs <- bed2clocs(fh, bed)
#' basta.close(fh)
#'
bed2clocs <- function(handle, bed, check=TRUE) {  # check
    #
    # internal warning
    #
    .warn <- function(n0, n1, what, message) {
        lx.warnif(n0 != n1, what, " ", n0-n1, "/", n0,
                        "=", round((n0-n1)*100/max(n0,1)),
                        "% rows ", message,
                        up.frame=1)
    }
    #
    # make sure bed$chr is correctly formatted
    #
    storage.mode(bed$chr) <- "character"

    #
    # transform seq name -> seq index
    #
    lx.out("  transforming bed chrname to index")

    n0 <- nrow(bed)
    seq.index <- sapply(handle$header$seq, function(x) x$index, USE.NAMES=TRUE)
    seq.names <- unique(names(seq.index))
    bed.names <- unique(bed$chr)
    val.names <- intersect(seq.names, bed.names)
    bed <- bed[bed$chr %in% val.names,]
    .warn(n0, nrow(bed), "removed", "with invalid chr names")
    chrindex <- seq.index[bed$chr]

    if (nrow(bed) == 0)
      return(clocs.matrix(bed))

    #
    # more check
    #
    if (check) {
        lx.out("  checking boundaries")
        n0 <- nrow(bed)
        #
        # for compatibilty with version < 1.6
        # where header$sizes was absent
        #
        if (is.null(handle$header$sizes))
          handle$header$sizes <- sapply(handle$header$seq, function(x) x$size)
        #
        # check from >= 0 and from <= to and from <= size
        #
        sizes <- handle$header$sizes[bed$chr]
        valid <- (bed$from >= 0) & (bed$from <= bed$to) & (bed$from <= sizes)
        bed <- bed[valid,]
        chrindex <- chrindex[valid]
        .warn(n0, nrow(bed), "removed", "with invalid boundaries")
        
        if (nrow(bed) == 0)
          return(clocs.matrix(bed))
        #
        # check to <= size
        #
        sizes <- handle$header$sizes[bed$chr]
        over <- (bed$to > sizes)
        bed$to[over] <- sizes[over]
        .warn(n0, n0-sum(over), "truncated", "with invalid boundaries")
    }

    if (nrow(bed) == 0)
      return(clocs.matrix(bed))

    #
    # shape up
    #

    bed$from <- bed$from + 1L   # 1-based
    bed$chr <- chrindex
    
    clocs.matrix(bed)
}

