# -------------------------------------------------
# $Id: xlx.coords.r 377 2018-11-21 22:36:20Z viari $
# XLX coordinate systems
#
# @todo: make an S3 class Lcoation to wrap this stuff
#

# -------------------------------------------------
#' @name HELP.COORD
#' @docType data
#' @title Coordinate systems
#' @description
#' XLX tools use three coordinates system:\cr
#' \subsection{Relative string coordinates, called \code{sloc}}{
#'   a \code{sloc} is a string of the form :\cr
#'   \code{"chrname:from-to"} or \code{"chrname:from:to"}\cr
#'   where \code{chrname} is the sequence name (not the sequence index)
#'    \code{from} and \code{to} can be either 1-based (default)
#'   or 0-based (by specifiying the zero.based.loc=TRUE option)
#' }
#' \subsection{Relative coordinates, called \code{cloc}}{
#'    a \code{cloc} is a 1-based \code{slocation} of the form:\cr
#'    \code{c(chrindex, from, to)}
#'    where \code{chrindex} is the (1-based) index of sequence entry in basta file.\cr
#'    \code{chrindex}, \code{from} and \code{to} are (32 bits) integers.
#' }
#' \subsection{Absolute coordinates, called \code{coord}}{
#'  a \code{coord} represents two absolute positions in the catenated chromosomes,
#'  of the form \code{c(from, to)}.\cr
#'  absolute coordinates are always 1-based\cr
#'  \code{from} and \code{to} are (64 bits) doubles actually representing integers
#'  (there is no loss of precision until 53 bits i.e. 9,007,199,254,740,992)
#' }
#' It is not memory nor speed efficient to manipulate large amount
#' of clocations as lists (it uses about 64 bytes per clocation).\cr
#' Instead, XLX manipulate sets of clocations by matrices or list of matrices
#' (this uses 12 or 8 bytes per clocation and is much quicker to operate).
#' \subsection{Matrix of clocations, called \code{clocs}}{
#' a \code{clocs} is a nx3 matrix. each row is a clocation chrindex, from, to.
#' columns are named: "chr", "from", "to" respectively.
#' if necessary you may convert it to a dataframe by: \code{as.data.frame(clocs)}.
#' note that "chr" is (as in cloc) a chrindex \bold{not} a chr name.
#' use \code{basta.index2name} or \code{basta.name2index} to transform between
#' names and indexes}
#' \subsection{List of matrices of locations, called \code{llocs}}{
#' a \code{llocs} is a more memory efficient version of \code{clocs}.
#' this is a named list of matrices. each element of the list is named
#' by a chromosome index (as character) and contains an mx2 matrix
#' of from, to relative positions on this chromosome.
#' }
#' \subsection{Matrix of coordinates, called \code{coords}}{
#' a \code{coords} is a nx2 matrix. each row is a coord absfrom, absto.
#' columns are named: "from", "to" respectively.
#' if necessary you may convert it to a dataframe by: \code{as.data.frame(coords)}
#' }
#' \subsection{Summary}{
#'   \tabular{llll}{
#'   \bold{single location} \tab \tab \tab \cr
#'   \bold{shortname} \tab \bold{name} \tab \bold{definition} \tab \bold{base}\cr
#'   sloc \tab relative slocation \tab "chrname:from-to" \tab 0 or 1-based\cr
#'   cloc \tab relative clocation \tab c(chrindex, from, to) \tab 1-based\cr
#'   coord \tab absolute coordinates \tab c(absfrom, absto) \tab 1-based offset\cr\cr
#'   \bold{multiple locations} \tab \tab \tab \cr
#'   \bold{shortname} \tab \bold{name} \tab \bold{definition} \tab \bold{base}\cr
#'   clocs \tab matrix of clocations \tab nx3 matrix \tab 1-based\cr
#'   llocs \tab list of matrices of locations \tab n list of mx2 matrices \tab 1-based\cr
#'   coords \tab matrix of coordinates \tab nx2 matrix \tab 1-based\cr
#'   }
#' }
#' 
#' @note
#' in 1-based system endpoints are included : [from, to]\cr
#' in 0-based system the right endpoint is excluded : [from, to[\cr
#' the conversion between 0-based and 1-based is therefore\cr
#' from0 = from1 - 1\cr
#' to0   = to1\cr
#' @note
#' conversion between coordinate systems is performed by the
#' <xxx>2<yyy> functions (e.g. \link{coord2cloc})
#' @note
#' when extracting rows (or cols) from matrix, don't forget
#' to add the \code{drop=FALSE} last subscript, in order to avoid
#' spurious coercing to vector when selecting a single row (or col).
#' (eg: \code{clocs[1,,drop=FALSE]})
#'
NULL

# =================================================
# internals <no_export>
# =================================================

# -------------------------------------------------
# <internal> <no_export>
# emit warning
#
.xlx.warn <- function(ret=NULL, ..., up.frame=1) {
  lx.warn(..., up.frame=up.frame)
  ret
}

# -------------------------------------------------
# <internal> <no_export>
# default colnames for clocations matrix
#
.xlx.colnames <- function() {
  c("chr", "from", "to")
}

# -------------------------------------------------
# <internal> <no_export>
# clocations matrix to interval (chr is lost)
#
.clocs2inter <- function(clocs) {
  if (is.null(clocs)) clocs <- matrix(0, ncol=3, nrow=0)
  clocs <- clocs[clocs[,2]<=clocs[,3],,drop=FALSE]
  intervals::Intervals(clocs[,-1], type="Z")
}

# =================================================
# API
# =================================================

# -------------------------------------------------
#' reformat data to proper matrix of clocations
#' @description
#' this function is mostly used within other functions to properly
#' (re)format a clocs matrix. It is quite unusual to call it directly,
#' please consider \link{clocations} instead.
#' @param x data to reformat (see details)
#' @return nx3 matrix of clocations with proper
#' colnames and storage.
#' @details
#' data can be :\cr
#' \itemize{
#' \item NULL : return empty matrix
#' \item matrix : (should be nx3) then just setup colnames and storage mode
#' \item dataframe : (should be nx3) then convert to matrix 
#' \item  anything else: transform to nx3 matrix 
#' }
#' @seealso \link{clocations}
#' @examples
#' clocs.matrix() # empty clocs
#' clocs.matrix(list(c(1,1,10), c(2,1,10)))
#' clocs.matrix(c(1,1,10, 2,1,10))
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' clocs.matrix(lapply(c("seq1:1-10", "seq2:1-10"), sloc2cloc, handle=fh))
#' basta.close(fh)
#
clocs.matrix <- function(x=NULL) {
  if (length(x) == 0)        x <- matrix(0L, nrow=0, ncol=3)
  else if (is.matrix(x))     x <- x
  else if (is.data.frame(x)) x <- as.matrix(x)
  else                       x <- matrix(unlist(x, use.names=FALSE),
                                         ncol=3, byrow=TRUE)

  colnames(x) <- .xlx.colnames()
  rownames(x) <- NULL
  storage.mode(x) <- "integer"
  x
}

# -------------------------------------------------
#' create a matrix of clocations from data
#' @description
#' check if data is a proper matrix of clocations and
#' reformat it if necessary.
#' @param x data to reformat (see details)
#' @return nx3 matrix of clocations with proper
#' colnames and storage.
#' @details
#' data can be :\cr
#' \itemize{
#' \item NULL : return empty matrix
#' \item matrix : (should be nx3) then just setup colnames and storage mode
#' \item dataframe : (should be nx3) then convert to matrix 
#' \item  anything else: transform to nx3 matrix 
#' }
#' @seealso \link{clocs.matrix}
#' @examples
#' clocations() # empty clocs
#' clocations(list(c(1,1,10), c(2,1,10)))
#' clocations(c(1,1,10, 2,1,10))
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' clocations(lapply(c("seq1:1-10", "seq2:1-10"), sloc2cloc, handle=fh))
#' basta.close(fh)
#
clocations <- function(x=NULL) {

  if (   (length(dim(x)) != 2)
      || (storage.mode(x) != "integer")
      || (! identical(colnames(x), .xlx.colnames())))
    x <- clocs.matrix(x)
  
  x
}

# -------------------------------------------------
#' transform relative slocation to relative clocation
#' @description
#' see \link{HELP.COORD} for help on coordinates systems
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param slocation relative slocation ("chrname:from-to")
#' @param zero.based.loc given slocation is 0-based
#' @return relative clocation (1-based), NULL on error
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' sloc2cloc(fh, "seq1:1-10")
#' basta.close(fh)
#'
sloc2cloc <- function(handle, slocation, zero.based.loc=FALSE) {
  if (is.null(slocation))
    return(NULL)
  
  loc <- unlist(strsplit(slocation, ":|-"))
  if (length(loc) != 3)
    return(.xlx.warn(NULL, "malformed slocation : ", slocation))
  
  indx <- basta.name2index(handle, loc[1])
  if (is.null(indx))
    return(.xlx.warn(NULL, "unknown sequence : ", loc[1]))

  zero <- ifelse(zero.based.loc, 1L, 0L)
  from <- as.integer(loc[2]) + zero
  to   <- as.integer(loc[3])
  
  c(indx, from, to)
}

# -------------------------------------------------
#' transform relative clocation to relative slocation
#' @description
#' see \link{HELP.COORD} for help on coordinates systems
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param clocation relative clocation c(chrindex, from, to) (1-based)
#' @param zero.based.loc returned slocation should be 0-based
#' @return relative slocation "chrname:from-to" (0 or 1-based), NULL on error
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' cloc2sloc(fh, c(1, 1, 10))
#' basta.close(fh)
#'
cloc2sloc <- function(handle, clocation, zero.based.loc=FALSE) {
  if (is.null(clocation))
    return(NULL)
  
  name <- basta.index2name(handle, clocation[1])
  if (is.null(name))
    return(.xlx.warn(NULL, "unknown sequence index : ", clocation[1]))
    
  zero <- ifelse(zero.based.loc, 1, 0)
  paste(name, ":", clocation[2] - zero, "-", clocation[3], sep="")
}

# -------------------------------------------------
#' transform relative clocation to absolute coordinates
#' @description
#' see \link{HELP.COORD} for help on coordinates systems
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param clocation relative clocation c(chrindex, from, to) (1-based)
#' @param truncate truncate 3' to seq.size if needed
#' @return absolute coordinates c(absfrom, absto) (1-based), NULL on error
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' cloc2coord(fh, c(1, 1, 10))
#' cloc2coord(fh, c(2, 1, 10))
#' basta.close(fh)
#'
cloc2coord <- function(handle, clocation, truncate=TRUE) {
  if (is.null(clocation))
    return(NULL)
  
  seqindx <- clocation[1]
  if ((seqindx <= 0) || (seqindx > handle$header$nbseq))
    return(.xlx.warn(NULL, "unknown sequence index : ", seqindx))

  seq <- handle$header$seq[[seqindx]]
  if (truncate) {
    if (clocation[2] > seq$size) clocation[2] <- seq$size
    if (clocation[3] > seq$size) clocation[3] <- seq$size
  }
  c(clocation[2], clocation[3]) + seq$offset
}

# -------------------------------------------------
#' transform relative slocation to absolute coordinates
#' @description
#' see \link{HELP.COORD} for help on coordinates systems
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param slocation relative slocation ("chrname:from-to")
#' @param zero.based.loc given slocation is 0-based
#' @param truncate truncate 3' to seq.size if needed
#' @return absolute coordinates c(absfrom, absto) (1-based), NULL on error
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' sloc2coord(fh, "seq1:1-10")
#' sloc2coord(fh, "seq2:1-10")
#' basta.close(fh)
#'
sloc2coord <- function(handle, slocation, zero.based.loc=FALSE, truncate=TRUE) {
  cloc <- sloc2cloc(handle, slocation, zero.based.loc=zero.based.loc)
  if (is.null(cloc))
    return(NULL)
  cloc2coord(handle, cloc, truncate=truncate)
}

# -------------------------------------------------
#' transform absolute coordinates to relative clocation
#' @description
#' see \link{HELP.COORD} for help on coordinates systems
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param coord absolute coordinates c(absfrom, absto) (1-based) or
#'        single absolute position
#' @param truncate truncate 5' and 3' to seq.size if needed
#' @return relative clocation c(chrindex, from, to) (1-based), NULL on error
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' coord2cloc(fh, c(1, 10))
#' coord2cloc(fh, c(25, 34))
#' basta.close(fh)
#'
coord2cloc <- function(handle, coord, truncate=TRUE) {

  if (length(coord) == 1) coord <- c(coord, coord)
  
  off <- handle$header$offsets

  ifrom <- findInterval(coord[1]-1, off)
  ito   <- findInterval(coord[2]-1, off)
  
  if (ifrom != ito)
    return(.xlx.warn(NULL, "coordinates span several chromosomes"))
  
  if (ifrom <= 0)
    return(.xlx.warn(NULL, "invalid (<=0) coordinates"))
  
  seq <- handle$header$seq[[ifrom]]
  loc  <- coord - seq$offset
  
  from <- loc[1]
  to   <- loc[2]

  if ((from > seq$size) && truncate) from <- seq$size
  if ((to   > seq$size) && truncate) to   <- seq$size
  
  as.integer(c(ifrom, from, to))
}

# -------------------------------------------------
#' transform absolute coordinates to relative slocation
#' @description
#' see \link{HELP.COORD} for help on coordinates systems
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param coord absolute coordinates c(absfrom, absto) (1-based) or
#'        single absolute position
#' @param zero.based.loc given slocation is 0-based
#' @param truncate truncate 3' to seq.size if needed
#' @return relative slocation "chrname:from-to" (0 or 1-based), NULL on error
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' coord2sloc(fh, c(1, 10))
#' coord2sloc(fh, c(25, 34))
#' basta.close(fh)
#'
coord2sloc <- function(handle, coord, zero.based.loc=FALSE, truncate=TRUE) {
  cloc <- coord2cloc(handle, coord, truncate=truncate)
  if (is.null(cloc))
    cloc
  else
    cloc2sloc(handle, cloc, zero.based.loc=zero.based.loc)
}

# -------------------------------------------------
#' transform matrix of absolute coordinates to matrix of relative clocations
#' @description
#' transform a nx2 matrix of absolute coordinates (or a vector of point
#' coordinates) to nx3 matrix of relative clocations.\cr
#' see \link{HELP.COORD} for help on coordinates systems
#'
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param coords nx2 matrix of absolute coordinates  (1-based) or
#'        vector of n absolute point coordinates
#' @param truncate truncate 5' and 3' to seq.size if needed
#' @return nx3 matrix of relative clocation (1-based)
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#'
#' x <- sample(1:50, 10, replace=TRUE)
#' coo <- cbind(x, x)
#' clocs <- coords2clocs(fh, coo)
#' plocs <- coords2clocs(fh, x)
#' identical(clocs, plocs)
#'
#' x <- sample(25:30, 10, replace=TRUE)
#' coo <- cbind(from=x, to=x+10)
#' clocs <- coords2clocs(fh, coo)
#' rcoo <-clocs2coords(fh, clocs)
#' identical(coo, rcoo)
#'
#' clocs <- coords2clocs(fh, matrix(0, ncol=2, nrow=0))
#' clocs <- coords2clocs(fh, matrix(1, ncol=2, nrow=1))
#'
#' basta.close(fh)
#' @note
#' if some absolute coordinates span several chromosomes
#' then the corresponding rows are discarded.
#'
coords2clocs <- function(handle, coords, truncate=TRUE) {

  if (is.null(coords))
    return(clocs.matrix(NULL))
  
  header <- handle$header

  ncols <- ncol(coords)
  if ( is.null(ncols) || (ncols < 2))
    coords <- cbind(coords, coords)
    
  findx <- findInterval(coords[,1] - 1, header$offsets)
  tindx <- findInterval(coords[,2] - 1, header$offsets)

  ok <- (findx == tindx)
  if (! all(ok)) {
    .xlx.warn(NULL, "coordinates span several chromosomes")
    findx <- findx[ok]
    tindx <- tindx[ok]
    coords <- coords[ok,,drop=F]
  }
  
  ok <- (findx > 0)
  if (! all(ok)) {
    .xlx.warn(NULL, "invalid (<=0) coordinates")
    findx <- findx[ok]
    tindx <- tindx[ok]
    coords <- coords[ok,,drop=F]
  }

  if (truncate) {
    #
    # for compatibilty with version < 1.6
    # where header$sizes was absent
    #
    if (is.null(header$sizes))
      header$sizes <- sapply(header$seq, function(x) x$size)
    off  <- header$offsets[findx]
    siz  <- header$sizes[findx]
    clocs <- cbind(findx, 
                   pmin(siz, coords[,1] - off),
                   pmin(siz, coords[,2] - off))
  } else {
    clocs <- cbind(findx, coords - header$offsets[findx])
  }
  
  
  clocs.matrix(clocs)
}

# -------------------------------------------------
#' transform matrix of relative clocations to matrix of absolute coordinates 
#' @description
#' transform a nx3 matrix of relative clocations to
#' a nx2 matrix of absolute coordinates\cr
#' see \link{HELP.COORD} for help on coordinates systems
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param clocations nx3 matrix of relative clocation (1-based)
#' @return nx2 matrix of absolute coordinates  (1-based)
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#'
#' x <- sample(1:50, 10, replace=TRUE)
#' coo <- cbind(x, x)
#' clocs <- coords2clocs(fh, coo)
#' plocs <- coords2clocs(fh, x)
#' identical(clocs, plocs)
#'
#' x <- sample(25:30, 10, replace=TRUE)
#' coo <- cbind(from=x, to=x+10)
#' clocs <- coords2clocs(fh, coo)
#' rcoo <-clocs2coords(fh, clocs)
#' identical(coo, rcoo)
#'
#' clocs <- coords2clocs(fh, matrix(0, ncol=2, nrow=0))
#' clocs2coords(fh, clocs)
#' clocs <- coords2clocs(fh, matrix(1, ncol=2, nrow=1))
#' clocs2coords(fh, clocs)
#'
#' basta.close(fh)
#'
clocs2coords <- function(handle, clocations) {
  if (is.null(clocations)) clocations <- clocs.matrix(NULL)
  offs <- sapply(handle$header$seq, function(x) x$offset, USE.NAMES=FALSE)
  clocations[,-1, drop=FALSE] + offs[clocations[,1, drop=FALSE]]
}

# -------------------------------------------------
#' split matrix of clocations into submatrices
#' @description
#' split matrix of clocations (by rows) into submatrices according
#' to \code{by} \cr
#' @param clocations matrix of clocations
#' @param by group factors (should be of length nrow(clocations))
#' @param keep.split logical keep split indexes as attribute (see details)
#' @details 
#' if \code{keep.split} is TRUE then the original indexes of clocations
#' are kept (as attribute \code{split}) for further reordering (see \link{clocs.rbind}).
#' @return list of submatrices (named by factors)
#' @seealso \link{clocs.rbind} for the reverse operation
#' @note
#' levels in \code{by} that do not occur are dropped.
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' clocs <- coords2clocs(fh, 1:25)
#' x <- clocs.rsplit(clocs)
#' y <- clocs.rbind(x)
#' z <- clocs2coords(fh, y)
#' identical(as.integer(z[,1]), 1:25)
#' basta.close(fh)
#'
clocs.rsplit <- function(clocations, by=clocations[,1], keep.split=FALSE) {
  if (is.null(clocations)) clocations <- clocs.matrix(NULL)
  spl <- split(seq_len(nrow(clocations)), by)
  res <- lapply(spl, function(x) clocs.matrix(clocations[x,,drop=FALSE]))
  if (keep.split) attr(res, "split") <- unlist(spl, use.names=F)
  res
}

# -------------------------------------------------
#' catenate a list of matrices of clocations into single matrix
#' @description
#' catenate (by rows) a list of matrices of clocations into a
#' single matrix of clocations and optionaly reorder rows\cr
#' @details 
#' this is equivalent to \code{do.call(rbind, submatrices)}
#' followed by an optional reordering of rows when the \code{submatrices}
#' argument has a \code{split} attribute (see \link{clocs.rsplit}).
#' @param submatrices list of submatrices of clocations
#' @return matrix of clocations
#' @seealso \link{clocs.rsplit} for the reverse operation
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' clocs <- coords2clocs(fh, 1:25)
#' x <- clocs.rsplit(clocs)
#' y <- clocs.rbind(x)
#' z <- clocs2coords(fh, y)
#' identical(as.integer(z[,1]), 1:25)
#' clocs[1,1] <- 2L
#' clocs[25,1] <- 1L
#' x <- clocs.rsplit(clocs, keep=T)
#' y <- clocs.rbind(x)
#' identical(y, clocs)
#' basta.close(fh)
#'
clocs.rbind <- function(submatrices) {
  res <- clocations(do.call(rbind, submatrices))
  spl <- attr(submatrices, "split", exact=TRUE)
  if (! is.null(spl)) res <- res[order(spl),]
  res
}

# -------------------------------------------------
#' convert clocations to llocations
#' @description
#' convert a nx3 matrix of clocations to a list of
#' mx2 locations.
#' (see \link{HELP.COORD} for help on coordinates systems)
#' @param clocations nx3 matrix of relative clocations (1-based)
#' @return named list of mx2 relative locations per chromosome
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' clocs <- coords2clocs(fh, 1:25)
#' llocs <- clocs2llocs(clocs)
#' rclocs <- llocs2clocs(llocs)
#' identical(clocs, rclocs)
#' basta.close(fh)
#'
clocs2llocs <- function(clocations) {
  lx.lapply(clocs.rsplit(clocations), function(clocs) clocs[,-1, drop=FALSE],
                use.threads=FALSE)
}

# -------------------------------------------------
#' convert llocations to clocations
#' @description
#' convert a named list of mx2 locations to a nx3 matrix of clocations.
#' (see \link{HELP.COORD} for help on coordinates systems)
#' @param llocations named list of mx2 relative locations per chromosome (see note)
#' @return nx3 matrix of relative clocations (1-based)
#' @note
#' llocations must be named by chromosome indexes (as character)
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' clocs <- coords2clocs(fh, 1:25)
#' llocs <- clocs2llocs(clocs)
#' rclocs <- llocs2clocs(llocs)
#' identical(clocs, rclocs)
#' basta.close(fh)
#'
llocs2clocs <- function(llocations) {
  clocs.rbind(lx.napply(llocations, function(name, lloc) {
    cbind(as.integer(name), lloc)
  }, use.threads=FALSE))
}

# -------------------------------------------------
#' test if clocations is empty
#' @description 
#' Test if clocations is empty (either null or no rows)
#' @param clocations nx3 matrix of relative clocations (1-based)
#' @return boolean TRUE if empty.
#' @examples
#' clocs.is.empty(NULL)
#' clocs.is.empty(clocations())
#' clocs.is.empty(clocations(c(1,1,10)))
#' 
clocs.is.empty <- function(clocations) {
  nr <- nrow(clocations)
  is.null(nr) || (nr == 0)
}

# -------------------------------------------------
#' clocations sanity check
#' @description 
#' Test if clocations matrix is valid i.e. that\cr
#' \code{1 <= from <= to <= seq.len} and \code{1 <= chr <= nchr}.
#' @param clocations nx3 matrix of relative clocations (1-based)
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open}). this parameter is optional (see details).
#' @details if \code{handle} is provided then the function checks
#' that \code{to <= seq.len} and \code{chr <= nchr} else these
#' conditions are ignored.
#' @note an empty clocations is valid.
#' @return boolean TRUE if valid.
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' clocs.is.valid(NULL)
#' clocs <- clocations(c(1,10,25, 2,30,10))
#' clocs.is.valid(clocs)
#' clocs <- clocations(c(1,10,25, 2,10,30))
#' clocs.is.valid(clocs)
#' clocs.is.valid(clocs, fh)
#' clocs <- clocations(c(1,10,24, 2,10,30))
#' clocs.is.valid(clocs, fh)
#' basta.close(fh)
#' 
clocs.is.valid <- function(clocations, handle=NULL) {
  if (clocs.is.empty(clocations)) return(TRUE)
  ok <- all(  (clocations[,1] >= 1)
            & (clocations[,2] >= 1)
            & (clocations[,3] >= clocations[,2]))
  if (! ok) return(FALSE)
  if (! is.null(handle)) {
    ok <- all(clocations[,1] <= handle$header$nbseq)
    if (! ok) return(FALSE)
    #
    # for compatibilty with version < 1.6
    # where header$sizes was absent
    #
    if (is.null(handle$header$sizes))
      handle$header$sizes <- sapply(handle$header$seq, function(x) x$size)
    ok <- all(clocations[,3] <= handle$header$sizes[clocations[,1]])
  }
  ok
}

# -------------------------------------------------
#' test if clocations are \bold{not} sorted
#' @description 
#' Test if clocations are not sorted without the cost of sorting it.
#' @details the sort order is: first by increasing chromosome index
#' (clocations[,1]) then, for equal chromosome index, by increasing from
#' (clocations[,2]) then, for equal from, by increasing to if 
#' \code{decreasing.to==FALSE} else by decreasing to.
#' @param clocations nx3 matrix of relative clocations (1-based)
#' @param decreasing.to boolean or NA.
#' should the sort order of \code{to} be increasing or decreasing?
#' may be set to NA if you don't care about the order of to.
#' @return boolean TRUE if unsorted, FALSE if sorted
#' @note for coords you may use the R base function \link{is.unsorted}
#' @seealso \link{clocs.sort}
#' @examples
#' clocs <- clocations(c(1,1,5, 1,10,10, 1,10,20, 2,5,10))
#' clocs.is.unsorted(clocs)
#' clocs.is.unsorted(clocs, decreasing.to=TRUE)
#' clocs.is.unsorted(clocs, decreasing.to=NA)
#' clocs <- clocs.sort(clocs, decreasing.to=TRUE)
#' 
clocs.is.unsorted <- function(clocations, decreasing.to=FALSE) {
  dif.chr  <- diff(clocations[,1])
  if (any(dif.chr < 0)) return(TRUE)
  dif.from <- diff(clocations[,2])
  if (any((dif.from < 0) & (dif.chr == 0))) return(TRUE)
  if (is.na(decreasing.to)) return(FALSE)
  dif.to <- diff(clocations[,3])
  dif.to <- if (decreasing.to) (dif.to > 0) else (dif.to < 0)
  any(dif.to & (dif.from == 0) & (dif.chr == 0))
}

# -------------------------------------------------
#' sort clocations
#' @description 
#' sort clocations in increasing order of chr, from and increasing or
#' decreasing order of to.
#' @details the sort order is: first by increasing chromosome index
#' (clocations[,1]) then, for equal chromosome index, by increasing from
#' (clocations[,2]) then, for equal from, by increasing to
#' if \code{decreasing.to=FALSE} else by decreasing to.
#' @param clocations nx3 matrix of relative clocations (1-based)
#' @param decreasing.to boolean.
#' should the sort order of \code{to} be decreasing or increasing?
#' @param order return order index instead of clocations
#' @return sorted nx3 matrix of relative clocations (if order=FALSE)
#' or an integer vector of row indexes (if order=TRUE)
#' @note for coords you may simply use the R base function \link{sort}
#' @seealso \link{clocs.is.unsorted}
#' @examples
#' clocs <- clocations(c(1,10,20, 1,10,30, 2,5,10, 1,1,100))
#' clocs.sort(clocs)
#' clocs.sort(clocs, decreasing.to=TRUE)
#'
clocs.sort <- function(clocations, decreasing.to=FALSE, order=FALSE) {
  if (clocs.is.empty(clocations)) return(clocations(NULL))
  to <- if (decreasing.to) -clocations[,3] else clocations[,3]
  ord <- order(clocations[,1], clocations[,2], to)
  if (order) ord else clocations[ord,,drop=F]
}

# -------------------------------------------------
#' test if clocations are disjoint
#' @description 
#' Test if clocations are (weakly or strongly) disjoint.\cr
#' weakly disjoint <=> no interval is completely
#' included in another one (but intervals may overlap)\cr
#' strongly disjoint <=> no two intervals overlap.
#' @param clocations nx3 matrix of relative clocations (1-based)
#' @param strong if strongly disjoint
#' @return boolean TRUE if (weakly/strongly) disjoint
#' @note this function runs slightly quicker if clocations has already
#' been sorted by \link{clocs.sort} with decreasing.to=TRUE.
#' @examples
#' clocs <- clocations(c(1,1,10, 1,11,20, 1,21,30, 2,5,10))
#' clocs.is.disjoint(clocs)
#' clocs <- clocations(c(1,1,10, 1,5,20, 1,10,30, 2,5,10))
#' clocs.is.disjoint(clocs)
#' clocs.is.disjoint(clocs, strong=FALSE)
#' clocs <- clocations(c(1,1,10, 1,5,30, 1,10,30, 2,5,10))
#' clocs.is.disjoint(clocs, strong=FALSE)
#' 
clocs.is.disjoint <- function(clocations, strong=TRUE) {
  if (clocs.is.empty(clocations)) return(TRUE)
  if (clocs.is.unsorted(clocations, decreasing.to=TRUE))
    clocations <- clocs.sort(clocations, decreasing.to=TRUE)
  if (strong) {
    dif.chr <- diff(c(rbind(clocations[,1], clocations[,1])))
    dif.pos <- diff(c(t(clocations[,2:3])))
  } else {
    dif.chr <- diff(clocations[,1])
    dif.pos <- diff(clocations[,3])
  }
  ! any((dif.pos <= 0) & (dif.chr == 0))
}

# -------------------------------------------------
# <internal> <no_export>
# clocs.join algo a (shift) - for (weakly) disjoint intervals
# assume clocations have been sorted and are (weakly) disjoint
#
.clocs.join.shift <- function(clocations, delta=0L) {
  
  to   <- lx.head(clocations[,3], -1)
  chrt <- lx.head(clocations[,1], -1)
  from <- lx.tail(clocations[,2], -1)
  chrf <- lx.tail(clocations[,1], -1)
  
  # joined : boolean vector TRUE on junctions
  joined  <- c((chrf == chrt) & ((from - to - 1) <= delta), FALSE)

  # nothing to do  
  if (! any(joined)) return(clocations)
  
  # shifted : joined right shifted
  shifted <- lx.shift(joined, -1, F)
  
  # begs -> (F)T transitions
  begs <- (! shifted) & joined
  # ends -> (T)F transitions
  ends <- shifted & (! joined)
  
  clocations[begs,3] <- pmax(clocations[begs,3], clocations[ends,3])
  
  # keep boundaries
  clocations[begs | !(joined | ends),,drop=F]
}

# -------------------------------------------------
# <internal> <no_export>
# clocs.join algo b (reduce) - for all other cases
#
.clocs.join.reduce <- function(clocations, use.threads=lx.use.threads()) {
  
  chrs <- unique(clocations[,1])
  
  clocations <- clocs.rsplit(clocations)
  
  res <- lx.lapply(as.character(chrs), function(chr) {
    int <- .clocs2inter(clocations[[chr]])
    mat <- as.matrix(intervals::close_intervals(intervals::reduce(int)))
    mat <- if (nrow(mat) != 0) cbind(as.integer(chr), mat) else clocs.matrix(NULL)
    mat
  }, use.threads=use.threads, mc.preschedule=FALSE)
  
  clocs.rbind(res)
}

# -------------------------------------------------
#' join clocations
#' @description
#' join consecutive clocations that are separated by at most \code{delta} bp,
#' compact results and retains intervals above a specified width.
#' @details
#' This function joins together consecutive intervals (separated by at most
#' \code{delta >= 0} bp). It then compacts results to disjoint intervals
#' and finally retains intervals above a given width.\cr
#' It is legal to call it with  \code{delta = 0} in order to fusion
#' overlapping or stricly adjacent intervals. This will produce the most
#' compact representation of the input set of intervals and is
#' equivalent to \link{clocs.reduce}.\cr\cr
#' The function implements two different algorithms.\cr
#' The first one (shift)
#' is very efficient if the clocations are (weakly) disjoints
#' (see \link{clocs.is.disjoint}).\cr
#' The second algorithm (reduce) does not have this requirement but works more
#' slowly on the average.\cr
#' The function will switch to the most appropriate
#' algorithm by using \link{clocs.is.disjoint}. If you know that the disjoint
#' condition will \bold{not} be satisfied, you may force the use of the (reduce)
#' algorithm by using the \code{.force.reduce} parameter.\cr
#' note that in all cases the result is the same, just the execution time
#' may vary.\cr
#' the \code{use.threads} parameter is only active with algorithm (reduce).\cr
#' Results are always sorted by increasing chromosome index, from and to 
#' positions (see \link{clocs.sort}).
#' @param clocations nx3 matrix of relative clocations (1-based)
#' @param delta positive or zero integer. the maximal spacing between
#' two consecutive intervals to be joined.
#' @param minsize minimum interval width. all resulting intervals
#' strictly smaller than \code{minsize} are discarded.
#' @param .force.reduce (see details).
#' @param use.threads (see \link{lx.use.threads}).
#' @seealso \link{clocs.reduce}
#' @examples
#' clocs <- clocations(c(1,1,10, 1,11,20, 1,20,30, 1,40,50, 1,60,70, 1,70,80, 1,90,100, 2,1,10))
#' clocs.join(clocs)
#' clocs.join(clocs, delta=9)
#' clocs <- clocs.rbind(list(clocs, c(1,1,100)))
#' clocs.join(clocs)
#'
clocs.join <- function(clocations, delta=0L, minsize=1L,
                       .force.reduce=FALSE,
                       use.threads=lx.use.threads()) {
  
  if (clocs.is.empty(clocations))
    return(clocs.matrix(NULL))
  
  lx.stopif(delta < 0, "delta should be >= 0")
  
  if (! .force.reduce) {
    clocations <- clocs.sort(clocations, decreasing.to=TRUE)
    .force.reduce <- ! clocs.is.disjoint(clocations, strong=FALSE)
  }
  

                        # ------------------
  if (.force.reduce) {  # algo (b) : reduce
                        # ------------------
    clocations[,3] <- clocations[,3] + delta
    clocations <- .clocs.join.reduce(clocations, use.threads=use.threads)
    clocations[,3] <- clocations[,3] - delta
                        # ------------------
  } else {              # algo (a) : shift
                        # ------------------
    clocations <- .clocs.join.shift(clocations, delta=delta)
  }
  
  # threshold result
  valid <- (clocations[,3] - clocations[,2] + 1L) >= minsize
  
  clocs.sort(clocs.matrix(clocations[valid,,drop=FALSE]))
}

# -------------------------------------------------
#' compactly re-represent clocations
#' @description
#' a set of clocations represents intervals on chromosomes.
#' in general these intervals may overlap (partially or completely)
#' or may be strictly adjacent.
#' this function computes the union of all intervals on each
#' chromosome. This will produce (strongly) disjoint intervals
#' (see \link{clocs.is.disjoint}) corresponding to the most compact
#' representation of the input set of intervals. It also sorts the resulting
#' clocations by increasing chromosome index, from and to positions.
#' Finally only intervals above a specified width are retained.
#' @param clocations nx3 matrix of relative clocations (1-based)
#' @param minsize minimum interval width. all resulting intervals
#' strictly smaller than \code{minsize} are discarded.
#' @param use.threads (see \link{lx.use.threads})
#' @note
#' this is formally equivalent (and actually implemented as):\cr
#' \code{clocs.join(clocations, delta=0L, minsize=minsize, use.thread=use.thread)}\cr
#' this function has been kept for historical reasons
#' (and to keep open the possibility
#' of a more efficient version in the future).
#' @note
#' \code{clocs.reduce(x)} is formally equivalent to \code{clocs.inter(x,x)}
#' but much more efficient.
#' @seealso \link{clocs.join}, \link{clocs.inter}
#' @examples
#' clocs <- clocations(c(1,1,10, 1,11,20, 1,20,30, 1,40,50, 1,60,70, 1,70,80, 1,90,100, 2,1,10))
#' clocs.reduce(clocs)
#' clocs <- clocs.rbind(list(clocs, c(1,1,100)))
#' clocs.reduce(clocs)
#' clocs.reduce(NULL)
#'
clocs.reduce <- function(clocations,
                         minsize=1L,
                         use.threads=lx.use.threads()) {
  
  clocs.join(clocations, delta=0L, minsize=minsize, use.threads=use.threads)
}

# -------------------------------------------------
#' get overlaps from one set of clocations to another
#' @description
#' given two sets of clocations, indicates for each clocation of
#' the first set which clocations from the second set it overlaps
#' @param clocations nx3 matrix of relative clocations (1-based)
#' @param over mx3 matrix of relative clocations (1-based)
#' @param use.threads (see \link{lx.use.threads})
#' @return a list of length nrow(clocations). the elements are vector
#' of indices indicating which elements of the second set are overlapped.
#' A list element of length 0 indicates no overlapping elements.
#' @note
#' multithreading is performed by chromosomes
#' @seealso \link{clocs.nearest}, \link{clocs.included}, \link{clocs.covered}
#' @examples
#' c1 <- clocations(c(1,1,10, 1,20,30, 2,1,10))
#' clocs.overlap(c1, c1)
#' c2 <- clocations(c(1,1,20))
#' clocs.overlap(c1, c2)
#' clocs.overlap(c2, c1)
#' c2 <- clocations(c(1,1,15, 1,5,25))
#' clocs.overlap(c1, c2)
#' clocs.overlap(c2, c1)
#' c2 <- clocations(c(2,1,10, 1,1,15, 1,5,25))
#' clocs.overlap(c1, c2)
#' clocs.overlap(c2, c1)
#' clocs.overlap(c1, NULL)
#' clocs.overlap(NULL, NULL)
#'
clocs.overlap <- function(clocations, over,
                          use.threads=lx.use.threads()) {
  
  # over should be sorted by chr
  ord.over <- NULL
  if (is.unsorted(over[,1])) {
    ord.over <- order(over[,1])
    over <- over[ord.over,]
  }

  clocations <- clocs.rsplit(clocations, keep.split=T)
  over <- clocs.rsplit(over)
  
  # offsets
  off  <- head(cumsum(c(0, sapply(over, nrow))), -1)
  names(off) <- names(over)
  off[setdiff(names(clocations), names(over))] <- 0
  
  res <- lx.lapply(names(clocations), function(chr) {
    in1 <- .clocs2inter(clocations[[chr]])
    in2 <- .clocs2inter(over[[chr]])
    lapply(intervals::interval_overlap(in1, in2), function(x) x + off[[chr]])
  }, use.threads=use.threads, mc.preschedule=FALSE)
  
  res <- unlist(res, recursive=F, use.names=F)

  spl <- attr(clocations, "split")
  res <- if (is.null(spl)) res else res[order(spl)]
  
  if (! is.null(ord.over))
    res <- lapply(res, function(x) ord.over[x])
  
  res
}

# -------------------------------------------------
#' Identify nearest member(s) in a set of clocations
#' @description
#' For each clocations in the \code{from} argument,
#' identify the nearest clocation or clocations (in case of ties)
#' in the \code{to} argument.
#' @details
#' the distance between two intervals is their spacing. So if they overlap
#' the distance is 0.\cr
#' @param from nx3 matrix of relative clocations (1-based)
#' @param to mx3 matrix of relative clocations (1-based)
#' @param use.threads (see \link{lx.use.threads})
#' @return A data frame with three columns: 
#' distance_to_nearest, which_nearest, and which_overlap.
#' the first one is NA if there are no interval at all in
#' the \code{to} set.\cr
#' The last two are lists, since there may be zero, one, 
#' or more nearest intervals in the \code{to} set.
#' @note
#' multithreading is performed by chromosomes
#' @seealso \link{clocs.overlap}, \link{clocs.included}, \link{clocs.covered}
#' @examples
#' c1 <- clocations(c(1,5,15, 1,15,40, 1,20,30, 1,25,35, 1,60,70, 2,1,10))
#' c2 <- clocations(c(1,1,20, 1,40,50))
#' clocs.nearest(c1,c2)
#' c2 <- clocations(c(2,1,10, 1,1,20, 1,40,50))
#' clocs.nearest(c1,c2)
#' clocs.nearest(c1,NULL)
#' clocs.nearest(NULL, NULL)
#'
clocs.nearest <- function(from, to,
                          use.threads=lx.use.threads()) {

  # to should be sorted by chr
  ord.to <- NULL
  if (is.unsorted(to[,1])) {
    ord.to <- order(to[,1])
    to <- to[ord.to,]
  }
  
  from <- clocs.rsplit(from, keep.split=T)
  to   <- clocs.rsplit(to)

  # offsets
  off  <- head(cumsum(c(0, sapply(to, nrow))), -1)
  names(off) <- names(to)
  off[setdiff(names(from), names(to))] <- 0
  
  res <- lx.lapply(names(from), function(chr) {
    in1 <- .clocs2inter(from[[chr]])
    in2 <- .clocs2inter(to[[chr]])
    df <- intervals::which_nearest(in1, in2)
    df$which_nearest <- lapply(df$which_nearest, function(x) x + off[[chr]])
    df$which_overlap <- lapply(df$which_overlap, function(x) x + off[[chr]])
    df
  }, use.threads=use.threads, mc.preschedule=FALSE)
  
  res <- do.call(rbind, res)
  
  spl <- attr(from, "split")
  res <- if (is.null(spl)) res else res[order(spl),]

  if (! is.null(ord.to)) {
    res$which_nearest <- lapply(res$which_nearest, function(x) ord.to[x])
    res$which_overlap <- lapply(res$which_overlap, function(x) ord.to[x])
  }
  
  rownames(res) <- NULL
  res
}

# -------------------------------------------------
#' assess inclusion from one set of clocations to another
#' @description
#' given two sets of clocations, indicates for each clocation of
#' the first set if it is included in at least one clocation of the
#' second set.
#' @param clocations nx3 matrix of clocations to be tested.
#' @param include nx3 matrix of clocations to search in
#' @param use.threads (see \link{lx.use.threads})
#' @return logical vector of length nrow(clocations)
#' @seealso \link{clocs.covered}, \link{clocs.overlap}
#' @examples
#' c1 <- clocations(c(1,1,10))
#' c2 <- clocations(c(1,1,5, 1,6,10))
#' clocs.included(c1, c2)
#' clocs.included(c2, c1)
#' c1 <- clocations(c(1,1,10, 1,20,30, 2,10,20))
#' c2  <- clocations(c(1,1,20, 1,25,30))
#' clocs.included(c1, c2)
#' clocs.included(c2, c1)
#' clocs.included(c1, NULL)
#' clocs.included(NULL, NULL)
#' # the difference with clocs.covered is in the reduction of the second argument
#' c1 <- clocations(c(1,20,30))
#' c2 <- clocations(c(1,1,25, 1,25,40))
#' clocs.included(c1, c2)
#' clocs.covered(c1, c2)
#' clocs.included(c1, clocs.reduce(c2))
#'
clocs.included <- function(clocations, include,
                           use.threads=lx.use.threads()) {
  
  .included <- function(c1, c2) (c1[[2]] >= c2[[2]]) && (c1[[3]] <= c2[[3]])
  
  over <- clocs.overlap(clocations, include)
  
  res <- lx.lapply(seq_along(over), function(i) {
    any(sapply(over[[i]], function(j) .included(clocations[i,], include[j,])))
  }, use.threads=use.threads, mc.preschedule=TRUE)

  unlist(res, recursive=F, use.names=F)
}

# -------------------------------------------------
#' assess coverage from one set of clocations by another
#' @description
#' this is a variant of \link{clocs.included} which test if clocations
#' from the first set are fully covered by one or more intervals in the second
#' set.
#' @details this is almost functionaly equivalent to
#' \code{clocs.included(clocations, clocs.reduce(cover))}.
#' i.e. the second set is first reduced (see \link{clocs.reduce}) to
#' disjoint intervals then the inclusion test (see \link{clocs.included})
#' is performed. But it is implemented in a more efficient way, especially
#' when \code{cover} is already disjoint.\cr
#' the main difference is that \link{clocs.included} keeps row order from
#' \code{clocations} whereas this implementation reorder rows in the result
#' for efficiency.
#' @param clocations nx3 matrix of clocations to be tested.
#' @param cover nx3 matrix of clocations to search in
#' @param use.threads (see \link{lx.use.threads})
#' @return logical vector of length nrow(clocations)
#' @note if \code{cover} is already disjoint, this is equivalent to
#' \link{clocs.included} but much more efficient.
#' @seealso \link{clocs.included}, \link{clocs.reduce}, \link{clocs.overlap}
#' @examples
#' c1 <- clocations(c(1,1,10))
#' c2 <- clocations(c(1,1,5, 1,6,10))
#' clocs.covered(c1, c2)
#' clocs.covered(c2, c1)
#' c1 <- clocations(c(1,1,10, 1,20,30, 2,10,20))
#' c2  <- clocations(c(1,1,20, 1,25,30))
#' clocs.covered(c1, c2)
#' clocs.covered(c2, c1)
#' clocs.covered(c1, NULL)
#' clocs.covered(NULL, NULL)
#' # the difference with clocs.covered is in the reduction of the second argument
#' c1 <- clocations(c(1,20,30))
#' c2 <- clocations(c(1,1,25, 1,25,40))
#' clocs.covered(c1, c2)
#' clocs.included(c1, c2)
#' clocs.included(c1, clocs.reduce(c2))
#'
clocs.covered <- function(clocations, cover,
                         use.threads=lx.use.threads()) {
  
  if (clocs.is.empty(cover))
    return(rep(FALSE, nrow(clocations(clocations))))
                                    
  if (clocs.is.unsorted(cover, decreasing.to=TRUE))
    cover <- clocs.sort(cover, decreasing.to=TRUE)

  if (! clocs.is.disjoint(cover))
    cover <- clocs.reduce(cover, use.threads=use.threads)
  
  cover[,3] <- cover[,3] + 1
  
  cover <- split(as.vector(t(cover[,2:3])), rep(cover[,1], each=2))
  
  is.point <- all(clocations[,2] == clocations[,3])

  apply.clocs(clocations, function(clocs, handle) {
    chr <- as.character(clocs[1,1])
    from.ok <- findInterval(clocs[,2], cover[[chr]])
    ok <- (from.ok %% 2 == 1)
    if (! is.point) {
      to.ok   <- findInterval(clocs[,3], cover[[chr]])
      ok <- ok & (from.ok == to.ok)
    }
    ok
  }, handle=NULL, flatten=T, use.threads=use.threads)
}

# -------------------------------------------------
# <internal not exported>
# compute intervals operation on two sets of clocations
# oper = intervals::interval_intersection or intervals::interval_union
# see clocs.inter and clocs.union below
#
.clocs.oper <- function(clocations1, clocations2, chrs, oper,
                        minsize=1L, use.threads=lx.use.threads()) {

  opname <- as.character(lx.args("oper")[1])

  clocations1 <- clocs.rsplit(clocations1)
  clocations2 <- clocs.rsplit(clocations2)

  res <- lx.lapply(as.character(chrs), function(chr) {
    lx.out("running ", opname, " on regions of chr: ", chr, up.frame=1, level="debug")
    in1 <- .clocs2inter(clocations1[[chr]])
    in2 <- .clocs2inter(clocations2[[chr]])
    in1 <- oper(in1, in2) # keep oper out of next call
    mat <- as.matrix(intervals::close_intervals(in1))
    mat <- mat[(mat[,2] - mat[,1] + 1L) >= minsize,, drop=FALSE]
    mat <- if (nrow(mat) != 0) cbind(as.integer(chr), mat) else clocs.matrix(NULL)
    mat
  }, use.threads=use.threads, mc.preschedule=FALSE)

  clocs.rbind(res)
}

# -------------------------------------------------
#' compute the intersection of two sets of clocations 
#' @description
#' each set of clocations represents intervals on chromosomes.
#' this function intersects (by chromosome) the union of all intervals from the first set
#' with the union of all intervals from the second set and retains intervals
#' above a specified width.\cr
#' @param clocations1 nx3 matrix of relative clocations (1-based)
#' @param clocations2 mx3 matrix of relative clocations (1-based)
#' @param minsize minimum interval width (see details)
#' @param use.threads (see \link{lx.use.threads})
#' @return kx3 matrix of relative clocations (1-based)
#' @details
#' minsize parameter: all resulting intervals strictly smaller than \code{minsize} are discarded
#' @note
#' the result is always compact and strongly disjoint
#' @note
#' intersecting a set with itself is formally equivalent to calling \link{clocs.reduce}
#' @note
#' multithreading is performed by chromosomes
#' @seealso \link{clocs.included}, \link{clocs.covered}, \link{clocs.overlap}.
#' @examples
#' c1 <- clocations(c(1,1,10, 1,20,30, 2,1,10))
#' c2 <- clocations(c(1,5,15, 1,10,25))
#' clocs.inter(c1, c2)
#' clocs.inter(c2, c1)
#' identical(clocs.inter(c2,c2), clocs.reduce(c2))
#' clocs.inter(c1, NULL)
#' clocs.inter(NULL, NULL)
#'
clocs.inter <- function(clocations1, clocations2,
                        minsize=1L, use.threads=lx.use.threads()) {

  if (clocs.is.empty(clocations1) || clocs.is.empty(clocations2))
      return(clocs.matrix(NULL))

  chrs <- intersect(unique(clocations1[,1]), unique(clocations2[,1]))

  .clocs.oper(clocations1, clocations2, chrs,
              intervals::interval_intersection,
              minsize=minsize, use.threads=use.threads)
}

# -------------------------------------------------
#' compute the union of two sets of clocations 
#' @description
#' each set of clocations represents intervals on chromosomes.
#' this function makes union (by chromosome) of all intervals
#' from the first set and all intervals from the second set and retains intervals
#' above a specified width.\cr
#' @param clocations1 nx3 matrix of relative clocations (1-based)
#' @param clocations2 mx3 matrix of relative clocations (1-based)
#' @param minsize minimum interval width (see details)
#' @param use.threads (see \link{lx.use.threads})
#' @return kx3 matrix of relative clocations (1-based)
#' @details
#' minsize parameter: all resulting intervals strictly smaller than \code{minsize} are discarded
#' @note
#' this is formally equivalent to concatenating (by \link{clocs.rbind}) the two sets 
#' and calling \link{clocs.reduce} but much more efficient.
#' @note
#' multithreading is performed by chromosomes
#' @examples
#' c1 <- clocations(c(1,1,10, 1,20,30, 2,1,10))
#' c2 <- clocations(c(1,5,15, 1,10,25))
#' clocs.union(c1, c2)
#' identical(clocs.union(c1, c2), clocs.reduce(clocs.rbind(list(c1, c2))))
#' identical(clocs.union(c1, NULL), c1)
#' clocs.union(NULL, NULL)
#'
clocs.union <- function(clocations1, clocations2,
                        minsize=1L, use.threads=lx.use.threads()) {

  if (clocs.is.empty(clocations1)) clocations1 <- clocs.matrix(NULL)
  if (clocs.is.empty(clocations2)) clocations2 <- clocs.matrix(NULL)
  
  if (clocs.is.empty(clocations1) && clocs.is.empty(clocations2))
    return(clocs.matrix(NULL))

  chrs <- union(unique(clocations1[,1]), unique(clocations2[,1]))

  .clocs.oper(clocations1, clocations2, chrs,
              intervals::interval_union,
              minsize=minsize, use.threads=use.threads)
}

# -------------------------------------------------
#' filter clocations by size
#' @description
#' keep only clocations which size \code{(w=to-from+1)} is greater
#' or equal to \code{minsize}.
#' @param clocations nx3 matrix of relative clocations (1-based)
#' @param reduce perform a \link{clocs.reduce} first
#' @param minsize minimum width
#' @param use.threads (see \link{lx.use.threads}) (only used if reduce==TRUE)
#' @examples
#' c1 <- clocations(c(1,1,5, 1,20,30, 2,1,10))
#' identical(clocs.threshold(c1, minsize=0), c1)
#' clocs.threshold(c1, minsize=10)
#' clocs.threshold(c1, minsize=30)
#' clocs.threshold(NULL)
#'
clocs.threshold <- function(clocations, minsize=1L, reduce=TRUE,
                            use.threads=lx.use.threads()) {
  if (reduce)
    clocations <- clocs.reduce(clocations, use.threads=use.threads)
  valid <- (clocations[,3] - clocations[,2] + 1L) >= minsize
  clocs.matrix(clocations[valid,,drop=FALSE])
}

# -------------------------------------------------
#' transform a matrix of clocations to bitfield(s)
#' @description
#' transform a matrix of clocations to bit bitfield(s) of 
#' allowed positions on specified chromosome(s).\cr
#' bitfield(s) are defined in \code{bit} library\cr
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param clocations nx3 matrix of relative clocations (1-based)
#' @param chrs vector (possibly scalar) of chromosome indexes (not names) to work on
#' @param save.mem save memory at expense of speed (see details)
#' @param use.threads (see \link{lx.use.threads})
#' @return named list (possibly of size 0) of bitfields
#' @details
#' by default, this function internally works using logicals.
#' This requires N bytes of memory per chromosome, 
#' where N is the size of each chromosome.
#' The \code{save.mem} parameter will force using bitfields internally.
#' This results in a 30 fold reduction of memory size at expense of speed.
#' If memory is short, also consider using \code{use.threads = FALSE} to
#' proceed each chromosome sequentially.
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' slocs <- c("seq1:1-10", "seq1:15-20", "seq2:2-5") 
#' clocs <- clocs.matrix(lapply(slocs, sloc2cloc, handle=fh))
#' bits <- clocs2bits(fh, clocs)
#' empty <- clocs2bits(fh, clocs.matrix(NULL))
#' basta.close(fh)
#
clocs2bits <- function(handle, clocations, chrs=unique(clocations[,1]),
                       save.mem=FALSE, use.threads=lx.use.threads()) {

  use.threads = use.threads && (length(chrs) > 1)
  
  clocations <- clocations[clocations[,1] %in% chrs,,drop=FALSE]
  
  clocations <- clocs.rsplit(clocations)

  lx.out("computing bits on chrs: [", paste(names(clocations), collapse=", "), "]")
  res <- lx.napply(clocations, function(chr, clocs) {
     chr <- as.integer(chr)
     lx.out("  computing bits on chrindex: ", chr, level="debug")
     size <- handle$header$seq[[chr]]$size
     clocs[,2] <- pmax(1, clocs[,2])
     clocs[,3] <- pmin(size, clocs[,3])
     res <- if (save.mem) bit::bit(size) else logical(size)
     for (i in seq_len(nrow(clocs)))
       res[clocs[i,2]:clocs[i,3]] <- TRUE
     bit::as.bit(res)
  }, use.threads=use.threads, mc.preschedule=FALSE, SIMPLIFY=FALSE)

  res
}

# -------------------------------------------------
#' find runs of TRUE's in bitfield
#' @description
#' considering a single bitfield (usually representing
#' allowed positions on a chromosome), this function
#' will recover all runs of TRUE (larger than the given
#' threshold) and return them as a nx3 matrix of clocations
#' (with specified chromosome index \code{chr}).
#' @param bit a bitfield (see package bit)
#' @param chr default chrindex
#' @param minsize minimum number of consecutive TRUE to report (see details)
#' @param p0 region origin (see details)
#' @param delta region size factor (see details)
#' @return nx3 matrix of clocations
#' @details
#' \code{p0} and \code{delta} are two parameters to transform indices
#' in bitfileds into actual positions on chromosomes according to:\cr
#'  \code{       pos = p0 + (i-1) * delta}\cr
#' this is useful when indices actually correspond to binned values
#' (delta=binsize) or to regions that do not start at 1
#' (p0 = from).\cr
#' when using delta!=1, the minsize parameter is interpreted with the
#' transformation applied (e.g with delta=1000 and minsize=1000, a single
#' TRUE will actually pass the test)
#' @seealso \link{bits2clocs} for a list version
#' @examples
#' b <- bit::as.bit(c(TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE,FALSE))
#' runs2clocs(b)
#' runs2clocs(b, minsize=3)
#' runs2clocs(b, delta=1000, minsize=1000) 
#' runs2clocs(b, delta=1000, minsize=2000) 
#'
runs2clocs <- function(bit, chr=0, minsize=1L, p0=1L, delta=1L) {
  if (lx.warnif(! bit::is.bit(bit), "runs2clocs requires a bit::bit object"))
    return(clocs.matrix(NULL))

  .fact <- function(i) {p0 + (i-1) * delta}
    
  left  <- c(bit::as.bit(FALSE), bit)
  right <- c(bit, bit::as.bit(FALSE))

  begs <- .fact(bit::as.which(right & ! left))
  ends <- .fact(bit::as.which(left  & ! right) - 1L) + delta - 1

  val <- (ends - begs + 1) >= minsize
  res <- if (sum(val) == 0) NULL else cbind(as.integer(chr), begs[val], ends[val])
  
  clocs.matrix(res)
}

# -------------------------------------------------
#' transform bitfields into matrix of clocations
#' @description
#' considering a \bold{list of bitfields} (each of them representing
#' allowed positions on a chromosome), this function
#' will recover runs of TRUE's (larger than the given
#' threshold) on each of them and return them as a nx3 matrix of clocations.
#' the input list should be named by the chromosome indexes.
#' @param bits named list of bitfields (see note)
#' @param minsize minimum region size
#' @param p0 region origin (see details)
#' @param delta region size factor (see details)
#' @param use.threads (see \link{lx.use.threads})
#' @return a matrix of clocations
#' @seealso \link{runs2clocs} for single bitfield version
#' @details
#' names of the \code{bits} parameter are chromosome indexes
#' (in order to put them into clocations)\cr
#' \code{p0} and \code{delta} are two parameters to transform indices
#' of TRUE's in bitfields into actual positions on chromosomes according to:\cr
#'  \code{       pos = p0 + (i-1) * delta}\cr
#' this is useful when indices actually correspond to binned values
#' (delta=binsize) or to regions that do not start at 1
#' (p0 = from)
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' slocs <- c("seq1:1-10", "seq1:15-20", "seq2:2-3") 
#' clocs <- clocs.matrix(lapply(slocs, sloc2cloc, handle=fh))
#' bits <- clocs2bits(fh, clocs)
#' rclocs <- bits2clocs(bits)
#' identical(clocs, rclocs)
#' rclocs <- bits2clocs(bits, 5)
#' rclocs <- bits2clocs(bits, 50)
#' bits[[1]] <- bit::bit(length(bits[[1]]))
#' rclocs <- bits2clocs(bits)
#' bits[[2]] <- bit::bit(length(bits[[2]]))
#' rclocs <- bits2clocs(bits)
#' basta.close(fh)
#'
bits2clocs <- function(bits, minsize=1L, p0=1L, delta=1L,
                       use.threads=lx.use.threads()) {

  use.threads = use.threads && (length(bits) > 1)
    
  lx.out("computing clocs on chrs: [", paste(names(bits), collapse=", "), "]")
    
  res <- lx.napply(bits, function(chr, b) {
    lx.out("  computing clocs on chr: ", chr, level="debug")
    runs2clocs(b, chr=chr, minsize=minsize, p0=p0, delta=delta)
  }, use.threads=use.threads, mc.preschedule=FALSE, SIMPLIFY=FALSE)
    
  clocs.rbind(res)
}

# -------------------------------------------------
#' sample absolute point locations on chromosomes
#' @description
#' sample locations within regions specified by clocations.
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param clocations regions where we can sample (endpoints included)
#' @param size number of points to sample
#' @param replace sample with replacement (see \link{lx.sample})
#' @return vector of \code{size} absolute point coordinates.
#' (see \link{coords2clocs} to transform into clocations)
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' clocs <- basta2clocs(fh)
#' samp <- coords.sample(fh, clocs, size=10)
#' coords2clocs(fh, sort(samp))
#' basta.close(fh)
#' @note
#' if replace=FALSE and N, the number of points in the union
#' of all regions, is less than 2*size then downsample to N/2 points
#'
coords.sample <- function(handle, clocations, size=1000000L, replace=FALSE) {

   coords <- clocs2coords(handle, clocations)
   lens   <- coords[,2] - coords[,1] + 1
   
   #
   # determine total region size
   #
   tot.size <- sum(lens)
   lx.out("sampling ", nrow(clocations), " regions of total size ", tot.size, " bp")
   if ((! replace) && (tot.size < (2*size))) {
     lx.warn("not enough region space to sample ", size, " with replace=FALSE",
             " downsampling to ", tot.size/2)
     size <- tot.size / 2
   }

   #
   # sample uniformly
   #
   picks <- ceiling(lens * size / tot.size)
   
   samp <- lx.mapply(function(f, l, p) {
                f - 1 + sample(l, size=p, replace=replace)
           }, coords[,1], lens, picks, use.threads=FALSE)
   
   #
   # resample using prob weights = region length
   # for sampling correction.
   #
   # here, it is important to use lx.sample and not base::sample.
   # when replace=FALSE this is much quicker.
   #

   lx.out("resampling to size ", size)
   prob <- lx.mapply(function(l, p) {
                rep(l,p)
            }, lens, picks, use.threads=FALSE)

   samp <- unlist(samp, use.names=FALSE)
   prob <- unlist(prob, use.names=FALSE)

   if (length(samp) <= 1)
     return(samp)
   
   # open question: should we sample with replace=replace 
   # or replace=FALSE here ?
   
   lx.sample(samp, size=size, prob=prob, replace=replace)
}

