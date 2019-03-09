# -------------------------------------------------
# $Id: xlx.basta.r 376 2018-11-20 20:44:11Z viari $
# xlx basta format
#

# -------------------------------------------------
#' @name HELP.BASTA
#' @docType data
#' @title Basta format
#' @description
#' Basta file format is similar to Fasta format
#' but allow indexed access to sequences.\cr
#' this is a little endian binary file composed of
#' a header:\cr
#' \tabular{lll}{
#' int32 \tab 0x62617332 \tab magic number ('bas2')\cr
#' int32 \tab nbseq \tab number of sequences\cr
#' ---- \tab ------------ \tab -------\cr
#' \tab \tab repeat nbseq times\cr
#' ---- \tab ------------ \tab -------\cr
#' string \tab namei \tab name of sequence i (see 1)\cr
#' int64 \tab sizei \tab length of sequence i\cr
#' int32 \tab crc32i \tab crc32 of sequence i\cr
#' ---- \tab ------------ \tab -------\cr
#' }
#' followed by the concatenation of all sequences
#' as character arrays (not NULL terminated).
#'
#' (1) string format is:
#' \tabular{lll}{
#' int32 \tab size \tab string length\cr
#' bytes \tab size + 1 \tab NULL terminated char array\cr
#'}
#'
#' @note
#' When opening a basta file, the header is loaded
#' into memory but not the sequences. Sequences
#' are directly accessed from disk.
#'
#' @note
#' Conversion from fasta to basta is performed by the
#' external C executable \code{fasta2basta} provided
#' in Csrc directory.
#'
NULL

# =================================================
# globals
# =================================================

.basta.MAGIC <- 0x62617332  # 'bas2'

# =================================================
# internals <no_export>
# =================================================

# -------------------------------------------------
# <internal> <no_export>
#  check sequence crc32
#  this is an internal function (used in basta.open)
#  and not exported
#
.basta.check.crc32 <- function(handle, seqname) {
  seq <- handle$header$seq[[seqname]]
  dna <- basta.fetch.cloc(handle, as.integer(c(seq$index, 1, seq$size)))
  crc <- digest::digest(dna, algo="crc32", serialize=F, length=seq$size)
  ref <- lx.tobase(seq$crc32)
  ok  <- (crc == ref)
  if (ok) 
    lx.out("crc checksum sequence ", seq$name, " ok", level="debug")
  else
    lx.out("crc checksum sequence ", seq$name, " ", crc, " != ", ref, level="debug")
  ok
}

# -------------------------------------------------
# <internal> <no_export>
# read basta header
#  this is an internal function (used in basta.open)
#  and not exported
#
.basta.read.header <- function(handle) {
  header <- list()
  
  header$magic = lx.read.int32(handle)
  lx.stopif(header$magic != .basta.MAGIC, "bad magic header", up.frame=1)

  header$nbseq <- lx.read.int32(handle)
  
  header$seq <- list()
  header$baseoffset <- 8
  offset <- 0
  
  for (e in 1:header$nbseq) {
    nam <- lx.read.string(handle)
    siz <- lx.read.int64(handle)
    crc <- lx.read.int32(handle)
    header$seq[[nam]] <- list(index=e, name=nam, size=siz, crc32=crc, offset=offset)
    header$baseoffset <- header$baseoffset + nchar(nam) + 17
    lx.out("header: index=", e, " name=", nam, " size=", siz,
              " crc32=", lx.tobase(crc, prefix="0x"),
              " offset=", offset,
              level="debug", up.frame=1)
    offset <- offset + siz
  }
  
  # keep offsets and seq sizes here to speedup some operations
  
  header$offsets <- sapply(header$seq, function(x) x$offset)
  header$sizes   <- sapply(header$seq, function(x) x$size)
  
  handle$header <- header
  handle
}


# =================================================
# API
# =================================================

# -------------------------------------------------
#' open basta file
#' @description
#' open basta file for reading
#' @param filename basta file name
#' @param check.crc32 perform crc32 check
#' @return basta file handle
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"), check.crc32=TRUE)
#' basta.close(fh)
#'
basta.open <- function(filename, check.crc32=FALSE) {

  handle <- lx.open(filename)
  handle <- .basta.read.header(handle)
  handle$type <- "basta"

  # check crc32 if requested
  if (check.crc32)
    sapply(handle$header$seq, function(x) .basta.check.crc32(handle, x$name))

  handle
}

# -------------------------------------------------
#' close basta file
#' @description
#' same as \link{lx.close}
#' @param handle file handle (opened by \link{basta.open})
#'
basta.close <- lx.close

# -------------------------------------------------
#' convert seqname to seqindex
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param seqname character vector of sequence name(s)
#' @param silent logical keep silent if seqname(s) not found
#' @return integer vector of 1-based sequence index or of 0 if seqname(s) not found
#' @seealso \link{basta.index2name}
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' basta.name2index(fh, "seq1")
#' basta.index2name(fh, 2)
#' basta.name2index(fh, c("seq1", "nothere"))
#' basta.index2name(fh, 1:3)
#' basta.close(fh)
#'
basta.name2index <- function(handle, seqname, silent=FALSE) {
  nams <- names(handle$header$seq)
  sapply(seqname, function(nam) {
    ok <- nam %in% nams
    lx.warnif(! (ok || silent), nam, " : name not found", up.frame=3)
    if (ok) handle$header$seq[[nam]]$index else 0
  })
}

# -------------------------------------------------
#' convert seqindex to seqname
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param seqindex integer vector of 1-based sequence index
#' @param silent logical keep silent if index(es) out of bounds
#' @return character vector of seq name(s) or NULL if index(es) out of bounds
#' @seealso \link{basta.name2index}
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' basta.name2index(fh, "seq1")
#' basta.index2name(fh, 2)
#' basta.name2index(fh, c("seq1", "nothere"))
#' basta.index2name(fh, 1:3)
#' basta.close(fh)
#'
basta.index2name <- function(handle, seqindex, silent=FALSE) {
  sapply(seqindex, function(indx) {
    ok <- (indx > 0) && (indx <= handle$header$nbseq)
    lx.warnif(! (ok || silent), indx, " : index out of bounds", up.frame=3)
    if (ok) handle$header$seq[[indx]]$name else NULL
  })
}

# -------------------------------------------------
#' fetch sequence using absolute coordinates
#' @param handle basta file handle (as returned by \link{basta.open})
#' @param coord absolute coordinates (c(absfrom, absto)) (1-based) or
#'        single absolute position.
#' @return sequence string
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' basta.fetch.coord(fh, c(25, 34))
#' basta.fetch.sloc(fh, "seq2:1-10")
#' basta.fetch.cloc(fh, c(2, 1, 10))
#' basta.close(fh)
#'
basta.fetch.coord <- function(handle, coord) {
  if (length(coord) == 1) coord <- c(coord, coord)
  if (coord[1] > coord[2]) {
    lx.warn("invalid coordinates : ", coord)
    return("")
  }
  lx.seek(handle, handle$header$baseoffset + coord[1] - 1)
  lx.read.char(handle, coord[2]-coord[1]+1, .raw=TRUE)
}

# -------------------------------------------------
#' fetch sequence using relative clocation
#' @param handle basta file handle (as returned by \link{basta.open})
#' @param clocation relative clocation = c(seqname, from, to) (1-based)
#' @param truncate truncate 3' to seq.size if needed
#' @return sequence string
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' basta.fetch.coord(fh, c(25, 34))
#' basta.fetch.sloc(fh, "seq2:1-10")
#' basta.fetch.cloc(fh, c(2, 1, 10))
#' basta.close(fh)
#' 
basta.fetch.cloc <- function(handle, clocation, truncate=TRUE) {
  coord <- cloc2coord(handle, clocation, truncate=truncate)
  basta.fetch.coord(handle, coord)
}

# -------------------------------------------------
#' fetch sequence using relative slocation
#' @param handle basta file handle (as returned by \link{basta.open})
#' @param slocation relative slocation ("seqname:from-to")
#' @param zero.based.loc given slocation is 0-based
#' @param truncate truncate 3' to seq.size if needed
#' @return sequence string
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' basta.fetch.coord(fh, c(25, 34))
#' basta.fetch.sloc(fh, "seq2:1-10")
#' basta.fetch.cloc(fh, c(2, 1, 10))
#' basta.close(fh)
#' 
basta.fetch.sloc <- function(handle, slocation, zero.based.loc=FALSE, truncate=TRUE) {
  coord <- sloc2coord(handle, slocation, zero.based.loc=zero.based.loc, truncate=truncate)
  basta.fetch.coord(handle, coord)
}

# -------------------------------------------------
#' fetch sequence at several relative point locations
#' @description
#' \code{relpts} s a set of relative \bold{point} positions on the same chromosome.
#' this function returns sequence of length size starting at each position.
#' this formaly equivalent to:\cr
#' \code{clocs <- lapply(relpts, function(x) c(chrindex, x, x+size-1))}\cr
#' \code{res <- unlist(lapply(clocs, basta.fetch.cloc, handle=handle))}\cr
#' but is much quicker when relpts vector is large and values
#' span most of the chromosome.\cr
#' The idea is to load the chromosome
#' counts by chunks of size \code{.chunk.size} instead of accessing each
#' location individually (thus reducing disk access overhead).
#' @param handle file handle (as returned by \link{basta.open})
#' @param chr chromosome index (if integer) or chromosome name (if character)
#' @param relpts vector of relative positions (1-based) on this
#' chromosome
#' @param size size of sequence to fetch
#' @param .chunk.size <internal parameter> size of chunk. changing this parameter
#' will only affect time or memory used, not result.
#' @return array of character string giving the sequence starting
#' at each point location.
#' @examples
#' basta <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' x <- basta.fetch.points.chr(basta, 1, 1:10)
#' y <- lx.strsplit(basta.fetch.cloc(basta, c(1,1,10)), "")
#' identical(x, y)
#' x <- basta.fetch.points.chr(basta, 1, 15:25, size=10)
#' basta.close(basta)
#'
basta.fetch.points.chr <- function(handle, chr, relpts, size=1L,
                                   .chunk.size=1000000L) {
  
  lx.stopif(size <= 0, "size parameter should be > 0")
  if (.chunk.size <= size) .chunk.size <- size
  chrindex <- if (is.character(chr)) basta.name2index(handle, chr) else chr
  
  lx.out("fetching ", length(relpts), " points on chr index ", chrindex, level="debug")
  chr.size <- handle$header$seq[[chrindex]]$size
  chunk.size <- min(chr.size, .chunk.size)
  
  chunk.from <- seq.int(min(relpts), min(chr.size, max(relpts)), by=chunk.size)
  
  #
  # keep initial points order
  #
  ptorder <- order(relpts)
  relpts <- relpts[ptorder]
  
  #
  # split into chunks (this will reorder points)
  #
  lx.out("  splitting in ", length(chunk.from), " chunks", level="debug")
  chunk.indx <- split(relpts, factor(findInterval(relpts, chunk.from),
                                     levels=seq_along(chunk.from)))
  
  chunk.clocs <- lapply(chunk.from, function(x) {
    as.integer(c(chrindex, x, min(x+chunk.size-1+size-1, chr.size)))
  })
  
  chunks <- mapply(function(x, y) {
    list(cloc=x, pos=y)
  }, chunk.clocs, chunk.indx, SIMPLIFY=FALSE)
  #
  # load chunks in turn and get counts for positions in each chunk
  #
  .load <- function(chunk) {
    seq <- if (length(chunk$pos) == 0) ""
    else basta.fetch.cloc(handle, chunk$cloc)
    lapply(chunk$pos - chunk$cloc[2] + 1, function(x) substr(seq, x, x+size-1))
  }
  
  lx.out("  loading in ", length(chunks), " chunks", level="debug")
  res <- unlist(lapply(chunks, .load), recursive=FALSE)
  
  #
  # put points back in initial order
  #
  lx.out("  reshaping results", level="debug")
  res <- unlist(res[order(ptorder)])
  
  res
}

# -------------------------------------------------
#' fetch symbols counts using absolute coordinates
#' @description 
#' fetch sequence thru \link{basta.fetch.coord} and
#' count the number of occurences of symbols specified
#' in \code{sym} in contiguous bins of size \code{binsize}
#' @param handle basta file handle (as returned by \link{basta.open})
#' @param coord absolute coordinates (c(absfrom, absto)) (1-based) or
#'        single absolute position.
#' @param sym vector of strings specifying symbols to be counted.
#' see details.
#' @param binsize size of bins (defaults to whole sequence length)
#' @param case.sensitive symbols in \code{sym} are case sensitive
#' @param drop drop the last bin if sequence length is not a muliple
#'        of binsize
#' @return a matrix or vector of counts. if \code{length(sym)==1} returns
#' a vector of symbol(s) counts for each bin position.
#' if \code{length(sym)>1} returns a matrix of symbols counts
#' with length(sym) columns and each row corresponds to each bin
#' position.
#' @details each string in \code{sym} specifies a set of symbols to be
#' counted. if this set starts with '!', it means symbols \bold{not} in
#' set. As a special case the string "Other" is equivalent to "!ACGT".
#' @seealso \link{basta.count.coord}
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' basta.fetch.coord(fh, c(1, 24))
#' # count all DNA symbols
#' basta.count.coord(fh, c(1, 24))
#' # count GC only
#' basta.count.coord(fh, c(1, 24), sym="GC")
#' # count GC in bins
#' basta.count.coord(fh, c(1, 24), sym="GC", binsize=4)
#' basta.close(fh)
#
basta.count.coord <- function(handle, coord, 
                              sym=c("A", "C", "G", "T", "other"),
                              binsize=diff(range(coord))+1,
                              case.sensitive=FALSE,
                              drop=TRUE) {

  if (! exists("dontCheck")) dontCheck <- identity # for R < 3.1
  
  .bit.sum <- function(bits, range)
    .Call("R_bit_sum", bits, as.integer(range), PACKAGE=dontCheck("bit"))

  len <- diff(range(coord))+1
  seq <- basta.fetch.coord(handle, coord)
  pos <- seq(from=1, to=len-binsize+1, by=binsize)
  rst <- if (drop) 0 else len %% binsize

  res <- lapply(sym, function(s) {
    issym <- bit::bit(len)
    if (tolower(s) == "other") s <- "!ACGT"
    isneg <- substring(s, 1, 1) == "!"
    if (isneg) s <- substring(s, 2)
    if (! case.sensitive) s <- paste0(tolower(s), toupper(s))
    issym[lx.strchr(seq, s)] <- TRUE
    if (isneg) issym <- ! issym
    res <- sapply(pos, function(p) .bit.sum(issym, c(p, p+binsize-1)))
    if (rst != 0) res <- c(res, .bit.sum(issym, c(len-rst+1, len)))
    res
  })
  
  res <- do.call(cbind, res)
  colnames(res) <- sym
  
  if (ncol(res) == 1) as.vector(res) else res
}

# -------------------------------------------------
#' fetch symbols counts using relative clocation
#' @description 
#' fetch sequence thru \link{basta.fetch.cloc} and
#' count the number of occurences of symbols specified
#' in \code{sym} in contiguous bins of size \code{binsize}
#' @param handle basta file handle (as returned by \link{basta.open})
#' @param clocation relative clocation = c(seqname, from, to) (1-based)
#' @param truncate truncate 3' to seq.size if needed
#' @param sym vector of strings specifying symbols to be counted.
#' see details.
#' @param binsize size of bins (defaults to whole sequence)
#' @param case.sensitive symbols in \code{sym} are case sensitive
#' @param drop drop the last window if sequence length is not a muliple
#'        of binsize
#' @return a matrix or vector of counts. if \code{length(sym)==1} returns
#' a vector of symbol(s) counts for each bin position.
#' if \code{length(sym)>1} returns a matrix of symbols counts
#' with length(sym) columns and each row corresponds to each bin
#' position.
#' @details each string in \code{sym} specifies a set of symbols to be
#' counted. if this set starts with '!', it means symbols \bold{not} in
#' set. As a special case the string "Other" is equivalent to "!ACGT".
#' @seealso \link{basta.count.coord}
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' basta.fetch.cloc(fh, c(1, 1, 24))
#' # count all DNA symbols
#' basta.count.cloc(fh, c(1, 1, 24))
#' # count GC only
#' basta.count.cloc(fh, c(1, 1, 24), sym="GC")
#' # count GC in bins
#' basta.count.cloc(fh, c(1, 1, 24), sym="GC", binsize=4)
#' basta.close(fh)
#
basta.count.cloc <- function(handle, clocation, truncate=TRUE,
                              sym=c("A", "C", "G", "T", "other"),
                              binsize=clocation[3]-clocation[2]+1,
                              case.sensitive=FALSE,
                              drop=TRUE) {

  coord <- cloc2coord(handle, clocation, truncate=truncate)
  basta.count.coord(handle, coord, sym=sym, binsize=binsize,
                    case.sensitive=case.sensitive, drop=drop) 
}

# -------------------------------------------------
#' make clocations spanning all chromosomes
#' @description
#' make clocations spanning all chromosomes declared in basta/baf file
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @return nx3 matrix of clocations
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' clocs <- basta2clocs(fh)
#' basta.close(fh)
#' 
basta2clocs <- function(handle) {
  x <- lapply(handle$header$seq, function(x) c(x$index, 1L, x$size))
  clocs.matrix(unlist(x, use.names=F))
}

# -------------------------------------------------
#' make absolute coordinates spanning all chromosomes
#' @description
#' make absolute coordinates spanning all chromosomes
#' declared in basta/baf file
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @return nx2 matrix of absolute coordinates (1-based)
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' coords <- basta2coords(fh)
#' basta.close(fh)
#' 
basta2coords <- function(handle) {
  clocs2coords(handle, basta2clocs(handle))
}
