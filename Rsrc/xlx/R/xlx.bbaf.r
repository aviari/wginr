# -------------------------------------------------
# $Id: xlx.bbaf.r 355 2017-12-11 07:50:48Z viari $
# xlx baf format
#

# -------------------------------------------------
#' @name HELP.BAF
#' @docType data
#' @title Baf format
#' @description
#' Baf is a binary format to compactly represents reads alignments
#' from bam files. It basically only keeps the information about
#' each allele counts at each position of the chromosomes
#' (all other information such as alignement quality, CIGAR etc...
#' is discarded). It can therefore be used to retrieve allelic frequencies,
#' total cover (sum of all alleles) or GC content (sum of G and C alleles).
#' 
#' This is a little endian binary file composed of
#' a header:\cr
#' \tabular{lll}{
#' int32 \tab 0x62696d31 \tab magic number ('baf1')\cr
#' int32 \tab nbseq \tab number of sequences\cr
#' ---- \tab ------------ \tab -------\cr
#' \tab \tab repeat nbseq times\cr
#' ---- \tab ------------ \tab -------\cr
#' string \tab namei \tab name of sequence i (see 1)\cr
#' int64 \tab sizei \tab length of sequence i\cr
#' int32 \tab codei \tab encoding size in bytes for this sequence (0, 1,2,4)\cr
#' ---- \tab ------------ \tab -------\cr
#' }
#' followed by the concatenation of nbseq arrays each of
#' 4 * sizei * codei bytes. each codei bytes (codei=1,2 or4)
#' represent total number of read bases (i.e non counting deletions) covering 
#' each position.\cr
#' (codei=1: unsigned char, codei=2: unsigned short, codei=4: unsigned int32,
#' and codei=0 means that all counts on this chromosome are 0)\cr.
#'
#' (1) string format is:
#' \tabular{lll}{
#' int32 \tab size \tab string length\cr
#' bytes \tab size + 1 \tab NULL terminated char array\cr
#'}
#'
#' @note
#' When opening a baf file, the header is loaded
#' into memory but not the count arrays. Counts
#' will be directly accessed from disk when needed.
#'
#' @note
#' Conversion from bam to baf is performed by the
#' external C executable \code{bam2baf} provided
#' in Csrc directory.
#'
#' @note
#' Baf header is compatible with Basta header (see \link{HELP.BASTA})
#' and Baf handles can therefore be passed as the handle argument of
#' most functions accepting a Basta handle (except of course for
#' those that need to access to sequence).
#'
#' @note
#' of course when using together a Basta and a Baf file, you should ensure
#' that sequences in both header are stricly identical. In practice this means that
#' the Bam file from which the baf file was generated was built using
#' the same fasta sequences from which the Basta file was generated.\cr
## see \link{baf.is.basta} to check coherency
#'
NULL

# =================================================
# globals
# =================================================

.baf.MAGIC   <- 0x62616631  # 'baf1'

# =================================================
# internals <no_export>
# =================================================

# -------------------------------------------------
# <internal> <no_export>
# read baf header
#  this is an internal function (used in baf.open)
#  and not exported
#
.baf.read.header <- function(handle) {

  header <- list()
  
  header$magic = lx.read.int32(handle)
  lx.stopif(header$magic != .baf.MAGIC, "bad magic header")

  header$nbseq <- lx.read.int32(handle)
  
  header$seq <- list()
  header$baseoffset <- 8
  off.read <- off.seq <- 0
  
  for (e in 1:header$nbseq) {
    nam <- lx.read.string(handle)
    siz <- lx.read.int64(handle)
    cod <- lx.read.int32(handle)
    header$seq[[nam]] <- list(index=e, name=nam, size=siz, code=cod,
                              off.read=off.read,
                              off.seq=off.seq,
                              offset=off.seq)
    header$baseoffset <- header$baseoffset + nchar(nam) + 17
    lx.out("header: index=", e, " name=", nam, " size=", siz,
           " encode=", cod,
           " off.read=", off.read,
           " off.seq=", off.seq,
           level="debug", up.frame=1)
    off.read <- off.read + 4 * siz * cod
    off.seq <- off.seq + siz
  }
  
  # keep offsets and sizes to speedup some operations
  
  header$off.reads <- sapply(header$seq, function(x) x$off.read)
  header$off.seqs  <- sapply(header$seq, function(x) x$off.seq)
  header$sizes     <- sapply(header$seq, function(x) x$size)

  # for compatibility with basta$header
  header$offsets   <- header$off.seqs 

  handle$header <- header
  handle
}

# -------------------------------------------------
# <internal> <no_export>
# read n read counts from baf file starting at given
# offset.
# reads 4*n elements (one element for each base in order 'A', 'C', 'G', 'T')
# and return an integer matrix of size nx4
#
.baf.fetch <- function(handle, offset, n, code) {
  if (code == 0) 
    fun <- function(h,n,signed=F) rep(0, n)
  else if (code == 1)
    fun <- lx.read.int8
  else if (code == 2)
    fun <- lx.read.int16
  else
    fun <- lx.read.int32

  lx.seek(handle, offset)
  
  res <- fun(handle, n=4*n, signed=F)
  
  res <- matrix(res, ncol=4, byrow=T, dimnames=list(NULL, c("A","C","G","T")))
  
  lx.out("fetched : ", nrow(res), " sites", level="debug", with.mem=TRUE)
  
  res
}

# -------------------------------------------------
# <internal> <no_export>
# determine mincov based on quantile
# count : nx4 count matrix
# quant : quantile * 1000
#
.mincov <- function(count, quant) {
  count <- rowSums(count)
  count <- count[count != 0]
  mincov <- max(0, quantile(count,  abs(quant/1000)))
  lx.out("mincov = ", mincov, " [", length(count), " data points]", level="debug")
  mincov
}

# -------------------------------------------------
# <internal> <no_export>
# empty matrix
#
.empty.mat <- function() {
  matrix(integer(0), ncol=4, nrow=0, dimnames=list(NULL, c("A","C","G","T")))
}


# =================================================
# API
# =================================================

# -------------------------------------------------
#' open baf file
#' @description
#' open baf file for reading
#' @param filename baf file name
#' @return baf file handle
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' baf.close(baf)
#'
baf.open <- function(filename) {
  handle <- lx.open(filename)
  handle <- .baf.read.header(handle)
  handle$type <- "baf"
  
  handle
}

# -------------------------------------------------
#' close baf file
#' @description
#' same as \link{lx.close}
#' @param handle file handle (opened by \link{baf.open})
#'
baf.close <- lx.close

# -------------------------------------------------
#' convert seqname to seqindex
#' @description
#' convert seqname to seqindex.\cr
#' see \link{basta.name2index} this is the same function
#' @inheritParams basta.name2index
#'
baf.name2index <- basta.name2index

# -------------------------------------------------
#' convert seqindex to seqname
#' @description
#' convert seqindex to seqname.\cr
#' see \link{basta.index2name} this is the same function
#' @inheritParams basta.index2name
#'
baf.index2name <- basta.index2name

# -------------------------------------------------
#' fetch allele counts
#' @description
#' fetch allele counts using absolute coordinates.\cr
#' \code{coord} defines a region on a chromosome. this function
#' returns a count matrix (with 4 columns) of
#' the number of symbols at each position within the region.
#' @param handle file handle (as returned by \link{baf.open})
#' @param coord absolute sequence coordinates (c(absfrom, absto)) (1-based)
#'               or a single position absfrom (this implies absto=absfrom)
#' @return integer matrix of size n x 4 containing allele counts.
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @seealso \link{baf.fetch.cloc}, \link{baf.bin.coord},
#' \link{baf.fetch.points.chr}, \link{baf.heterozygous.coord}
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' baf.fetch.coord(baf, c(33725, 33732))
#' i <- baf.name2index(baf, "machaon")
#' baf.fetch.cloc(baf, c(i, 560, 567))
#' baf.close(baf)
#' 
baf.fetch.coord <- function(handle, coord) {

  if (length(coord) == 1) coord <- c(coord, coord)

  off <- handle$header$off.seqs

  ifrom <- findInterval(coord[1] - 1, off)
  ito   <- findInterval(coord[2] - 1, off)
  
  if (ifrom != ito) {
    lx.warn("coordinates overlap chromosomes")
    return(NULL)
  }

  seq <- handle$header$seq[[ifrom]]

  coord <- coord - seq$off.seq
  
  off <- handle$header$baseoffset + seq$off.read + 4 * (seq$code * (coord[1] - 1))

  .baf.fetch(handle, off, coord[2] - coord[1] + 1, seq$code)
}

# -------------------------------------------------
#' fetch allele counts
#' @description
#' fetch allele counts using using relative coordinates.\cr
#' \code{cloc} defines a region on a chromosome. this function
#' returns a count matrix (with 4 columns) of
#' the number of symbols at each position within the region.
#' @param handle file handle (as returned by \link{baf.open})
#' @param clocation relative clocation = c(seqname, from, to) (1-based)
#' @return integer matrix of size n x 4 containing allele counts.
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @seealso \link{baf.fetch.coord}, \link{baf.bin.cloc},
#' \link{baf.fetch.points.chr}, \link{baf.heterozygous.cloc}
#' 
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' baf.fetch.coord(baf, c(33725, 33732))
#' i <- baf.name2index(baf, "machaon")
#' baf.fetch.cloc(baf, c(i, 560, 567))
#' baf.close(baf)
#'
baf.fetch.cloc <- function(handle, clocation) {
  coord <- cloc2coord(handle, clocation)
  baf.fetch.coord(handle, coord)
}

# -------------------------------------------------
#' fetch allele counts
#' @description
#' fetch allele counts at several relative point locations
#' on the same chromosome.\cr
#' \code{relpts} is a set of relative \bold{point} positions on the same chromosome.
#' this function returns a count matrix (with 4 columns) of
#' the number of symbols at each position.
#' this formaly equivalent to:\cr
#' \code{clocs <- lapply(relpts, function(x) c(chrindex, x, x))}\cr
#' \code{do.call(rbind, lapply(clocs, baf.fetch.cloc, handle=handle))}\cr
#' but is much quicker when relpts vector is large and values
#' span most of the chromosome.\cr
#' The idea is to load the allele
#' counts by chunks of size \code{.chunk.size} instead of accessing each
#' location individually (thus reducing disk access overhead).
#' @param handle file handle (as returned by \link{baf.open})
#' @param chr chromosome index (if integer) or chromosome name (if character)
#' @param relpts vector of relative positions (1-based) on this
#' chromosome
#' @param .chunk.size <internal parameter> size of chunk. changing this parameter
#' will only affect time or memory used, not result.
#' @return integer matrix of size n x 4 containing allele counts.
#' @seealso \link{baf.fetch.cloc}, \link{baf.fetch.coord} 
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' x <- baf.fetch.points.chr(baf, 3, 550:570)
#' y <- baf.fetch.points.chr(baf, 3, 550:570, .chunk.size=1)
#' identical(x, y)
#' baf.close(baf)
#'
baf.fetch.points.chr <- function(handle, chr, relpts, .chunk.size=1000000L) {

  if (length(relpts) == 0)
    return(.empty.mat())
  
  chrindex <- if (is.character(chr)) baf.name2index(handle, chr) else chr
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
                        as.integer(c(chrindex, x, min(x+chunk.size-1, chr.size)))
                        })

  chunks <- mapply(function(x, y) {
                        list(cloc=x, pos=y)
                        }, chunk.clocs, chunk.indx, SIMPLIFY=FALSE)
  #
  # load chunks in turn and get counts for positions in each chunk
  #
  .load <- function(chunk) {
     count <- if (length(chunk$pos) == 0) c()
            else baf.fetch.cloc(handle, chunk$cloc)
     count[chunk$pos - chunk$cloc[2] + 1,,drop=F]
  }
  
  lx.out("  loading in ", length(chunks), " chunks", level="debug")
  res <- lapply(chunks, .load)

  lx.out("  binding results", level="debug")
  res <- do.call(rbind, res)

  #
  # put points back in initial order
  #
  lx.out("  reshaping results", level="debug")
  res <- res[order(ptorder),,drop=F]
  
  res
}

# -------------------------------------------------
#' binning coverage or GC content using absolute coordinates
#' @description
#' \code{coord} defines a region on a chromosome. this function
#' collects coverage or GC content by bins of width \code{binsize} within the region.
#' @param handle file handle (as returned by \link{baf.open})
#' @param coord absolute sequence coordinates (c(absfrom, absto)) (1-based)
#'        or a single position absfrom (this implies absto=absfrom)
#' @param binsize size of bins
#' @param what what to collect "coverage" or "gc" (may be abrreviated)
#' @param fun collect function (e.g. sum, mean, median, user-closure, ...) see details.
#' @param drop drop the last element of result if region width is not a muliple
#'        of binsize
#' @param na.gc boolean to specify how to handle GC content for positions with
#'        0 coverage. na.gc = TRUE will produce 0/0 = NA and na.gc = FALSE
#'        will produce 0/0 = 0.
#' @param ... optional arguments to be passed to \code{fun}
#' @param .quick use a quicker algorithm (valid for fun=sum or fun=mean only and
#'        na.gc=FALSE) at the expense of memory overhead.
#' @return numeric vector of size n containing binned cover or gc content
#' @details
#' let us note allele.counts the binsize x 4 matrix of alleles counts in each
#' bin.\cr
#' if (what=="cover") then fun(coverage) is collected in each bin,\cr
#' with coverage = rowSums(allele.counts)\cr\cr
#' if (what=="gc") then fun(gc.line) is collected in each bin,\cr
#' with gc.line = rowSums(GC.allele.counts) / rowSums(allele.counts).
#' (with a special treatment of NA's. see below)\cr
#' Therefore fun=sum will produce the number of GC alleles in bin and 
#' fun=mean will produce the \%GC. Note that functions other than \code{sum}
#' or \code{mean} (e.g. \code{median}) are usually meaningless
#' with what=="gc"\cr\cr
#' \code{fun} can be any function or user-supplied closure
#' taking a numerical vector of size n<=binsize as input 
#' and returning a scalar.\cr
#' \bold{added in v1.7} if the formal argument of \code{fun} is named
#' \code{count} then fun argument is an integer matrix of size n<=binsize x 4
#' as returned by \link{baf.fetch.coord} giving each allele at each position.
#' As before it should return a numerical scalar.\cr\cr
#' the \code{na.gc} parameter is intended to handle the special case where
#' coverage=0 at a position.\cr
#' Then the computed gc.line at this position is
#' NA if na.gc=TRUE, and 0 if na.gc=FALSE.\cr
#' Please note that na.gc=TRUE will disable quick mode.\cr\cr
#' the \code{drop} parameter handles the last bin when 
#' region width is not a muliple of binsize.\cr
#' if drop=TRUE then the last (incomplete) bin is omited.
#' if drop=FALSE then the last (incomplete) bin is included.
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @seealso \link{baf.fetch.coord} \link{baf.bin.cloc}
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' baf.fetch.coord(baf, c(33725, 33730))
#' # 'fun' usage
#' baf.bin.coord(baf, c(33725, 33730), 3) # sum
#' baf.bin.coord(baf, c(33725, 33730), 3, fun=mean)
#' baf.bin.coord(baf, c(33725, 33730), 3, fun=median)
#' # 'na.gc' usage
#' baf.bin.coord(baf, c(33725, 33730), 3, what="gc")
#' baf.bin.coord(baf, c(33725, 33730), 3, what="gc", na.gc=TRUE)
#' baf.bin.coord(baf, c(33725, 33730), 3, what="gc", na.gc=TRUE, na.rm=TRUE)
#' # 'drop' usage
#' baf.bin.coord(baf, c(33725, 33730), 4)
#' baf.bin.coord(baf, c(33725, 33730), 4, drop=FALSE)
#' baf.bin.coord(baf, c(33725, 33730), 10)
#' baf.bin.coord(baf, c(33725, 33730), 10, drop=FALSE)
#' baf.close(baf)
#
baf.bin.coord <- function(handle, coord, binsize=10000L, 
                 what=c("coverage", "gc"), fun=sum, drop=TRUE, na.gc=FALSE, ...,
                 .quick=any(sapply(c(sum, mean), identical, fun)) && (!na.gc)) {
  
  what <- match.arg(what)
  
  .quick <- .quick && any(sapply(c(sum, mean), identical, fun)) && (!na.gc)
  
  rest <- if (drop) 0 else (coord[2]-coord[1]+1) %% binsize

  .pload <- function(coor) {
    baf.fetch.coord(handle, coor)
  }
  
  .qload <- function(coor) {
    tmp <- baf.fetch.coord(handle, coor)
    res <- rowSums(tmp)
    if (what=="gc")
      res <- (tmp[,"G"]+tmp[,"C"]) / (if (na.gc) res else pmax(1, res))
    res
  }
  
  if (.quick) {
    res <- .qload(coord)
    res <- lx.binsum(res, binsize, drop=drop)
    if (identical(fun, mean)) {
      res <- res / binsize
      if (rest != 0) 
        res[length(res)] <- res[length(res)] * binsize / rest
    }
  } else {
    .load <- if (identical(names(formals(fun)[1]), "count")) .pload else .qload
    res <- if (coord[2] - coord[1] + 1 < binsize) numeric(0)
           else sapply(seq.int(coord[1], coord[2]-binsize+1, by=binsize),
                       function(pos) fun(.load(c(pos, pos+binsize-1)), ...))
    if (rest != 0)
      res <- c(res, fun(.load(c(coord[2]-rest+1, coord[2]))))
  }
  res
}

# -------------------------------------------------
#' binning coverage or GC content using relative coordinates
#' @description
#' \code{clocation} defines a region on a chromosome. this function
#' collects coverage or GC content by bins of width \code{binsize} within the region.
#' @param handle file handle (as returned by \link{baf.open})
#' @param clocation relative clocation = c(seqname, from, to) (1-based)
#' @param binsize size of bins
#' @param what what to collect "coverage" or "gc" (may be abrreviated)
#' @param fun collect function (e.g. sum, mean, median, user-closure, ...) see details.
#' @param drop drop the last element of result if region width is not a muliple
#'        of binsize
#' @param na.gc boolean to specify how to handle GC content for positions with
#'        0 coverage. na.gc = TRUE will produce 0/0 = NA and na.gc = FALSE
#'        will produce 0/0 = 0.
#' @param ... optional arguments to be passed to \code{fun}
#' @param .quick use a quicker algorithm (valid for fun=sum or fun=mean only and
#'        na.gc=FALSE) at the expense of memory overhead.
#' @return numeric vector of size n containing binned cover or gc content
#' @details
#' let us note allele.counts the binsize x 4 matrix of alleles counts in each
#' bin.\cr
#' if (what=="cover") then fun(coverage) is collected in each bin,\cr
#' with coverage = rowSums(allele.counts)\cr\cr
#' if (what=="gc") then fun(gc.line) is collected in each bin,\cr
#' with gc.line = rowSums(GC.allele.counts) / rowSums(allele.counts).
#' (with a special treatment of NA's. see below)\cr
#' Therefore fun=sum will produce the number of GC alleles in bin and 
#' fun=mean will produce the \%GC. Note that functions other than sum
#' or mean are usually meaningless with what=="gc"\cr\cr
#' \code{fun} can be any function or user-supplied closure
#' taking a numerical vector of size n<=binsize as input and 
#' returning a scalar.\cr
#' \bold{added in v1.7} if the formal argument of \code{fun} is named
#' \code{count} then fun argument is an integer matrix of size n<=binsize x 4
#' as returned by \link{baf.fetch.coord} giving each allele at each position.
#' As before it should return a numerical scalar.\cr\cr
#' the \code{na.gc} parameter is intended to handle the special case where
#' coverage=0 at a position.\cr
#' the \code{na.gc} parameter is intended to handle the special case where
#' coverage=0 at a position.\cr
#' Then the computed gc.line at this position is
#' NA if na.gc=TRUE, and 0 if na.gc=FALSE.\cr
#' Please note that na.gc=TRUE will disable quick mode.\cr\cr
#' the \code{drop} parameter handles the last bin when 
#' region width is not a muliple of binsize.\cr
#' if drop=TRUE then the last (incomplete) bin is omited.
#' if drop=FALSE then the last (incomplete) bin is included.
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @seealso \link{baf.fetch.cloc} \link{baf.bin.coord}
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' i <- baf.name2index(baf, "machaon")
#' baf.fetch.cloc(baf, c(i, 560, 565))
#' # 'fun' usage
#' baf.bin.coord(baf, c(33725, 33730), 3) # sum
#' baf.bin.cloc(baf, c(i, 560, 565), 3, fun=mean)
#' baf.bin.cloc(baf, c(i, 560, 565), 3, fun=median)
#' # 'na.gc' usage
#' baf.bin.cloc(baf, c(i, 560, 565), 3, what="gc")
#' baf.bin.cloc(baf, c(i, 560, 565), 3, what="gc", na.gc=TRUE)
#' baf.bin.cloc(baf, c(i, 560, 565), 3, what="gc", na.gc=TRUE, na.rm=TRUE)
#' # 'drop' usage
#' baf.bin.cloc(baf, c(i, 560, 565), 4)
#' baf.bin.cloc(baf, c(i, 560, 565), 4, drop=FALSE)
#' baf.bin.cloc(baf, c(i, 560, 565), 10)
#' baf.bin.cloc(baf, c(i, 560, 565), 10, drop=FALSE)
#' baf.close(baf)
baf.bin.cloc <- function(handle, clocation, binsize=10000L,
                what=c("coverage", "gc"), fun=sum, drop=TRUE, na.gc=FALSE, ...,
                .quick=any(sapply(c(sum, mean), identical, fun)) && (!na.gc)) {
  coord <- cloc2coord(handle, clocation)
  baf.bin.coord(handle, coord, binsize=binsize, what=what, fun=fun,
                drop=drop, na.gc=na.gc, ..., .quick=.quick)
}

# -------------------------------------------------
#' get sample of alleles counts
#' @description
#' sample positions on all chromosomes and return allele counts
#' @param handle file handle (as returned by \link{baf.open})
#' @param chrs integer (chromosome indexes) or character (chromosome names) vector
#' specifying which chromosomes to use. Use NULL to specify all
#' chromosome declared in baf header.
#' @param sample.size sample size (set to +Inf to collect all positions)
#' @param .seed random seed (for reproducibility). use a strictly negative
#' integer to disable seeding.
#' @param use.threads (see \link{lx.use.threads})
#' @return integer matrix of size sample.size x 4 containing allele counts.
#' with rownames of the form 'chrindex.position'.
#' @note you may set sample.size to +Inf to collect allele counts on all position.
#' but be careful this may use a very large amount of memory.
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' smp <- baf.sample(baf, sample.size=100, .seed=0, use.threads=FALSE)
#' baf.close(baf)
#'
baf.sample <- function(handle, chrs=NULL, sample.size=1e6L, .seed=-1L,
                       use.threads=lx.use.threads()) {

  # <internal> sample on one chromosome
  # will be possibly run within thread
  .baf.sample.chr <- function(handle, chrindex, sample.size, .seed) {
    if (.seed >= 0) set.seed(.seed)
    chr.size <- handle$header$seq[[chrindex]]$size
    smp.size <- min(sample.size, chr.size)
    pos <- sort(sample(chr.size, smp.size, replace=F))
    res <- baf.fetch.points.chr(handle, chrindex, pos)
    rownames(res) <- pos
    res
  }

  if (is.null(chrs))
    chrs <- seq.int(handle$header$nbseq)
  
  if (is.character(chrs))
    chrs <- sapply(chrs, function(chr) baf.name2index(handle, chr))
  
  # truncate chrs if needed
  chrs <- intersect(chrs, seq.int(handle$header$nbseq))

  # prepare seeds for random
  if (.seed >= 0) {
    set.seed(.seed)
    chr.seed <- sample(length(chrs), length(chrs), replace=F)
  } else {
    chr.seed <- rep(.seed, length(chrs))
  }
  names(chr.seed) <- chrs
  
  # truncate .sample.size if needed
  sum.size <- sum(sapply(chrs, function(chr) handle$header$seq[[chr]]$size))
  sample.size <- min(sample.size, sum.size)

  # sample by chromosome
  res <- lx.happly(chrs, function(chrindex, handle, seeds) {
    size <- ceiling(handle$header$seq[[chrindex]]$size * sample.size / sum.size)
    lx.out(size)
    .baf.sample.chr(handle, chrindex, sample.size=size, .seed=seeds[[as.character(chrindex)]])
  }, handle=handle, seeds=chr.seed, use.threads=use.threads)
  
  # add rownames
  chr.nams <- unlist(mapply(rep, chrs, sapply(res, nrow), USE.NAMES=F))
  res <- do.call(rbind, res)
  rownames(res) <- paste(chr.nams, rownames(res), sep=".")
  
  # resample to proper size
  if (sample.size != sum.size) {
    isamp <- sort(sample(nrow(res), sample.size, replace=F))
    res <- res[isamp,]
  }
  
  res
}

# -------------------------------------------------
#' filter allele counts
#' @description filter allele counts according to coverage, min and max
#' number of alleles and max allele frequency (see details).
#' @details 
#' \bold{definitions}\cr
#' an allele X is present iff:\cr
#' \itemize{\item count_X > \code{lowread}}
#' a site (i.e. a row of \code{count}) is valid iff:\cr
#' \itemize{
#' \item coverage >= \code{covmin}
#' \item \code{minall} <= nb_alleles <= \code{maxall}
#' \item if (nb_alleles > 1) abs(0.5 - count_max_allele / coverage) <= \code{deltafreq}
#' }
#' 
#' if \code{mincov} is < 0 then \code{mincov} is computed as:\cr
#' \code{quantile(coverage, -mincov/1000)}\cr
#' 
#' if \code{delta.freq} == NA or maxall < 2 then freq condition is ignored
#' 
#' @param count nx4 integer matrix of alleles counts.
#' as returned by \code{baf.fetch.xxx}, \code{baf.sample.xxx} or
#' \code{baf.heterozygous.xxx}.
#' @param lowread low read threshold (see details)
#' @param mincov minimal coverage (see details)
#' @param minall minimal number of alleles (see details)
#' @param maxall maximal number of alleles (see details)
#' @param deltafreq max allele frequency (see details)
#' @param what kind of result to return (see value)
#' @return \itemize{
#' \item if (what == "count") filtered count matrix
#' \item if (what == "index") indexes of valid rows
#' \item if (what == "logical") logical vector indicating valid rows
#' }
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' cnt <- baf.sample(baf, sample.size=Inf)
#' cnt.ok <- baf.count.filter(cnt, lowread=2, mincov=10, minall=1, maxall=4, deltafreq=NA)
#' baf.close(baf)
#
# note: since count can be very large (> 10e8 rows) this implementation
#       makes maximum use of vector operations
#
baf.count.filter <- function(count, lowread=2L, mincov=10L, minall=1L, maxall=2L,
                             deltafreq=0.1, what=c("count", "index", "logical")) {
  
  # determine mincov  
  if (mincov < 0)
    mincov <- .mincov(count, mincov)

  # get total coverage
  cov <- rowSums(count)

  # filter mincov
  ok <- (cov >= mincov)
  
  # filter nb alleles
  nball <- integer(nrow(count))
  for (i in 1:4) nball <- nball + (count[,i] > lowread)
  ok <- ok & (nball >= minall) & (nball <= maxall)
  
  # filter max allele frequency
  if ((! is.na(deltafreq)) && (maxall >= 2)) {
    frq <- lx.rowMaxs(count, ties.method="first")
    frq <- frq / pmax(1, cov)
    ok <- ok & ((nball < 2) | (abs(frq - 0.5) <= deltafreq))
    frq <- NULL
  }
  nball <- NULL
    
  # select result
  count <- switch(what[1],
                  logical=ok,
                  index=seq.int(nrow(count))[ok],
                  count[ok,,drop=F])
  
  lx.out("filtered sites : ", sum(ok), " / ", length(ok), level="debug", with.mem=TRUE)
  
  count
}

# -------------------------------------------------
#' get genotype from allele counts
#' @description retrieve genotypes from allele counts matrix
#' @param count nx4 integer matrix of alleles counts.
#' as returned by \code{baf.fetch.xxx}, \code{baf.sample.xxx} or
#' \code{baf.heterozygous.xxx}.
#' @param lowread low read threshold (see details)
#' @param what kind of result to return (see value)
#' @param sorted sort result by frequencies (see details)
#' @param use.threads (see \link{lx.use.threads})
#' @details 
#' this function returns the list of allele(s) present at each
#' row of the \code{count} matrix.\cr
#' an allele X is present iff: \code{count_X > lowread}\cr
#' the typeof result depends upon the \code{what} parameter:\cr
#' \itemize{
#' \item if (what == "symbol") list of character array of alleles symbols
#' \item if (what == "index") list of integer array of alleles indices
#' \item if (what == "string") array of genotype strings
#' }
#' if \code{sorted}==FALSE (default) each array element of the result list
#' (or each character in string) appears in the same order as in \code{colnames(count)}
#' whatever the frequency value.
#' if \code{sorted}==TRUE then array elements are sorted by decreasing frequency.
#' @return list of alleles or array of strings (see details)
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' cnt <- baf.sample(baf, sample.size=Inf)
#' # sample: count all genotypes
#' cnt.ok <- baf.count.filter(cnt, lowread=0, mincov=10, minall=1, maxall=4, deltafreq=NA)
#' geno <- baf.count.genotype(cnt.ok, lowread=0)
#' table(nb.all <- sapply(geno, length))
#' # sample: ordered bi-allelic genotypes
#' cnt.ok <- baf.count.filter(cnt, lowread=0, mincov=10, minall=2, maxall=2, deltafreq=NA)
#' geno <- baf.count.genotype(cnt.ok, lowread=0, sorted=TRUE)
#' baf.close(baf)
#
baf.count.genotype <- function(count, lowread=2L, what=c("symbol", "index", "string"),
                               sorted=FALSE, use.threads=lx.use.threads()) {

  what <- match.arg(what)
  
  .ord <- function(x) order(x, decreasing=T)
  .nam <- colnames(count)
  if (sorted) {
    .sel <- switch(what,
                   index=function(r) { o <- .ord(r); list(o[which(r[o] > lowread)]) },
                   function(r) { o <- .ord(r); list(.nam[o[which(r[o] > lowread)]]) })
  } else {
    .sel <- switch(what,
                   index=function(r) list(which(r > lowread)),
                   function(r) list(.nam[which(r > lowread)]))
  }

  geno <- if (use.threads) 
    lx.lapply(seq_len(nrow(count)), function(i) .sel(count[i,]), use.threads=T)
  else
    apply(count, 1, .sel)

  geno <- unlist(geno, recursive=F)
  
  if (what %in% "string")
    geno <- sapply(geno, paste, collapse="")
  
  geno
}


# -------------------------------------------------
#' get heterozygous positions within region 
#' @description
#' get heterozygous positions within region using absolute coordinates.\cr
#' \code{coord} defines a region on a chromosome. this function
#' gather all heterozygous positions defined as valid by \link{baf.count.filter}:\cr
#' \itemize{
#' \item coverage >= \code{covmin}
#' \item nb_alleles == 2
#' \item abs(0.5 - count_max_allele / coverage) <= \code{deltafreq}
#' }
#' @param handle file handle (as returned by \link{baf.open})
#' @param coord absolute sequence coordinates (c(absfrom, absto)) (1-based)
#'               or a single position absfrom (this implies absto=absfrom)
#' @param lowread low read threshold (see \link{baf.count.filter})
#' @param mincov minimal coverage (see \link{baf.count.filter})
#' @param deltafreq max allele frequency (see \link{baf.count.filter})
#' @return integer matrix of size n x 4 containing allele counts at heterozygous sites.
#' with (1-based) positions as rownames.
#' @note take care that if mincov < 0 the actual mincov will be computed on this
#' region (as \code{quantile(coverage, -mincov/1000)}),
#' not on whole chromosome nor genome. you better use a positive value for
#' mincov.
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @seealso \link{baf.heterozygous.cloc}, \link{baf.count.filter}
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' baf.heterozygous.coord(baf, c(33725, 33732), deltafreq=0.5)
#' baf.close(baf)
#'
baf.heterozygous.coord <- function(handle, coord, lowread=2L, mincov=10L, deltafreq=0.1) {

  # fetch points
  count <- baf.fetch.coord(handle, coord)

  # get heterozygous positions
  indx <- baf.count.filter(count, lowread=lowread, mincov=mincov, 
                           minall=2, maxall=2, deltafreq=deltafreq,
                           what="index")
  
  count <- count[indx,,drop=F]
  rownames(count) <- seq(from=coord[1], to=coord[2])[indx]
  
  count
}

# -------------------------------------------------
#' get heterozygous positions within region 
#' @description
#' get heterozygous positions within region using relative coordinates.\cr
#' \code{clocation} defines a region on a chromosome. this function
#' gather all heterozygous positions defined as valid by \link{baf.count.filter}:\cr
#' \itemize{
#' \item coverage >= \code{covmin}
#' \item nb_alleles == 2
#' \item abs(0.5 - count_max_allele / coverage) <= \code{deltafreq}
#' }
#' @param handle file handle (as returned by \link{baf.open})
#' @param clocation relative clocation = c(seqname, from, to) (1-based)
#' @param lowread low read threshold (see \link{baf.count.filter})
#' @param mincov minimal coverage (see \link{baf.count.filter})
#' @param deltafreq max allele frequency (see \link{baf.count.filter})
#' @return integer matrix of size n x 4 containing allele counts at heterozygous sites.
#' with (1-based) positions as rownames.
#' @note take care that if mincov < 0 the actual mincov will be computed on this
#' region (as \code{quantile(coverage, -mincov/1000)})
#' not on whole chromosome nor genome. you better use a positive value for
#' mincov.
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @seealso \link{baf.heterozygous.coord}, \link{baf.count.filter}
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' i <- baf.name2index(baf, "machaon")
#' baf.heterozygous.cloc(baf, c(i, 560, 567), deltafreq=0.5)
#' baf.close(baf)
#'
baf.heterozygous.cloc <- function(handle, clocation, lowread=2L, mincov=10L, deltafreq=0.1) {
  coord <- cloc2coord(handle, clocation)
  res <- baf.heterozygous.coord(handle, coord, lowread=lowread,
                                mincov=mincov, deltafreq=deltafreq)
  rownames(res) <- coords2clocs(handle, as.numeric(rownames(res)))[,2]
  res
}

# -------------------------------------------------
#' get heterozygous positions on chromosome
#' @description
#' get heterozygous positions on chromosome \code{chr}.
#' this function gather all heterozygous positions defined 
#' as valid by \link{baf.count.filter}:\cr
#' \itemize{
#' \item coverage >= \code{covmin}
#' \item nb_alleles == 2
#' \item abs(0.5 - count_max_allele / coverage) <= \code{deltafreq}
#' }
#' @param handle file handle (as returned by \link{baf.open})
#' @param chr chromosome index (if integer) or chromosome name (if character)
#' @param lowread low read threshold (see \link{baf.count.filter})
#' @param mincov minimal coverage (see \link{baf.count.filter})
#' @param deltafreq max allele frequency (see \link{baf.count.filter})
#' @param .chunk.size chunk size for loading chromosome (to save memory). Use NA
#' to load in one single chunk (quicker but memory expensive). see note below for the
#' compatibility with a negative mincov.
#' @param .sample.size sample size to determine mincov for the case where .chunk.size != NA
#' and mincov < 0 (see note).
#' @return integer matrix of size n x 4 containing allele counts at heterozygous sites.
#' with (1-based) positions as rownames.
#' @note if (mincov < 0) then mincov will be estimated 
#' (\code{quantile(coverage, -mincov/1000)}) as in \link{baf.count.filter}.
#' However the region for computing coverage will depends upon the \code{.chunk.size}
#' parameter: if \code{.chunk.size == NA} then this will be performed on the
#' whole chromosome.
#' if \code{.chunk.size != NA} then mincov will be first evaluated
#' on a sample of \code{.sample.size} data points. Therefore for exact results, you
#' should better use a positive value for mincov.
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @seealso \link{baf.heterozygous.coord}, \link{baf.heterozygous.cloc}
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' baf.heterozygous.chr(baf, 3, deltafreq=0.5)
#' baf.close(baf)
#'
baf.heterozygous.chr <- function(handle, chr, lowread=2L, mincov=10L, deltafreq=0.1, 
                                 .chunk.size=NA, .sample.size=1e6L) {
  
  chrindex <- if (is.character(chr)) baf.name2index(handle, chr) else chr
  
  chr.size <- handle$header$seq[[chrindex]]$size
  
  .chunk.size <- if (is.na(.chunk.size)) chr.size else .chunk.size
  
  chunk.from <- seq.int(1L, chr.size, by=.chunk.size)
  
  # estimate mincov if needed
  if ((.chunk.size != chr.size) && (mincov < 0)) {
    lx.warn("chunk size and mincov < 0 are incompatible. need to sample data points")
    samp <- baf.sample(handle, chrs=chr, sample.size=.sample.size, .seed=-1, use.threads=F)
    mincov <- .mincov(samp, mincov)
  }
  
  # go ahead
  res <- lapply(chunk.from, function(from) {
      cloc <- c(chrindex, from, min(from + .chunk.size - 1, chr.size))
      baf.heterozygous.cloc(handle, cloc, lowread=lowread,
                            mincov=mincov, deltafreq=deltafreq)
  })
  
  do.call(rbind, res)
}

# -------------------------------------------------
#' get heterozygous positions on all chromosomes
#' @description
#' get heterozygous positions on all chromosomes
#' this function gather all heterozygous positions
#' defined as valid by \link{baf.count.filter}:\cr
#' \itemize{
#' \item coverage >= \code{covmin}
#' \item nb_alleles == 2
#' \item abs(0.5 - count_max_allele / coverage) <= \code{deltafreq}
#' }
#' @param handle file handle (as returned by \link{baf.open})
#' @param chrs integer (chromosome indexes) or character (chromosome names) vector
#' specifying which chromosomes to use. Use NULL to specify all
#' chromosome declared in baf header.
#' @param lowread low read threshold (see \link{baf.count.filter})
#' @param mincov minimal coverage (see \link{baf.count.filter})
#' @param deltafreq max allele frequency (see \link{baf.count.filter})
#' @param flatten if TRUE flatten all chromosomes within a single matrix
#' else return a list of such matrices, one per chromosome
#' @param use.threads (see \link{lx.use.threads})
#' @param .chunk.size chunk size for loading chromosomes. see \link{baf.heterozygous.chr}
#' @param .sample.size sample size to determine mincov for the case where mincov < 0 
#' (see note below).
#' @return (list of) integer matrix of size n x 4 containing allele counts at heterozygous sites.
#' if flatten is TRUE matrix rownames are of the form: chr.position (1-based)
#' else position (1 based) only.
#' @note if (mincov < 0) then mincov will be estimated 
#' (as \code{quantile(coverage, -mincov/1000)}) as in \link{baf.count.filter}.
#' However the region for computing coverage will depends upon the \code{.chunk.size}
#' parameter: if \code{.chunk.size == NA} then this will be performed on each chromosome
#' separately (therefore leading to potential different values of mincov per chromosome).
#' if \code{.chunk.size != NA} then mincov will be first evaluated
#' on a sample of \code{.sample.size} data points. Therefore for exact results, you
#' better use a positive value for mincov.
#' @note
#' see \link{HELP.COORD} for help on coordinates systems
#' @seealso \link{baf.heterozygous.chr}
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' baf.heterozygous(baf, NULL, deltafreq=0.5)
#' baf.heterozygous(baf, NULL, deltafreq=0.5, flatten=FALSE)
#' baf.close(baf)
#'
baf.heterozygous <- function(handle, chrs=NULL, lowread=2L, mincov=10L, deltafreq=0.1,
                             flatten=TRUE, .chunk.size=NA, .sample.size=1e6L,
                             use.threads=lx.use.threads()) {

  if (is.null(chrs))
    chrs <- seq.int(handle$header$nbseq)
  
  if (is.character(chrs))
    chrs <- sapply(chrs, function(chr) baf.name2index(handle, chr))
  
  # truncate chrs if needed
  chrs <- intersect(chrs, seq.int(handle$header$nbseq))

  # estimate mincov if needed
  if ((! is.na(.chunk.size)) && (mincov < 0)) {
    lx.warn("chunk size and mincov < 0 are incompatible. need to sample data points")
    samp <- baf.sample(handle, chrs=chrs, sample.size=.sample.size, .seed=-1L, 
                       use.threads=use.threads)
    mincov <- .mincov(samp, mincov)
  }

  # collect by chromosome
  res <- lx.happly(chrs, function(chrindex, handle) {
    lx.out("collecting baf on chr ", chrindex, level="debug")
    baf.heterozygous.chr(handle, chrindex, lowread=lowread,
                         mincov=mincov, deltafreq=deltafreq,
                         .chunk.size=.chunk.size, .sample.size=.sample.size)
  }, handle=handle, use.threads=use.threads)
  
  # flatten and add rownames
  if (flatten) {
    chr.nams <- unlist(mapply(rep, chrs, sapply(res, nrow), USE.NAMES=F))
    res <- do.call(rbind, res)
    rownames(res) <- paste(chr.nams, rownames(res), sep=".")
  } else {
    names(res) <- chrs
  }

  res
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
#' @details
#' this is the same function as \link{basta2clocs}
#' 
baf2clocs <- basta2clocs

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
#' @details
#' this is the same function as \link{basta2coords}
#' 
baf2coords <- basta2coords

# -------------------------------------------------
#' allelic frequency regularization
#' @description
#' regularize allelic frequency using poisson pseudo-counts
#' @param count nx4 integer matrix of alleles counts.
#' as returned by \code{baf.fetch.xxx}, \code{baf.sample.xxx} or
#' \code{baf.heterozygous.xxx}.
#' @param tot total count (i.e. depth)
#' @param n number of draws per point
#' @param .eps lambda correction
#' @param .seed seed for random (do not seed if < 0)
#' @return names list of 3 vectors (cnt=regularized counts,
#' tot=regularized total, raf=regulized all. freq.)
#' @examples
#' N <- 10000
#' cnt <- round(rpois(N, 5))
#' tot <- cnt + round(rpois(N, 5))
#' unreg <- cnt / tot
#' reg <- baf.count.regularize(cnt, tot, "poisson", .seed=0)
#' 
baf.count.regularize.poisson <- function(count, tot, n=10L, .eps=0, .seed=-1L) {
  if (.seed >= 0) set.seed(.seed)
  zero  <- rep.int(0, max(length(count), length(tot)))
  count <- count + zero # force recycling
  tot   <- tot + zero
  rac   <- pmax(0, sapply(count, function(x) mean(rpois(n, lambda=x+.eps))))
  rrc   <- pmax(0, sapply(tot-count, function(x) mean(rpois(n, lambda=x+.eps))))
  rac[(count > tot) | (count < 0)] <- NA
  raf <- pmin(1.0, rac / (rac + rrc))
  list(cnt=rac, tot=rac+rrc, raf=raf)
}

# -------------------------------------------------
#' allelic frequency regularization
#' @description
#' regularize allelic frequency using gaussian pseudo-counts
#' @param count nx4 integer matrix of alleles counts.
#' as returned by \code{baf.fetch.xxx}, \code{baf.sample.xxx} or
#' @param tot total count (i.e. depth)
#' @param sd gaussian standard deviation
#' @param n number of draws per point
#' @param .seed seed for random (do not seed if < 0)
#' @return names list of 3 vectors (cnt=regularized counts,
#' tot=regularized total, raf=regulized all. freq.)
#' @examples
#' N <- 10000
#' cnt <- round(rpois(N, 5))
#' tot <- cnt + round(rpois(N, 5))
#' unreg <- cnt / tot
#' reg <- baf.count.regularize(cnt, tot, "gaussian", .seed=0)
#'
baf.count.regularize.gaussian <- function(count, tot, sd=0.5, n=10L, .seed=-1L) {
  if (.seed >= 0) set.seed(.seed)
  npt   <- max(length(count), length(tot))
  zero  <- rep.int(0, npt)
  count <- count + zero # force recycling
  tot   <- tot + zero
  rac   <- pmax(0, sapply(count, function(x) x + mean(rnorm(n, 0, sd=sd))))
  rrc   <- pmax(0, sapply(tot-count, function(x) x + mean(rnorm(n, 0, sd=sd))))
  rac[(count > tot) | (count < 0)] <- NA
  raf <- pmin(1.0, rac / (rac + rrc))
  list(cnt=rac, tot=rac+rrc, raf=raf)
}

# -------------------------------------------------
#' allelic frequency regularization
#' @description
#' regularize allelic frequency using various methods
#' @param count nx4 integer matrix of alleles counts.
#' as returned by \code{baf.fetch.xxx}, \code{baf.sample.xxx} or
#' @param tot total count (i.e. depth)
#' @param method method to use ("poisson" or "gaussian")
#' @param ... specific method parameters 
#' @return names list of 3 vectors (cnt=regularized counts,
#' tot=regularized total, raf=regularized all. freq.)
#' @examples
#' N <- 10000
#' cnt <- round(rpois(N, 30))
#' tot <- cnt + round(rpois(N, 30))
#' unreg <- cnt / tot
#' reg <- baf.count.regularize(cnt, tot, "gaussian", .seed=0)
#'
baf.count.regularize <- function(count, tot, method=c("poisson", "gaussian"), ...) {
  method <- match.arg(method)
  do.call(paste0("baf.count.regularize.", method), c(list(count=count, tot=tot), ...))
}
