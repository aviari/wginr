# -------------------------------------------------
# $Id: asdog.gcmodel.datasrc.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# GC and Cover data source:
#   load and store GC and Cover in memory for quick access
#

# =================================================
# internals <no_export>
# =================================================

# -------------------------------------------------
# <internal> <no_export>
# check if smooth.k is not 0
#
.has.smooth <- function(k) ! (is.null(k) || all(k==0))

# -------------------------------------------------
# <internal> <no_export>
# chr name to index
#
.chr.index <- function(handle, chr) 
  if (is.character(chr)) basta.name2index(handle, chr) else chr

# -------------------------------------------------
# <internal> <no_export>
# load gc cumsum in chr
#
# .chunk to prevent memory overload
#
.load.gc.chr <- function(handle, chr, grain=100L, smooth.k=c(3L,5L,15L),
                         .chunk=1e6) {
  chr <- .chr.index(handle, chr)
  chr.siz <- handle$header$seq[[chr]]$size
  .chunk <- grain * ceiling(.chunk/grain)
  gc <- unlist(lapply(seq.int(1, chr.siz, .chunk), function(from) {
    to  <- min(chr.siz, from+.chunk-1)
    seq <- basta.fetch.cloc(handle, c(chr, from, to))
    gc  <- logical(to-from+1)
    gc[lx.strchr(seq, "GC")] <- TRUE
    lx.binsum(gc, k=grain, drop=F)/grain
  }), use.names=F)
  if (.has.smooth(smooth.k))
    gc <- lx.smooth.median(gc, k=smooth.k)
  gc <- c(0, cumsum(gc))
  gc
}

# -------------------------------------------------
# <internal> <no_export>
# load coverage cumsum in chr
#
# .chunk to prevent memory overload
#
.load.cov.chr <- function(handle, chr, grain=100L, smooth.k=c(3L,5L,15L),
                          .chunk=1e5) {
  chr <- .chr.index(handle, chr)
  chr.siz <- handle$header$seq[[chr]]$size
  .chunk <- grain * ceiling(.chunk/grain)
  cov <- unlist(lapply(seq.int(1, chr.siz, .chunk), function(from) {
    to  <- min(chr.siz, from+.chunk-1)
    cov <- rowSums(baf.fetch.cloc(handle, c(chr, from, to)))
    lx.binsum(cov, k=grain, drop=F)/grain
  }), use.names=F)
  if (.has.smooth(smooth.k))
    cov <- lx.smooth.median(cov, k=smooth.k)
  cov <- c(0, cumsum(cov))
  cov
}

# -------------------------------------------------
# <internal> <no_export>
# load gc or cov cumsum
#
.load.data <- function(handle, fun, chrs=names(handle$header$seq),
                     grain=100L, smooth.k=c(3L,5L,15L),
                     use.threads=lx.use.threads()) {
  res <- lx.happly(chrs, function(chr, handle) 
          fun(handle, chr, grain=grain, smooth.k=smooth.k),
          handle=handle, use.threads=use.threads)
  names(res) <- chrs
  res
}

# -------------------------------------------------
# <internal> <no_export>
# collect %gc or cov at coords (or (point) clocation)
# return mean %GC or mean cov in intervals 
#     [pos-winsize/2, pos+winsize/2]
#
# what = "gc" or "cov"
#
.collect.pts <- function(datasrc, what, locs, winsize=1000L,
                         use.threads=lx.use.threads()) {
  
  clocs <- if (is.null(dim(locs))) coords2clocs(handle, locs) else locs

  lx.out("collect ", what, " at ", nrow(clocs), " locations",
         level="debug", with.mem=T)
  
  halfsize <- round(winsize/2)
  
  apply.clocs(clocs, function(clocs, ...) {
    chr  <- clocs[1,1]
    N    <- length(datasrc[[what]][[chr]]) - 1
    from <- pmax(1, round((clocs[,2] - halfsize)/datasrc$grain))
    to   <- pmin(N, round((clocs[,3] + halfsize)/datasrc$grain))
    (datasrc[[what]][[chr]][to+1] - datasrc[[what]][[chr]][from]) / (to-from+1)
  }, handle=NULL, flatten=TRUE, use.threads=use.threads)
  
}

# =================================================
# API
# =================================================

# -------------------------------------------------
# load rcgc datasource
#
asdog.rcgc.datasrc <- function(handle.basta, handle.baf, 
                               chrs=names(handle$header$seq),
                               grain=100L, smooth.k=c(3L,5L,15L),
                               use.threads=lx.use.threads()) {
  
  handle <- if (is.null(handle.basta)) handle.baf else handle.basta                                                                
  
  datasrc <- list()
  datasrc$header   <- handle$header
  datasrc$chrs     <- chrs
  datasrc$grain    <- grain
  datasrc$smooth.k <- smooth.k
  
  if (! is.null(handle.basta)) {
    lx.out("loading GC datasource")
    datasrc$gc <- .load.data(handle.basta, .load.gc.chr,
                             chrs=chrs, grain=grain, smooth.k=smooth.k,
                             use.threads=use.threads)
  }
                             
  if (! is.null(handle.baf)) {
    lx.out("loading Coverage datasource")
    datasrc$cov <- .load.data(handle.baf, .load.cov.chr,
                              chrs=chrs, grain=grain, smooth.k=smooth.k,
                              use.threads=use.threads)
  }

  datasrc  
}


# -------------------------------------------------
# collect %gc and cover at coords or clocations from datasrc
#
# datasrc: data source from asdog.rcgc.datasrc
# clocs: either point coords (vector) or clocations
#
asdog.rcgc.collect <- function(datasrc, locs,
                               winsize.gc=1000L, winsize.cov=1000L,
                               use.threads=lx.use.threads()) {
  
  clocs  <- if (is.null(dim(locs))) coords2clocs(datasrc, locs) else locs
  coords <- if (is.null(dim(locs))) locs  else clocs2coords(datasrc, locs)[,1]
  
  pts <- list()
  pts$winsize.cov <- winsize.cov
  pts$winsize.gc  <- winsize.gc
  pts$pos <- coords
  pts$cov <- if (is.null(datasrc$cov) || (winsize.cov <= 0)) numeric(0)
             else .collect.pts(datasrc, "cov", clocs, winsize.cov,
                               use.threads=use.threads)
  
  pts$gc  <- if (is.null(datasrc$gc) || (winsize.gc <= 0)) numeric(0)
             else .collect.pts(datasrc, "gc", clocs, winsize.gc,
                               use.threads=use.threads)
  
  pts    
}
