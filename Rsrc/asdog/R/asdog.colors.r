# -------------------------------------------------
# $Id: asdog.colors.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# color utilities
#

# -------------------------------------------------
# default palette function for gradient
#
asdog.gradient.palette <- viridisLite::viridis

# -------------------------------------------------
# compute colors associated to gradient
#
asdog.gradient.colors <- function(x, xrange=range(x), nlevels=10, alpha=0.5,
                                  palette=asdog.gradient.palette, rev=FALSE) {
  x <- pmax(xrange[1], pmin(xrange[2], x))
  pal <- palette(nlevels, alpha=alpha)
  if (rev) pal <- rev(pal)
  tail(pal[cut(c(xrange, x), breaks=nlevels, labels=F)], -2)
}

# -------------------------------------------------
# Default UCSC color scheme for chromosome colors
#
asdog.chromos.palette <- function(alpha=1) {
  .rgb <- function(r, g, b) rgb(r, g, b, alpha=255*alpha, maxColorValue=255)
  x    <- list()
  x$chr1  <- .rgb(153,102,0)
  x$chr2  <- .rgb(102,102,0)
  x$chr3  <- .rgb(153,153,30)
  x$chr4  <- .rgb(204,0,0)
  x$chr5  <- .rgb(255,0,0)
  x$chr6  <- .rgb(255,0,204)
  x$chr7  <- .rgb(255,204,204)
  x$chr8  <- .rgb(255,153,0)
  x$chr9  <- .rgb(255,204,0)
  x$chr10 <- .rgb(255,255,0)
  x$chr11 <- .rgb(204,255,0)
  x$chr12 <- .rgb(0,255,0)
  x$chr13 <- .rgb(53,128,0)
  x$chr14 <- .rgb(0,0,204)
  x$chr15 <- .rgb(102,153,255)
  x$chr16 <- .rgb(153,204,255)
  x$chr17 <- .rgb(0,255,255)
  x$chr18 <- .rgb(204,255,255)
  x$chr19 <- .rgb(153,0,204)
  x$chr20 <- .rgb(204,51,255)
  x$chr21 <- .rgb(204,153,255)
  x$chr22 <- .rgb(102,102,102)
  x$chrX  <- .rgb(153,153,153)
  x$chrY  <- .rgb(204,204,204)
  x$chr23 <- x$chrX
  x$chr24 <- x$chrY
  unlist(x, use.names=T)
}

# -------------------------------------------------
# compute colors associated to chromosome name
#
asdog.chromos.colors <- function(chrs, alpha=1) {
  pal <- asdog.chromos.palette(alpha=alpha)
  chrs <- sapply(as.character(chrs), function(chr) 
                      if (grepl("^chr", chr)) chr else paste0("chr", chr))
  pal[chrs]
}


