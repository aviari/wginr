# -------------------------------------------------
# $Id: asdog.bcp.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# Bayesian Change Point segmentation
#

# -------------------------------------------------
# <internal> make complex number
#
.c <- function(x,y=x) complex(real=x, imaginary=y)

# -------------------------------------------------
# BCP segment rRC and AF
#
# ... additionnal arguments to bcp
#
asdog.bcp.segment <- function(rrc, baf, rcmax=4,
                              smooth.rc=NULL,
                              smooth.af=NULL,
                              w0=NULL, p0=1e-10,
                              sample.size=100000L,
                              minseg.size=150L,
                              ...) {
  
  # threshold - should have been done before - this is just a patch in case of
  rrc <- pmax(pmin(rrc, rcmax), 0)
  rrc[is.na(rrc)] <- median(rrc, na.rm=T)
  
  baf <- pmax(pmin(baf, 1), 0)
  baf[is.na(baf)] <- 0.5
  
  # recenter baf on 0 and fold signal
  fld <- abs(baf - 0.5)
  
  # smooth if requested
  wrc <- if (is.null(smooth.rc)) rrc else lx.smooth.median(rrc, smooth.rc)
  wfd <- if (is.null(smooth.af)) fld else lx.smooth.median(fld, smooth.af)

  # decimate for speedup
  if (is.na(sample.size)) {
    pos <- seq_along(wrc)
  } else {
    pos <- lx.decimate(wrc, sample.size, as.index=T)
    wrc  <- wrc[pos]
    wfd  <- wfd[pos]
  }
  
  # bcp crashes when less than 4 data points
  if (length(wrc) <= 5) {
    lx.warn("not enough points for BCP")
    return(seg=NULL, res=NULL)
  }
  
  # make sure minseg.size is not too high
  minseg.size <- max(2, min(minseg.size, length(wrc)))
  
  # segment using BCP
  
  # run BCP
  lx.out("running bcp with p0=", p0, " w0=", w0)
  bcp <- bcp(cbind(wrc, wfd), w0=w0, p0=p0, ...)
  means <- .c(bcp$posterior.mean[,1], bcp$posterior.mean[,2])
  rle <- rle(means)

  # make segments
  #
  seg <- data.frame(ifrom=head(cumsum(c(1, rle$lengths)), -1))
  seg$ito <- seg$ifrom + rle$lengths - 1
  
  seg$pfrom <- pos[seg$ifrom]
  seg$pto   <- pos[seg$ito]

  seg$weight <- seg$ito - seg$ifrom + 1
  
  if (any(seg$weight >= minseg.size))
    seg <- seg[seg$weight >= minseg.size,,drop=F]
  
  lx.out("kept ", nrow(seg), " segments")

  seg$mean.rc <- apply(seg, 1, function(r) median(rrc[r[3]:r[4]]))
  seg$sd.rc   <- apply(seg, 1, function(r)    mad(rrc[r[3]:r[4]]))

  # unfold signal on segments
  #
  lx.out("unfolding segments")
  mfl <- apply(seg, 1, function(r) median(fld[r[3]:r[4]]))
  sfl <- apply(seg, 1, function(r)    mad(fld[r[3]:r[4]]))

  x <- t(mapply(function(m, s) asdog.unfold(m, s, type="median")$solution,
                mfl, sfl, USE.NAMES=F))
  
  seg$mean.af <- unlist(x[,1]) + 0.5
  seg$sd.af   <- unlist(x[,2])
  
  lx.out("found ", nrow(seg), " segments", with.mem=T)
  
  list(seg=seg, res=list(bcp=bcp, pos=pos, obs.rc=wrc, obs.af=wfd))
}

