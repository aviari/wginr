# -------------------------------------------------
# $Id: asdog.segment.plot.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# Segment plotting
#

# -------------------------------------------------
# local segments
# draw segments with lend=1 ensuring everything is visible
# small segments are represented with points
#
.segments <- function(x0, y0, x1=x0, y1=y0, lend=1, pch=15, cex=0.2, 
                      res=1000, tol=1, ...) {
  segments(x0, y0, x1, y1, lend=lend, ...)
  if (lend == 1) {
    # select small segments only
    sel <- lx.scale(x1-x0, c(1,res), par("usr")[1:2], zerob=T) < tol
    if (any(sel))
      points(x0[sel], (y0[sel]+y1[sel])/2, pch=pch, cex=cex, ...)
  }
  invisible()
}

# -------------------------------------------------
# local plot : cn or af profile
#
.plot.profile <- function(x, y, fun=plot, chrs=NULL, decimate=TRUE,
                          ylim=NULL,
                          alpha=ifelse(decimate, 1, 0.8),
                          cex=ifelse(decimate, 0.1, 0.05),
                          pch=16,
                          xlab="position", ylab="Y", 
                          .samp=5e4, ...) {
  
  if (missing(y)) {
    y <- x
    x <- seq_along(y)
  }

  if (is.null(ylim)) {
    ylim <- quantile(y, c(0.01, 0.99), na.rm=T)
  }
  
  below <- y<=ylim[1]
  above <- y>=ylim[2]
  y[below] <- ylim[1]
  y[above] <- ylim[2]
  
  pal <- lx.color.alpha(c(6, 1), alpha)
  col <- pal[ifelse(above|below, 1, 2)]

  if (decimate) {
    lx.plot(x, y, fun=fun, ylim=ylim, cex=cex, pch=pch,
            xlab=xlab, ylab=ylab, xaxs='i',
            col=col, .samp=.samp, ...)
  } else {
    fun(x, y, ylim=ylim, cex=cex, pch=pch,
        xlab=xlab, ylab=ylab, xaxs='i',
        col=col, ...)
  }

  if (! is.null(chrs)) {
    rle <- rle(chrs)
    gx <- x[cumsum(rle$lengths)]
    abline(v=gx, col=4)
    text(gx, ylim[2], labels=rle$values, cex=0.5, pos=2, off=0.1, col=4)
  }
  
  invisible()
}

# -------------------------------------------------
# zscor colors
#
.zscor.colors <- function(zscor, n=10, alpha=0.5) {
  asdog.gradient.colors(abs(zscor), xrange=c(0, n),
                        nlevels=n+1, alpha=alpha, rev=T) 
}

# -------------------------------------------------
# plot profiles rcaf
#
.plot.profile.rcaf <- function(segment, chrs=names(segment$rcaf.data),
                               ylim=NULL, main=NULL,
                               decimate=length(chrs)>1, ...) {
  
  chrs <- as.character(chrs)
  single <- length(chrs) <= 1
  
  pltheo <- segment$rcaf.theo
  hmm  <- segment$rcaf.hmm

  rcaf <- segment$rcaf.data[chrs]
  rcaf <- do.call(rbind, rcaf)
  
  if (single) {
    pos <- rcaf$pos
  } else {
    clocs <- clocations(cbind(rcaf$chr, rcaf$pos, rcaf$pos))
    pos <- clocs2coords(segment, clocs)[,1]
  }
  
  segs <- segment$rcaf.segs[as.character(segment$rcaf.segs$chr) %in% chrs,]
  
  if (single) {
    segs$cfrom <- segs$from
    segs$cto <- segs$to
  } else {
    clocs <- clocations(cbind(segs$chr, segs$from, segs$to))
    coords <- clocs2coords(segment, clocs)
    segs$cfrom <- coords[,1]
    segs$cto <- coords[,2]
  }
  
  if (is.null(ylim)) {
    cnmax <- segment$params$segment.plot.cnmax
    if (cnmax == 0) {
      rcmax <- segment$params$segment.plot.rcmax
    } else {
      rcmax <- asdog.relRC(segment$params$segment.plot.cnmax,
                           pltheo$alpha, pltheo$q0)
    }
    if (rcmax == 0) rcmax <- max(rcaf$rrc) # @fixme: [AV] use quantile instead
    ylim <- c(0, rcmax)
  }
  
  opar <- par(no.readonly=T)
  par(mfrow=c(2, 1),
      mgp=c(2,1,0),
      mar=c(0, 4, 0, 2) + 0.1,
      oma=c(4, 0, 2, 0) + 0.1)
  
  # ---------------------------------
  # RRC plot
  # ---------------------------------
  
  # empty plot to setup scale
  #
  .plot.profile(pos, rcaf$rrc, chrs=NULL, decimate=decimate,
                ylim=ylim, ylab="rrc", alpha=0, xaxt='n', ...)
  
  # color bands -> zscore
  #
  segments(segs$cfrom, 0, segs$cto, 0,
           col=.zscor.colors(segs$zscor.rrc),
           lwd=5000, lend=1)
  
  # replot points over bands
  #
  .plot.profile(pos, rcaf$rrc, fun=points, chrs=rcaf$chr,
                decimate=decimate, ylim=ylim, xaxt='n', ...)

  # CN scale
  #
  abline(h=unique(Re(thmm.parameters(hmm, "mean"))), lty=2, lwd=0.5, col=lx.GREY)
  axis(4, unique(pltheo$rc), unique(pltheo$ploidy), las=2, cex.axis=0.5)
  
  # reference CN line
  #
  ref.rc <- asdog.relRC(segment$rcaf.refcn, pltheo$alpha, pltheo$q0)
  abline(h=ref.rc, lty=2, lwd=0.5, col=lx.BLUE)
  points(par("usr")[2], ref.rc, pch=23, cex=1, col=lx.BLUE, bg=lx.BLUE, xpd=NA)
  
  # plot segments
  #   yellow -> observed
  #   red    -> model 
  #
  .segments(segs$cfrom, segs$med.rrc, segs$cto, segs$med.rrc,
            col=lx.color.alpha(7, 0.5), lwd=5, lend=1)
  .segments(segs$cfrom, segs$lev.rrc, segs$cto, segs$lev.rrc,
            col=2, lwd=2, lend=1)
  
  # title
  #
  if (! is.null(main)) {
    title(main, line=0.5, xpd=NA)
  }

  # ---------------------------------
  # BAF plot
  # ---------------------------------
  
  # empty plot to setup scale
  #
  .plot.profile(pos, rcaf$baf, chrs=NULL, decimate=decimate,
                ylim=c(0,1), ylab="af", alpha=0, ...)
  
  # color bands -> zscore
  #
  segments(segs$cfrom, 0.5, segs$cto, 0.5,
           col=.zscor.colors(segs$zscor.baf),
           lwd=5000, lend=1)
  
  # replot points over bands
  #
  .plot.profile(pos, rcaf$baf, fun=points, chrs=rcaf$chr,
                decimate=decimate, ylim=c(0,1), ...)
  abline(h=0.5, col=lx.BLUE, lty=2, lwd=0.5)
  
  # plot segments
  #   yellow -> observed
  #   red    -> model 
  #
  .segments(segs$cfrom, segs$med.baf, segs$cto, segs$med.baf,
            col=lx.color.alpha(7, 0.5), lwd=5, lend=1)
  .segments(segs$cfrom, 1-segs$med.baf, segs$cto, 1-segs$med.baf,
            col=lx.color.alpha(7, 0.5), lwd=5, lend=1)
  .segments(segs$cfrom, segs$lev.baf, segs$cto, segs$lev.baf,
            col=2, lwd=2, lend=1)
  .segments(segs$cfrom, 1-segs$lev.baf, segs$cto, 1-segs$lev.baf,
            col=2, lwd=2, lend=1)
  
  par(opar)
  invisible()  
}

# -------------------------------------------------
# plot profile  rrc
#
.plot.profile.rrc <- function(segment, chrs=names(segment$rrc.data),
                              ylim=NULL, main=NULL,
                              decimate=length(chrs)>1, ...) {
  chrs <- as.character(chrs)
  single <- length(chrs) <= 1
  
  pltheo <- segment$rrc.theo
  hmm  <- segment$rrc.hmm
  
  rrc <- segment$rrc.data[chrs]
  rrc <- do.call(rbind, rrc)
  
  if (single) {
    pos <- rrc$pos
  } else {
    clocs <- clocations(cbind(rrc$chr, rrc$pos, rrc$pos))
    pos <- clocs2coords(segment, clocs)[,1]
  }
  
  segs <- segment$rrc.segs[as.character(segment$rrc.segs$chr) %in% chrs,]
  
  if (single) {
    segs$cfrom <- segs$from
    segs$cto <- segs$to
  } else {
    clocs <- clocations(cbind(segs$chr, segs$from, segs$to))
    coords <- clocs2coords(segment, clocs)
    segs$cfrom <- coords[,1]
    segs$cto <- coords[,2]
  }
  
  if (is.null(ylim)) {
    cnmax <- segment$params$segment.plot.cnmax
    if (cnmax == 0) {
      rcmax <- segment$params$segment.plot.rcmax
    } else {
      rcmax <- asdog.relRC(segment$params$segment.plot.cnmax,
                           pltheo$alpha, pltheo$q0)
    }
    if (rcmax == 0) rcmax <- max(rrc$rrc) # @fixme: [AV] use quantile instead
    ylim <- c(0, rcmax)
  }
  
  # empty plot to setup scale
  #
  .plot.profile(pos, rrc$rrc, chrs=NULL, decimate=decimate,
                ylim=ylim, ylab="rrc", alpha=0, ...)
  
  # color bands -> zscore
  #
  segments(segs$cfrom, 0, segs$cto, 0,
           col=.zscor.colors(segs$zscor.rrc),
           lwd=5000, lend=1)
  
  # replot points over bands
  #
  .plot.profile(pos, rrc$rrc, fun=points, chrs=as.character(rrc$chr), 
                decimate=decimate, ylim=ylim, ...)

  # CN scale
  #
  abline(h=thmm.parameters(hmm, "mean"), lty=2, lwd=0.5, col=lx.GREY)
  axis(4, unique(pltheo$rc), unique(pltheo$ploidy), las=2, cex.axis=0.5)

  # reference CN line
  #
  ref.rc <- asdog.relRC(segment$rrc.refcn, pltheo$alpha, pltheo$q0)
  abline(h=ref.rc, lty=2, lwd=0.5, col=lx.BLUE)
  points(par("usr")[2], ref.rc, pch=23, cex=1, col=lx.BLUE, bg=lx.BLUE, xpd=NA)
  
  # plot segments
  #   yellow -> observed
  #   red    -> model 
  #
  .segments(segs$cfrom, segs$med.rrc, segs$cto, segs$med.rrc,
           col=lx.color.alpha(7, 0.5), lwd=5, lend=1)
  .segments(segs$cfrom, segs$lev.rrc, segs$cto, segs$lev.rrc,
            col=2, lwd=2, lend=1)

  # title
  #
  if (! is.null(main)) {
    title(main, xpd=NA)
  }
  
  invisible()
}

# -------------------------------------------------
# S3 method to plot Segment
#
plot.Segment <- function(segment, chrs=names(segment$rrc.data), 
                           what=c("rcaf", "rrc"), decimate=length(chrs)>1, 
                         ...) {
  
  what <- match.arg(what)
  
  fun <- get(paste0(".plot.profile.", what), mode="function")
  
  fun(segment, chrs=chrs, decimate=decimate, ...)
}

