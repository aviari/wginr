# -------------------------------------------------
# $Id: asdog.plmodel.plot.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# Ploidy model : plotting
#

# ---------------------------------
# plot data in (rc, af) space
#

asdog.plot.plmodel.rcaf <- function(obs, theo=NULL, 
                                    col=lx.color.alpha("blue", 0.2),
                                    main=NULL,
                                    ...) {

  # <internal> draw a circle
  #
  .circle <- function (x, y, radius, nv = 100, col=NA, ...) {
    xylim <- par("usr")
    plotdim <- par("pin")
    xycr <- diff(xylim )[c(1, 3)]
    ymult <- plotdim[1]/plotdim[2] * xycr[2]/xycr[1]
    angle.inc <- 2 * pi/nv
    angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)
    invisible(mapply(function(x, y, r, col) {
      polygon(cos(angles) * r + x,
              sin(angles) * r * ymult + y,
              col=col, ...)
    }, x, y, radius, col))
  }

  # body
  #
  
  # obs points as circles
  
  wrng <- range(obs$weight)
  rrng <- extendrange(obs$rc, f=0.5)
  arng <- extendrange(obs$af, f=0.5)
  plot(obs$rc, obs$af, cex=0, xlim=rrng, ylim=arng, xlab="rrc", ylab="baf")
  pusr <- par('usr')
  urng <- max(pusr[2] - pusr[1], pusr[4] - pusr[3])
  urng <- c(0, urng/10)
  tmp <- apply(obs, 1, function(r) {
    rd <- lx.scale(r[['weight']], to=urng, from=wrng) 
    .circle(r[['rc']], r[['af']], rd, col=col, ...)
  })
  
  # theo points
  if (! is.null(theo)) {
    points(theo$rc, theo$af, pch=20, col=2, cex=1)
    text(theo$rc, theo$af, theo$label, pos=4, cex=0.5, col=2)
  }
  
  title(main)
  
  invisible()
}

# ---------------------------------
# plot fit in (alpha, Q) space
#

asdog.plot.plmodel.fit <- function(fit, nlevels=10, color.palette=asdog.gradient.palette,
                                   main=NULL, ...) {
  
  aseq <- as.numeric(rownames(fit$fit))
  qseq <- as.numeric(colnames(fit$fit))
  
  if (is.null(main))
    main <- paste0("a=", fit$a0, " Q=", fit$q0, " qual=", round(fit$qual, 2))

  filled.contour(aseq, qseq, fit$fit,
                 nlevels=nlevels,
                 axes=TRUE,
                 xlab="alpha", ylab="Q",
                 color.palette=color.palette,
                 plot.axes={axis(1); axis(2); 
                            title(main, line=1, xpd=NA);
                            points(fit$a0, fit$q0, pch=3, cex=4)},
                 ...)

  invisible()
}


# -------------------------------------------------
# plot rrc and af profiles
#
# note: 'what' is unused yet
#

asdog.plot.plmodel.profile <- function(plmodel, 
                                       what=c("obs", 'expect'), 
                                       main="", ...) {
  
  .chromo <- function(chrs) {
    rle <- rle(chrs)
    csm <- cumsum(rle$lengths)
    usr <- par("usr")
    rect(csm-+rle$lengths, usr[3], csm, usr[4], 
         col=lx.color.alpha(c("blue", "white"), 0.1))
    text(csm - rle$lengths/2, usr[4]*0.95, 
         rle$values, cex=0.6, srt=90)
  }
  
  what <- match.arg(what)  # unused yet
  params <- plmodel$params
  rcaf   <- plmodel$rcaf 
  seg    <- plmodel$seg
  
  rc.max  <- params$plmodel.plot.rcmax
  if (rc.max <= 0) rc.max <- max(rcaf$rrc) # @fixme: [AV] may use quantile instead

  opar <- par(no.readonly=T)
  par(mfrow=c(2, 1),
      mgp=c(2,1,0),
      mar=c(0, 4, 0, 1) + 0.1,
      oma=c(4, 0, 2, 0) + 0.1)
  
  lx.plot(rcaf$rrc, ylim=c(0, rc.max),
          xlab="", ylab="rrc", xaxt="n", ...)
  
  abline(h=1, col=1, lwd=1)
  
  segments(seg$pfrom, seg$mean.rc,
           seg$pto, seg$mean.rc,
           lwd=3, col=2)
  
  .chromo(rcaf$chr)
  
  title(main, line=1, xpd=NA)
  
  #
  
  lx.plot(rcaf$baf, ylim=c(0, 1),
          xlab="snp", ylab="af", xpd=NA, ...)
  
  abline(h=0.5, col=1, lwd=1)
  
  segments(seg$pfrom, seg$mean.af,
           seg$pto, seg$mean.af,
           lwd=3, col=2)
  
  .chromo(rcaf$chr)
  
  par(opar)
  
  invisible()
}


# -------------------------------------------------
# S3 method to plot PLModel model
#
plot.PLModel <- function(plmodel, 
                         what=c("rcaf", "fit", "profile.obs", "profile.expect"),
                         ...) {
  
  .plot.rcaf <- function(model, ...) 
                  asdog.plot.plmodel.rcaf(model$fit$obs, model$fit$theo, ...)
  
  .plot.fit <- function(model, ...) 
                 asdog.plot.plmodel.fit(model$fit, ...)

  .plot.profile.obs <- function(model, ...)
                         asdog.plot.plmodel.profile(model, "obs", ...)
  
  .plot.profile.expect <- function(model, ...)
                            asdog.plot.plmodel.profile(model, "expect", ...)
  
  what <- match.arg(what)
  
  fun <- get(paste0(".plot.", what), mode="function")
  
  fun(plmodel, ...)
}

