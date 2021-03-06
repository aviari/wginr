# -------------------------------------------------
# $Id: asdog.gcmodel.gloess.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# GC loess or gam model
#

# =================================================
# internals <no_export>
# =================================================

# -------------------------------------------------
# select points in most populated GC band
#
.select.gc.band <- function(pts, nbins=10) {
  scut <- cut(pts$gc, nbins, labels=1:nbins)
  sidx <- which(scut == which.max(table(scut)))
  list(ref.gc=mean(pts$gc[sidx]), range.gc=range(pts$gc[sidx]),
       idx=sidx, pos=pts$pos[sidx])
}

# -------------------------------------------------
# sample equally spaced points
#
.sample.spaced <- function(from, to, winsize, by=winsize) {
  ok <- (to - from + 1) >= winsize
  if (all(! ok)) return(integer(0))
  unlist(mapply(function(f,t) seq.int(f+winsize/2, t-winsize/2-1, by=by),
                from[ok], to[ok], SIMPLIFY=F))
}

# -------------------------------------------------
# quicker aggregate using lx.condsum
#
.aggregate <- function(x, y, N=1000) {
  res <- lx.condsum(round(x*N), round(y*N))
  res[,2] <- res[,3]/res[,2]/N
  res <- as.data.frame(res[,1:2])
  names(res) <- c("gc", "cov")
  res
}

# -------------------------------------------------
# initialize gloess model
#
.gloess.init <- function(pts, name="", fit="none", N=1000, span=0.75,
                         base.quant=0.01, base.trim=c(0.2, 0.8)) {
  mod <- list()
  mod$name <- name
  mod$span <- span
  mod$fit  <- fit
  mod$N    <- N
  mod$base.quant <- base.quant
  mod$base.trim  <- base.trim
  mod$cov  <- pts$cov
  mod$mcov <- median(mod$cov, na.rm=T)
  mod$gc   <- pts$gc
  #
  # -- slower version but uses median
  # mod$mean <- aggregate(mod$cov, by=list(gc=round(mod$gc*N)), median)
  # names(mod$mean) <- c("gc", "cov")
  #
  mod$mean <- .aggregate(mod$gc, mod$cov, N=N)
  mod
}

# -------------------------------------------------
# make constant gloess model
#
.gloess.constant <- function(pts, name="Constant", N=1000) {
  
  mod <- .gloess.init(pts, name, N=N, span=NA, base.quant=NA, base.trim=NULL)

  mod$pvec <- rep(mod$mcov, N+1)

  mod
}

# -------------------------------------------------
# make standard gloess model
#
.gloess.model <- function(pts, name="", N=1000, fit="gam", span=0.75, 
                          base.quant=0.01, base.trim=c(0.2, 0.8)) {

  mod <- .gloess.init(pts, name, fit=fit, N=N, span=span,
                      base.quant=base.quant, base.trim=base.trim)
  
  # baseline adjustment
  #
  baseline <- if (is.na(base.quant) || (base.quant <= 0)) 1 
              else quantile(mod$cov, base.quant, na.rm=T)
  cov <- c(baseline, baseline, mod$mean$cov)
  gc  <- c(0, N, mod$mean$gc)
  
  # make gloess model
  #
  gloess <- tryCatch(switch(fit,
                       gam=gam(cov ~ s(gc)),
                       loess(cov ~ gc, span=span,
                             control=loess.control(surface="direct"))),
              error = function(e) {
                lx.warn("gam/loess model failed - FIXME")
                .gloess.constant(pts=pts, name="Rescue", N=N)
              })

  if (! is.null(gloess$name)) # failure -> constant model
    return(gloess)
  
  mod$gloess <- gloess
  
  # prediction vector 
  #
  mod$pvec <- switch(fit,
                     gam=predict(mod$gloess, list(gc=seq.int(0, N))),
                     predict(mod$gloess, seq.int(0, N))
              )
  mod$pvec[mod$pvec <= baseline] <- baseline
  
  # trim low and high gc
  #
  if (! is.null(base.trim)) {
    bnds <- round(base.trim*N)
    mod$pvec[1:bnds[1]] <- mod$pvec[bnds[1]]
    mod$pvec[bnds[2]:length(mod$pvec)] <- mod$pvec[bnds[2]]
  }
  
  # remove outliers (may occur if span is too small)
  #
  outl <- 5*median(mod$pvec, na.rm=T)
  mod$pvec[mod$pvec >= outl] <- outl
  
  mod
}

# -------------------------------------------------
# gloess model predictor
#
.gloess.predict <- function(mod, gc) mod$pvec[round(gc*mod$N)+1]


# -------------------------------------------------
# init GC model
#
.model.init <- function(nmodel, winsize.gc=0, winsize.cov=NA,
                        fit="none", span=NA, base.quant=NA, base.trim=NULL,
                        nband=NA) {

  model <- asdog.object("GCGloessModel")
  model$nmodel      <- nmodel
  model$winsize.gc  <- winsize.gc
  model$winsize.cov <- winsize.cov
  model$fit         <- fit
  model$span        <- span
  model$base.quant  <- base.quant
  model$base.trim   <- base.trim
  model$nband       <- nband
  model$gloess      <- vector("list", nmodel)
  
  model
}

# -------------------------------------------------
# constant GC model
#
.model.constant <- function(datasrc, clocations, 
                            winsize.cov=1000L,
                            use.threads=lx.use.threads()) {
  
  lx.out("making model Constant")
  
  model <- .model.init(nmodel=1, winsize.cov=winsize.cov)
  
  pts <- asdog.rcgc.collect(datasrc, clocations,
                           winsize.gc=winsize.cov,
                           winsize.cov=winsize.cov,
                           use.threads=use.threads)
  
  gloess <- .gloess.constant(pts, "Constant")

  gloess$winsize.gc  <- winsize.cov
  gloess$winsize.cov <- winsize.cov
  
  pts$pred <- rep(gloess$mcov, length(pts$pos))
  
  model$pts <- pts
  
  model$alpha <- 1
  
  model$gloess[[1]] <- gloess
  
  model
}

# -------------------------------------------------
# gloess GC model with single window
#
.model.single <- function(datasrc, clocations, 
                          winsize.gc=1000L, 
                          winsize.cov=1000L,
                          fit="gam",
                          span=0.75,
                          base.quant=0.01, base.trim=c(0.2, 0.8),
                          use.threads=lx.use.threads()) {
  
  lx.out("making model Single W=", winsize.gc)
  
  model <- .model.init(nmodel=1, winsize.gc=winsize.gc,
                       winsize.cov=winsize.cov, fit=fit, span=span,
                       base.quant=base.quant, base.trim=base.trim)
  
  pts <- asdog.rcgc.collect(datasrc, clocations,
                           winsize.gc, winsize.cov,
                           use.threads=use.threads)
  
  gloess <- .gloess.model(pts, "Single", fit=fit,
                          span=span[1],
                          base.quant=base.quant, 
                          base.trim=base.trim)
  
  gloess$winsize.gc  <- winsize.gc
  gloess$winsize.cov <- winsize.cov
  
  pts$pred <- .gloess.predict(gloess, pts$gc)
  
  model$pts <- pts
  
  model$alpha <- median(pts$cov/pts$pred, na.rm=T)
  
  model$gloess[[1]] <- gloess
  
  model
}

# -------------------------------------------------
# gloess GC model with two windows
#
.model.twin <- function(datasrc, clocations,
                        winsize.gc=c(1000L, 100000L), 
                        winsize.cov=c(1000L, 1000L),
                        fit="gam",
                        span=0.75,
                        base.quant=0.01, base.trim=c(0.2,0.8),
                        nband=c(4L, 10L),
                        use.threads=lx.use.threads()) {
  
  model <- .model.init(nmodel=2, winsize.gc=winsize.gc,
                       winsize.cov=winsize.cov, fit=fit, span=span,
                       base.quant=base.quant, base.trim=base.trim,
                       nband=nband)
  
  # -- Large window
  # remove small window effect in large window model
  #
  lx.out("making model Large W=", winsize.gc[2])
  
  lpts.full <- asdog.rcgc.collect(datasrc, clocations,
                                 winsize.gc[2], winsize.cov[2],
                                 use.threads=use.threads)
  
  # subsample large windows (we shall oversample later on) 
  #
  nb.sub <- length(.sample.spaced(-winsize.gc[2]/2, winsize.gc[2]/2,
                                  winsize.gc[1], winsize.gc[1]))
  idx.sub <- seq.int(1, length(lpts.full$pos), by=nb.sub)
  lpts <- lpts.full
  lpts$pos <- lpts.full$pos[idx.sub]
  lpts$gc  <- lpts.full$gc[idx.sub]
  
  # oversample small windows in large window
  #
  samp.ovr <- .sample.spaced(lpts$pos-winsize.gc[2]/2,
                             lpts$pos+winsize.gc[2]/2,
                             winsize.gc[1], winsize.gc[1])
  
  lpts.ovr <- asdog.rcgc.collect(datasrc, samp.ovr,
                                winsize.gc[1], winsize.cov[1],
                                use.threads=use.threads)
  
  cov.mat <- matrix(lpts.ovr$cov, ncol=nb.sub, byrow=T)
  
  # collect small windows within GC bandwidth
  # (and increase bandwidth if necessary)
  #
  nna.frac  <- 0
  nband.min <- min(nband)
  nband     <- max(nband)
  while ((nna.frac < 0.5) && (nband >= nband.min)) {
    band <- .select.gc.band(lpts.full, nbins=nband)
    cut.mat <- matrix(cut(lpts.ovr$gc, band$range.gc, label=F), ncol=nb.sub, byrow=T)
    lpts$cov  <- rowMeans(cut.mat * cov.mat, na.rm=T)
    nna.frac <- sum(!is.na(lpts$cov))/length(lpts$cov)
    lx.out("nband=", nband, " bandwidth=", format(band$range.gc, digits=3), 
           " frac=", format(nna.frac, digits=3), level="debug")
    if (nna.frac < 0.5) nband <- nband - 1
  }
  
  # maybe we prefer to remove NaN
  if (FALSE) {
    na.cov <- is.na(lpts$cov)
    lpts$cov[na.cov] <- mean(lpts$cov[! na.cov])
    lpts$gc[na.cov]  <- mean(lpts$gc[! na.cov])
  }
  
  gloess <- .gloess.model(lpts, "Large", fit=fit,
                          span=span[2],
                          base.quant=base.quant,
                          base.trim=base.trim)
  
  gloess$winsize.gc  <- winsize.gc[2]
  gloess$winsize.cov <- winsize.cov[2]
  model$gloess[[2]] <- gloess
  
  model$band  <- band
  
  # -- Small window
  # remove large window effect in small window model
  #
  lx.out("making model Small W=", winsize.gc[1])
  
  # resample small window within large windows in GC band
  #
  samp.swin <- .sample.spaced(band$pos-winsize.gc[2]/2, band$pos+winsize.gc[2]/2,
                              winsize.gc[1], winsize.gc[1])
  step.swin <- floor(length(samp.swin)/nrow(clocations))
  if (step.swin > 1)
    samp.swin <-samp.swin[seq.int(1, length(samp.swin), by=step.swin)]
  
  spts  <- asdog.rcgc.collect(datasrc, samp.swin,
                             winsize.gc[1], winsize.cov[1],
                             use.threads=use.threads)
  
  gloess <- .gloess.model(spts, "Small", fit=fit,
                          span=span[1],
                          base.quant=base.quant,
                          base.trim=base.trim)
  
  gloess$winsize.gc  <- winsize.gc[1]
  gloess$winsize.cov <- winsize.cov[1]
  
  model$gloess[[1]] <- gloess
  
  # compute model$alpha
  
  model$alpha <- 1
  
  pred <- asdog.gcPredict.gloess(datasrc, model, clocations,
                                 use.threads=use.threads)
  
  model$alpha <- median(lpts.full$cov/pred$cov, na.rm=T)

  model
}

# =================================================
# API
# =================================================

# -------------------------------------------------
# gloess GC model (with one or two windows)
#
asdog.gcModel.gloess <- function(datasrc, locs, 
                                 winsize.gc=c(1000L, 100000L), 
                                 winsize.cov=1000L,
                                 fit=c("gam", "loess"),
                                 span=0.75,
                                 base.quant=0.01, base.trim=c(0.2,0.8),
                                 nband=c(4L, 10L),
                                 use.threads=lx.use.threads()) {
  
  fit <- match.arg(fit)
  
  clocations <- if (is.null(dim(locs))) coords2clocs(datasrc, locs)
                else locs
  
  nmodel      <- length(winsize.gc)
  winorder    <- order(winsize.gc)
  winsize.gc  <- head(winsize.gc[winorder], 2)
  winsize.cov <- lx.recycle(winsize.cov, nmodel)[winorder]
  span        <- lx.recycle(span, nmodel)[winorder]

  # -----------------------------------------
  # single window model
  # -----------------------------------------

  if (nmodel == 1) {
    model <- if (winsize.gc[1] <= 0)
                .model.constant(datasrc, clocations,
                                winsize.cov=winsize.cov[1],
                                use.threads=use.threads)
             else 
                .model.single(datasrc, clocations,
                              winsize.gc=winsize.gc[1],
                              winsize.cov=winsize.cov[1],
                              fit=fit,
                              span=span,
                              base.quant=base.quant,
                              base.trim=base.trim,
                              use.threads=use.threads)
    return(model)
  }

  # -----------------------------------------
  # two windows model with at least one of width <= 0
  # => recurse to single window
  # -----------------------------------------
  
  if (min(winsize.gc) <= 0) {
    model <- asdog.gcModel.gloess(datasrc, clocations,
                                  winsize.gc=max(winsize.gc),
                                  winsize.cov=winsize.cov[which.max(winsize.gc)],
                                  fit=fit,
                                  span=span,
                                  base.quant=base.quant,
                                  base.trim=base.trim,
                                  use.threads=use.threads)
    return(model)
  }
  
  # -----------------------------------------
  # two windows model
  # -----------------------------------------

  model <- .model.twin(datasrc, clocations,
                       winsize.gc=winsize.gc,
                       winsize.cov=winsize.cov,
                       fit=fit,
                       span=span,
                       base.quant=base.quant,
                       base.trim=base.trim,
                       nband=c(min(nband), max(nband)),
                       use.threads=use.threads)
  
  return(model)
}

# -------------------------------------------------
# GC gloess model predict
#
asdog.gcPredict.gloess <- function(datasrc, model, locs,
                                   use.threads=lx.use.threads()) {
  
  clocations <- if (is.null(dim(locs))) coords2clocs(datasrc, locs) else locs
  
  pts <- lapply(model$gloess, function(gloess)
              asdog.rcgc.collect(datasrc, clocations, 
                                gloess$winsize.gc, 0L,
                                use.threads=use.threads))
  
  pred <- .gloess.predict(model$gloess[[1]], pts[[1]]$gc)
  
  if (model$nmodel > 1) {
    pred <- pred +
            .gloess.predict(model$gloess[[2]], pts[[2]]$gc) -
            .gloess.predict(model$gloess[[2]], model$band$ref.gc)
  }
  
  list(gcs=pts[[1]]$gc, gcl=pts[[model$nmodel]]$gc, cov=pred*model$alpha)
}


# -------------------------------------------------
# gloess GC model optimization
#
# note: next two functions should be global
#       because of using lx.get0 backport for R < 3.2.0 :-(
#
asdog.gloess.objective.pearson <- function(cov, pred, gc) {
    abs(cor(cov/pred, gc, method="pearson"))
}

asdog.gloess.objective.gam.rsq <- function(cov, pred, gc, N=1000) {
  abs(summary(gam(lx.decimate(cov/pred, N) ~ s(lx.decimate(gc, N))))$r.sq)
}

asdog.gcOptimize.gloess <- function(datasrc, coords,
                                learn.ratio=0.1,
                                winsize.gc=list(c(0L, 500L, 1000L, 2000L, 5000L),
                                                c(0L, 1e5L, 2e5L, 5e5L, 1e6L)),
                                winsize.cov=1000L,
                                fit=c("gam", "loess"),
                                objective=c("gam.rsq", "pearson"),
                                span=0.75,
                                base.quant=0.01,
                                base.trim=NULL,
                                nband=c(4L, 10L),
                                use.threads=lx.use.threads()) {
  
  fit <- match.arg(fit)
  objective <- match.arg(objective)
  objective.fun <- lx.get0(paste0("asdog.gloess.objective.", objective), mode="function")
  
  # remove outliers
  #
  pts <- asdog.rcgc.collect(datasrc, coords,
                           winsize.gc=0,
                           winsize.cov=winsize.cov,
                           use.threads=use.threads)
  
  coords <- coords[pts$cov <= 2 * median(pts$cov, na.rm=T)]

  # separate learn and test sets  
  #
  ntot <- length(coords)
  coords.learn <- coords[seq.int(1, ntot, length.out=round(ntot*learn.ratio))]
  coords.test  <- sort(setdiff(coords, coords.learn))
  
  clocs.learn <- coords2clocs(datasrc, coords.learn)
  clocs.test  <- coords2clocs(datasrc, coords.test)
  
  pts.test <- asdog.rcgc.collect(datasrc, clocs.test,
                                winsize.gc=winsize.cov, # this is ok
                                winsize.cov=winsize.cov,
                                use.threads=use.threads)
  
  # search for best windows
  #
  swins <- winsize.gc[[1]]
  lwins <- winsize.gc[[2]]
  
  scores <- sapply(lwins, function(lwin) {
    sapply(swins, function(swin) {
      mod <- asdog.gcModel.gloess(datasrc, clocs.learn,
                                  winsize.gc=c(swin, lwin),
                                  winsize.cov=winsize.cov,
                                  fit=fit,
                                  span=span,
                                  nband=nband,
                                  base.quant=base.quant,
                                  base.trim=base.trim,
                                  use.threads=use.threads)
      
      pred <- asdog.gcPredict.gloess(datasrc, mod, clocs.test,
                                    use.threads=use.threads)

      objective.fun(pts.test$cov, pred$cov, pts.test$gc)

    })
  })
    
  colnames(scores) <- lwins
  rownames(scores) <- swins

  # retrieve best model
  #
  scores.min <- which(scores==min(scores),arr.ind=T)
  best.swin <- swins[scores.min[[1]]]
  best.lwin <- lwins[scores.min[[2]]]
  
  best.mod <- asdog.gcModel.gloess(datasrc, clocs.learn,
                                   winsize.gc=c(best.swin, best.lwin),
                                   winsize.cov=winsize.cov,
                                   fit=fit,
                                   span=span,
                                   nband=nband,
                                   base.quant=base.quant,
                                   base.trim=base.trim,
                                   use.threads=use.threads)
  
  pred <- asdog.gcPredict.gloess(datasrc, best.mod, clocs.test,
                                use.threads=use.threads)

  pred <- list(pos=pts.test$pos, 
               gc=pts.test$gc, 
               cov=pts.test$cov,
               pred=pred$cov)
  
  info <- list(fit=fit, objective=objective)
  
  list(scores=scores, model=best.mod, test=pred, info=info)
}

# -------------------------------------------------
# S3 method to plot GCGloess model
#
plot.GCGloessModel <- function(model, xlim=c(0.2,0.8), ylim=c(0,2), ...) {
  
  # setup
  opar <- par(no.readonly=T)
  par(mfrow=c(1, model$nmodel),
      cex.main=0.9)
  
  # model
  sapply(model$gloess, function(mod) {
    lx.plot(mod$gc, mod$cov/mod$mcov,
            xlim=xlim,
            ylim=ylim,
            xlab="%GC", ylab="rel. cov.",
            main=paste0(mod$name, " W=", mod$winsize.gc), ...)
    points(mod$mean$gc/mod$N, mod$mean$cov/mod$mcov, pch=19, cex=0.3, col=4)
    lines(seq.int(0, 1, 1/mod$N), mod$pvec/mod$mcov, col=2, lwd=3)
    abline(h=1, col=3)
    if (is.null(model$band$range.gc))
      abline(v=mean(mod$gc), col=3)
    else {
      py <- par("usr")[3:4]
      rect(model$band$range.gc[1], py[1], model$band$range.gc[2], py[2],
           col=lx.color.alpha(lx.BLUE, 0.3), border=NA)
      abline(v=model$band$ref.gc, col=3)
    }
  })
  
  # restore
  par(opar)
  
  invisible()
}


