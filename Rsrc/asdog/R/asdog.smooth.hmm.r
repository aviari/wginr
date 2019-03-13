# -------------------------------------------------
# $Id: asdog.smooth.hmm.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# Pre-Segmentation : HMM Smoothing
#
# Note: this method has been superseeded by BCP (see asdog.bcp.r)
#       and is no longer the default. It may be removed from API
#       in the future.
#

# -------------------------------------------------
# <internal> utility: check error class
#
.is.error <- function(e) "error" %in% class(e)

# -------------------------------------------------
# <internal> utility: iota function
#
.iota <- function(n) if (n > 0) seq_len(n) else integer(0)

# -------------------------------------------------
# <internal> utility: generic iota filter 
#
.filter.iota <- function(x, fun) {
  m <- length(x)
  rem  <- logical(m)
  for (i in seq_len(m)) {
    rem[i] <- any(fun(i, x) & (! rem[.iota(i-1)]))
  }
  rem
}

# -------------------------------------------------
# <internal> utility: init hmm from previous hmm,
# overiding mean or sd parameters
#
.hmm.init <- function(hmm, mean=NULL, sd=NULL) {
  mean <- if (is.null(mean)) thmm.parameters(hmm, "mean") else mean
  sd   <- if (is.null(sd))   thmm.parameters(hmm, "sd")   else sd
  tau  <- if (nrow(hmm$trans) > 1) hmm$trans[1,2] else 1
  
  thmm.init(dnorm, tau, mean=mean, sd=sd)
}

# -------------------------------------------------
# <internal> utility: rescale hmm sd (used in .hmm.baumwelch)
#
.hmm.scale.sd <- function(hmm, alpha=2) {
  sd <- thmm.parameters(hmm, "sd")
  if (all(sd == sd[1]))
    sd <- alpha * sd
  else
    sd[which.min(sd)] <- alpha * sd[which.min(sd)]
  .hmm.init(hmm, sd=sd)
}

# -------------------------------------------------
# <internal> utility
# baum-welch with no optimisation on trans and init.
# original baum-welch may fail if sd is too low as compared
# to the signal dynamic range (this produces NA's by underflow)
# the retry paramter controls if we should retry with increased sd.
#
.hmm.baumwelch <- function(hmm, obs, tol=1e-5, verbose=FALSE, retry=TRUE, ...) {
  ctrl <- thmm.bw.ctrl(hmm, maxiter=100, do.trans=FALSE, do.init=FALSE,
                       tol=tol, verbose=verbose, ...)
  res <- tryCatch(thmm.baumwelch(hmm, obs, ctrl), error=function(e) e)
  
  if (.is.error(res)) {
    if (! retry) stop(res$message)
    lx.warn("retrying : ", res$message)
    hmm <- .hmm.scale.sd(hmm)
    res <- .hmm.baumwelch(hmm, obs, tol=tol, verbose=verbose, retry=retry, ...)
  }
  res
}

# -------------------------------------------------
# <internal> utility
# BIC calculation
#
.bic <- function(hmm, obs, rebaum=FALSE, tol=1e-5, verbose=FALSE) {
  if (rebaum) hmm <- .hmm.baumwelch(hmm, obs, tol=tol, verbose=verbose)$hmm
  bic <- thmm.bic(hmm, obs, np=2, nt=1, ni=0)
  list(hmm=hmm, bic=bic)
}

# -------------------------------------------------
# <internal> utility
# reduce HMM : remove states
#
.hmm.remove.states <- function(hmm, istates) {
  if (length(istates) == 0) return(hmm)
  .cycle <- function(x, n) head(rep(x, n), n)
  .prm <- function(hmm, what) .cycle(thmm.parameters(hmm, what), nrow(hmm$trans))
  mean <- .prm(hmm, "mean")[-istates]
  sd   <- .prm(hmm, "sd")[-istates]
  init <- .prm(hmm, "init")[-istates]
  trans <- hmm$trans[-istates, -istates]
  thmm.init(hmm$info$density, trans, init, mean=mean, sd=sd)
}

# -------------------------------------------------
# <internal> utility
# reduce HMM : score states
#
.hmm.score.reduce <- function(hmm, obs, bic0=NA, rebaum=FALSE, tol=1e-2, verbose=FALSE) {
  m <- nrow(hmm$trans)
  if (m <= 1) return(list())
  bics <- lx.lapply(seq_len(m), function(i) {
    h <- .hmm.remove.states(hmm, i)
    b <- .bic(h, obs, rebaum=rebaum, tol=tol, verbose=verbose)$bic
    lx.out("hmm reduce: i=", i, "/", m, " bic=", b, " delta=", b-bic0, level="debug")
    b
  })
  unlist(bics)
}

# -------------------------------------------------
# <internal> utility
# reduce HMM : heuristic reduction
# this is a heuristics. hmm states are assumed to be ordered by decreasing weight
#
.hmm.quick.reduce <- function(hmm, maxlev=Inf) {
  .cycle <- function(x, n) head(rep(x, n), n)
  .prm <- function(hmm, what) .cycle(thmm.parameters(hmm, what), nrow(hmm$trans))
  m    <- nrow(hmm$trans)
  mean <- .prm(hmm, "mean")
  sd   <- .prm(hmm, "sd")
  rem  <- .filter.iota(mean, function(i, m) abs(m[.iota(i-1)] - m[i]) < sd[i])
  rem  <- rem | (cumsum(!rem) > maxlev)
  lx.out("quick reduce removed : ", sum(rem), " / ", m, " states", level="debug")
  .hmm.remove.states(hmm, which(rem))
}

# -------------------------------------------------
# <internal> utility
# reduce HMM : recursive reduction
#
.hmm.recursive.reduce <- function(hmm, obs, rebaum=FALSE, tol=1e-2, verbose=FALSE) {
  if (nrow(hmm$trans) <= 1) return(hmm)
  bic0 = .bic(hmm, obs, rebaum=rebaum, tol=tol, verbose=verbose)$bic
  lx.out("hmm reduce: bic0=", bic0, level="debug")
  bics <- .hmm.score.reduce(hmm, obs, bic0, rebaum=rebaum, tol=tol, verbose=verbose)
  imin <- which.min(bics)
  if (bics[imin] < bic0) {
    lx.out("hmm reduce: removed level ", imin, level="debug")
    .hmm.recursive.reduce(.hmm.remove.states(hmm, imin), obs,
                          rebaum=rebaum, tol=tol, verbose=verbose)
  } else
    hmm
}

# -------------------------------------------------
# <internal> utility
# reduce HMM : main call
#
.hmm.reduce <- function(hmm, obs, quick=FALSE, rebaum=FALSE, reorder=FALSE,
                        tol=1e-2, maxlev=Inf, verbose=FALSE) {
  if (quick) hmm <- .hmm.quick.reduce(hmm, maxlev=maxlev)
  hmm <- .hmm.recursive.reduce(hmm, obs, rebaum=rebaum, tol=tol, verbose=verbose)
  m <- nrow(hmm$trans)
  if (m > maxlev) {
    ord <- if (reorder) {
      bics <- .hmm.score.reduce(hmm, obs, rebaum=FALSE)
      order(bics, decreasing=FALSE)
    } else .iota(m)
    hmm <- .hmm.remove.states(hmm, tail(ord, -maxlev))
  }
  hmm
}

# -------------------------------------------------
# <internal> utility
# segment using HMM
#
.hmm.segment <- function(hmm, obs, minseg.size=2, mean.fun=median, sd.fun=mad) {
  mx <- thmm.parameters(hmm, "mean")
  vit <- thmm.viterbi(hmm, obs, with.probseq=F)
  r <- rle(vit$states)
  to <- cumsum(r$lengths)
  from <- head(c(1, to+1), -1)
  seg <- data.frame(cbind(ifrom=from, ito=to, npt=to-from+1,
                          state=r$values, stval=mx[r$values]))
  seg <- seg[seg$npt >= minseg.size,,drop=F]
  seg$mean <- apply(seg, 1, function(r) mean.fun(obs[r[2]:r[1]]))
  seg$sd   <- apply(seg, 1, function(r) sd.fun(obs[r[2]:r[1]]))
  
  seg
}

# -------------------------------------------------
# <internal> utility
# expand HMM states using segments
#
.hmm.expand <- function(hmm, obs, quick=FALSE, rebaum=TRUE, reorder=FALSE,
                        tol=1e-2, maxlev=10,
                        minseg.size=2, verbose=FALSE, .eps=1e-6) {
  lx.out("hmm expand init: ", nrow(hmm$trans), " states", level="debug")
  seg <- .hmm.segment(hmm, obs, minseg.size=minseg.size)
  seg <- seg[order(seg$npt, decreasing=T),]
  rem  <- .filter.iota(seg$mean, function(i, m) abs(m[.iota(i-1)] - m[i]) < .eps)
  seg <- seg[!rem,]
  tau <- if (nrow(hmm$trans) > 1) hmm$trans[1,2] else 1
  hmm <- thmm.init(hmm$info$density, tau, hmm$init,
                   mean=seg$mean, sd=pmax(seg$sd, 1e-3))
  lx.out("hmm expand inter: ",  nrow(hmm$trans), " states", level="debug")
  hmm <- .hmm.reduce(hmm, obs, quick=quick, rebaum=rebaum, reorder=reorder, 
                     tol=tol, maxlev=maxlev, verbose=verbose)
  lx.out("hmm expand end: ",  nrow(hmm$trans), " states", level="debug")
  hmm
}

#####
# API
#####

# -------------------------------------------------
# normal HMM smoothing / generic call
#
asdog.smooth.hmm.dnorm <- function(x, xmin=0, xmax=max(x, na.rm=T),
                                   raw=TRUE, quick=TRUE, rebaum=TRUE, reorder=FALSE,
                                   smooth=c(3,5,15,35,55,105), eps.max=0.1, 
                                   tol.bw=1e-5, tol.expand=1e-2, max.levels=10L,
                                   sample.size=100000L,
                                   minseg.size=150L, tau=1e-50) {
  
  verbose <- lx.verbose() < 10 # be verbose if level < "info"
  
  minseg.size <- max(2, minseg.size)
  
  # threshold - should have been done before - this is just a patch in case of
  x <- pmax(pmin(x, xmax), xmin)
  x[is.na(x)] <- median(x, na.rm=T)
  
  # compute hmm0 levels using smoothed density
  lx.out("smoothing signal", level="info")
  sx <- lx.smooth.median(x, k=smooth, tol=0)
  ds <- lx.density(sx)
  m0 <- ds$x[lx.maxima(ds$y, span=0, eps=eps.max)]
  m0 <- c(head(m0, max(max.levels-2, 2)), xmin, xmax)
  
  # decimate for speedup
  if (is.na(sample.size)) {
    pos <- seq_along(x)
  } else {
    pos <- lx.decimate(x, sample.size, as.index=T)
    x  <- x[pos]
    sx <- sx[pos]
  }

  # choose work signal
  wx <- if (raw) x else sx
  
  # estimate hmm0 sd
  wsd <- min(minseg.size, ceiling(length(wx)/10))
  sd0 <- median(sapply(seq.int(from=1, to=length(wx)-wsd, by=wsd/2),
                       function(i) sd(wx[i:(i+wsd)])))
  
  # make first hmm
  lx.out("init hmm0 : ", length(m0), " levels sd0=", round(sd0, digits=4), level="info")
  hmm0 <- thmm.init(dnorm, tau, mean=m0, sd=sd0)
  hmm1 <- .hmm.baumwelch(hmm0, wx, tol=tol.bw, verbose=verbose)$hmm
  m1 <- thmm.parameters(hmm1, "mean")
  
  # expand hmm
  lx.out("expand hmm1 : ", length(m1), " levels", level="info")
  hmm2 <- .hmm.expand(hmm1, wx, quick=quick, rebaum=rebaum, reorder=reorder,
                      tol=tol.expand, maxlev=max.levels, minseg.size=minseg.size,
                      verbose=verbose)
  hmm2 <- .hmm.baumwelch(hmm2, wx, tol=tol.bw, verbose=verbose)$hmm
  m2 <- thmm.parameters(hmm2, "mean")
  
  # segment
  seg <- .hmm.segment(hmm2, wx, minseg.size=minseg.size)
  seg$pfrom <- pos[seg$ifrom]
  seg$pto   <- pos[seg$ito]
  
  lx.out("final hmm : ", length(m2), " levels ", nrow(seg), " segments", level="info")

  # return
  list(pos=pos, ds=ds, raw=x, smo=sx, obs=wx, hmm0=hmm0, hmm=hmm2, seg=seg)
}

# -------------------------------------------------
# normal HMM smoothing / rRC smoothing
#
asdog.smooth.hmm.rrc <- function(rrc, rcmax=4, raw=TRUE,
                                 quick=TRUE, rebaum=TRUE, reorder=FALSE,
                                 smooth=c(3,5,15,35,55,105), eps.max=0.1,
                                 tol.bw=1e-5, tol.expand=1e-2,
                                 max.levels=10L, 
                                 sample.size=100000L,
                                 minseg.size=150L, tau=1e-50) {
  
  lx.out("hmm smooth rrc", level="info")
  res <- asdog.smooth.hmm.dnorm(rrc, xmin=0, xmax=rcmax, raw=raw,
                                quick=quick, rebaum=rebaum, reorder=reorder,
                                smooth=smooth, eps.max=eps.max,
                                tol.bw=tol.bw, tol.expand=tol.expand,
                                max.levels=max.levels,
                                sample.size=sample.size,
                                minseg.size=minseg.size, tau=tau)
  res
}

# -------------------------------------------------
# normal HMM smoothing / AF smoothing
#
asdog.smooth.hmm.baf <- function(baf, raw=FALSE,
                                 quick=TRUE, rebaum=TRUE, reorder=TRUE,
                                 smooth=c(3,5,15,35,55,105), eps.max=0.1,
                                 tol.bw=1e-5, tol.expand=1e-2,
                                 max.levels=5L,
                                 sample.size=100000L,
                                 minseg.size=150L, tau=1e-50) {
  
  # recenter baf on 0
  baf <- baf - 0.5

  # fold signal
  fld <- abs(baf)
  
  # hmm smooth
  lx.out("hmm smooth folded baf", level="info")
  res <- asdog.smooth.hmm.dnorm(fld, xmin=0, xmax=0.5, raw=raw,
                                quick=quick, rebaum=rebaum, reorder=reorder,
                                smooth=smooth, eps.max=eps.max,
                                tol.bw=tol.bw, tol.expand=tol.expand,
                                max.levels=max.levels,
                                sample.size=sample.size,
                                minseg.size=minseg.size, tau=tau)

  # get decimated positions used in asdog.smooth.hmm.dnorm
  baf <- baf[res$pos]
  fld <- fld[res$pos]

  seg <- res$seg

  # unfold signal on segments
  lx.out("unfolding segments", level="info")
  mfl <- apply(seg, 1, function(r) median(fld[r[[1]]:r[[2]]]))
  sfl <- apply(seg, 1, function(r)     sd(fld[r[[1]]:r[[2]]]))
  
  x <- t(mapply(function(m, s) asdog.unfold(m, s, type="median")$solution,
                mfl, sfl, USE.NAMES=F))
  seg$mean <- unlist(x[,1])
  seg$sd   <- unlist(x[,2])
  
  if (FALSE) {
    # unfold smoothed signal - using mean sd
    lx.out("unfolding smoothed signal", level="info")
    sd0 <- median(sfl)
    usmo <- lx.lapply(res$smo, function(m) asdog.unfold(m, sd0)$solution$mu)
    res$smo <- usmo
  }

  if (TRUE) {
    # unfold hmm
    lx.out("unfolding hmm", level="info")
    mfl <- thmm.parameters(res$hmm, "mean")
    hfl <- thmm.parameters(res$hmm, "sd")
    sfl <- hfl * median(sfl) / median(hfl)
    x <- mapply(function(m, s) asdog.unfold(m, s, type="median")$solution, mfl, sfl)
    hm <- unlist(x[1,])
    hs <- unlist(x[2,])
    hmm <- thmm.init(dnorm, tau, mean=hm, sd=hs)
    
    res$hmm <- hmm
    seg$stval <- hm[seg$state]
  }
  
  # end
  
  res$raw <- baf
  res$seg <- seg
  res
}

# -------------------------------------------------
# HMM smooth and segment rRC and AF
#
asdog.smooth.hmm.segment <- function(rrc, baf,
          rc.ctrl=list(rcmax=4L, raw=TRUE, smooth=c(3L, 5L, 15L, 35L, 55L, 105L),
                       eps.max=0.1, max.levels=10L, sample.size=1e5L,
                       minseg.size=150L, tau=1e-50),
          af.ctrl=list(raw=FALSE, smooth=c(3L, 5L, 15L, 35L, 55L, 105L),
                       eps.max=0.1, max.levels=5L, sample.size=1e5L,
                       minseg.size=150L, tau=1e-50)) {
  
  # mixin user's and formal arguments
  # allow user to specify only part of xxx.ctrl arguments
  #
  dft <- formals()  # dft values from declared arguments
  rc.ctrl <- lx.mixin(dft$rc.ctrl, rc.ctrl)
  af.ctrl <- lx.mixin(dft$af.ctrl, af.ctrl)
  
  # threshold rrc
  df <- data.frame(rrc=rrc, baf=baf)
  df <- na.omit(df[df$rrc <= rc.ctrl$rcmax,])
  
  # make sure sample size are the same for rc and af
  sample.size <- max(rc.ctrl$sample.size, af.ctrl$sample.size)
  
  # make sure minseg.size is below data length
  limseg.size <- ceiling(nrow(df)/10)
  rc.minseg.size <- rc.ctrl$minseg.size
  if (rc.minseg.size > limseg.size) {
    lx.warn("rc minseg.size [", rc.minseg.size, "] too big for data length. will be reduced to ", limseg.size)
    rc.minseg.size <- limseg.size
  }
  af.minseg.size <- af.ctrl$minseg.size
  if (af.minseg.size > limseg.size) {
    lx.warn("af minseg.size [", af.minseg.size, "] too big for data length. will be reduced to ", limseg.size)
    af.minseg.size <- limseg.size
  }
  
  # smooth rc
  lx.out("smoothing rRC", with.mem=T)
  
  res.rc <- asdog.smooth.hmm.rrc(df$rrc, rcmax=rc.ctrl$rcmax, raw=rc.ctrl$raw,
                                 smooth=rc.ctrl$smooth, eps.max=rc.ctrl$eps.max,
                                 max.levels=rc.ctrl$max.levels, 
                                 sample.size=sample.size,
                                 minseg.size=rc.minseg.size, tau=rc.ctrl$tau)
  
  # smooth af
  lx.out("smoothing AF", with.mem=T)
  
  res.af <- asdog.smooth.hmm.baf(df$baf, raw=af.ctrl$raw,
                                 smooth=af.ctrl$smooth, eps.max=af.ctrl$eps.max,
                                 max.levels=af.ctrl$max.levels,
                                 sample.size=sample.size,
                                 minseg.size=af.minseg.size, tau=af.ctrl$tau)
  
  # intersect RC and AF segment
  lx.out("intersecting rRC and AF segments", with.mem=T)
  
  irc <- intervals::Intervals(as.matrix(res.rc$seg[,1:2]))
  iaf <- intervals::Intervals(as.matrix(res.af$seg[,1:2]))
  int <- intervals::interval_intersection(irc, iaf)
  
  seg <- data.frame(ifrom=int[,1], ito=int[,2])
  seg$pfrom  <- res.rc$pos[seg$ifrom]
  seg$pto    <- res.rc$pos[seg$ito]
  seg$weight <- seg$ito - seg$ifrom + 1
  
  # compute mean and sd on each segment
  lx.out("computing mean and sd onsegments", with.mem=T)
  
  ind <- unlist(intervals::interval_overlap(int, irc))
  seg$mean.rc <- res.rc$seg[ind,]$mean
  seg$hmm.rc  <- res.rc$seg[ind,]$stval
  seg$sd.rc   <- res.rc$seg[ind,]$sd
  
  ind <- unlist(intervals::interval_overlap(int, iaf))
  seg$mean.af <- res.af$seg[ind,]$mean + 0.5
  seg$hmm.af  <- res.af$seg[ind,]$stval + 0.5
  seg$sd.af   <- res.af$seg[ind,]$sd
  
  if (nrow(seg) != 0) {
    minseg.size <- min(rc.minseg.size, af.minseg.size)
    if (all((seg$ito - seg$ifrom + 1) < minseg.size)) {
      lx.warn("minseg.size too high (will yield no segment), disabling last filter")
    } else {
      seg <- seg[(seg$ito - seg$ifrom + 1) >= minseg.size,,drop=F]
    }
  }

  # results
  lx.out("found ", nrow(seg), " segments", with.mem=T)
  list(seg=seg, res.rc=res.rc, res.af=res.af)
}



