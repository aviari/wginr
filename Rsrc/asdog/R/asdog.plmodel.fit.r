# -------------------------------------------------
# $Id: asdog.plmodel.fit.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# Ploidy model : fit RCAF model to data
#
# penalty model:
#
#  score = sum(i:=obs, score_i)
#
#  score_i = min(j:=theo, dist_i_j) / min(k:=theo!=j, dist_i_k)^rho
#
#  dist_i_j = (x_i - x_j)^gamma
#
#  gamma = 2 - gamma0 * max(q / max.Q, a / max.A)
#

# ---------------------------------
# fit model to obs. data

plmodel.fit <- function(obs,
                        min.A=0, max.A=1, step.A=0.05,
                        min.Q=1, max.Q=8, step.Q=0.05,
                        theo.weight=FALSE, 
                        gamma0=1, rho=1,
                        use.threads=lx.use.threads()) {

  #
  # internal utilities
  #
  
  # distance fit
  #
  .dist.fit <- function(obs, theo, gamma, rho) {
    .abs  <- function(x) min(1, abs(x))
    .dist <- function(x0, y0, x1, y1, gamma) (.abs(x0-x1)^gamma + .abs(y0-y1)^gamma)
    .dmin <- function(d, w, w0) { i <- which.min(d); d[i] * w[i] * w0 }
    mapply(function(x0, y0, w0) {
      d <- mapply(function(xt, yt) .dist(x0, y0, xt, yt, gamma),
                  theo$rc, theo$af, SIMPLIFY=TRUE, USE.NAMES=F)
      dmin <- .dmin(d, theo$weight, w0)
      if (dmin == 0) return(0)
      d[which.min(d)] <- Inf
      dmin <- dmin / .dmin(d, theo$weight, w0)^rho
    }, obs$rc, obs$af, obs$weight, SIMPLIFY=T, USE.NAMES=F)
  }

  # model fit
  #
  
  # <internal> model fit
  #
  .model.fit <- function(obs, theo, gamma, rho, as.log=T) {
    rss <- sum(.dist.fit(obs, theo, gamma, rho))
    n   <- length(obs$rc)
    if (as.log) -log(rss/n) else rss/n
  }
  
  #
  # body
  #
  
  max.rc <- max(obs$rc)
  aseq <- seq(min.A, max.A, by=step.A)
  qseq <- seq(min.Q, max.Q, by=step.Q)
  
  fit <- lx.lapply(qseq, function(q) {
    sapply(aseq, function(a) {
      #
      # we should generate enough theoretical points in the grid
      # to cover all observed points. this is virtually infinite
      # (when alpha -> 1) so we limit to a reasonable number
      # with qmax=20.
      qmax <- max(2, min(20, ceiling(asdog.absCN(max.rc, a, q))))
      theo <- asdog.theo.RCAF(a, q, qrange=0:qmax, with.weight=theo.weight)
      #
      # adjust gamma
      gamma <- 2 - gamma0 * max(q / max.Q, a / max.A)
      #
      # perform fit
      .model.fit(obs, theo, gamma=gamma, rho=rho)
    })
  }, use.threads=use.threads)

  fit <- sapply(fit, identity)
  colnames(fit) <- qseq
  rownames(fit) <- aseq
  
  fmax <- which(fit==max(fit, na.rm=T), arr.ind=T)
  #
  # ties are unlikely except for artificial data...
  # just in case, we choose lower q then lower a
  #
  fmax <- fmax[order(fmax[,2], fmax[,1]),,drop=F]
  a0 <- aseq[fmax[1,1]]
  q0 <- qseq[fmax[1,2]]

  #
  # fit quality = first / (first + second) maxima
  #
  .qual <- function(fit, da=0.10, dq=0.25) {
    .bnd <- function(seq, i, d) which(abs(seq-seq[i]) <= d)
    i0 <- which(fit==max(fit), arr.ind=T)
    s0 <- sum(fit[.bnd(aseq, i0[1,1], da), .bnd(qseq, i0[1,2], dq)])
    fit[.bnd(aseq, i0[1,1], da), .bnd(qseq, i0[1,2], dq)] <- min(fit)
    i1 <- which(fit==max(fit), arr.ind=T)
    s1 <- sum(fit[.bnd(aseq, i1[1,1], da), .bnd(qseq, i1[1,2], dq)])
    s0/(s0+s1)
  }
  qual <- .qual(exp(fit))

  list(obs=obs, fit=fit, 
       a0=a0, q0=q0,
       qual=qual)
}
