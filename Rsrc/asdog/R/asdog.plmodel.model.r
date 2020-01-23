# -------------------------------------------------
# $Id: asdog.plmodel.model.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# CN & AF contamination models
#
#
# -------------------------------------------------
# CN contamination model
# -------------------------------------------------
#
# there are 3 different quantities
#
# rRC: relative read count 
#   is the ratio of the read count at given position
#   by a *reference* count. the reference count can be
#      a) the mean/median of read counts on whole genome
#      b) the read count of a reference peak in whole
#         genome distribution of read counts
#      c) expected read count given by a GC correction model
#         (which can itself be computed on a reference peak
#          of the whole genome distribution of read counts)
#
# rCN: relative tumoral copy number
#   is the number of copies of the tumoral genome
#   relative to the number of copy of the *reference*
#
# aCN: absolute tumoral copy number
#   is the absolute number of copies (i.e. the local ploidy)
#   of the tumoral genome.
#
# here are some examples:
# 1) no contamination, diploid tumoral genome
# rRC = 1   ; aCN = 2 ; rCN = 1     # diploid ref
# rRC = 0.5 ; aCN = 1 ; rCN = 0.5   # haploid
# rRC = 1.5 ; aCN = 3 ; rCN = 1.5   # triploid
# rRC = 2   ; aCN = 4 ; rCN = 2     # tetraploid
#
# 2) full contamination
# rRC = 1  ; aCN = x ; rCN = x      # only possible value
#
# 3) 50% contamination, diploid tumoral genome
# rRC = 1    ; aCN = 2 ; rCN = 1     # diploid ref
# rRC = 0.25 ; aCN = 1 ; rCn = 0.5   # haploid
# rRC = 1.25 ; aCN = 3 ; rCN = 1.5   # triploid
# rRC = 1.5  ; aCN = 4 ; rCN = 2     # tetraploid
#
# rRC is the quantity experimentally measured with WGS
# rCN is used to define amplification/deletion levels
# aCN is used when absolute quantities are needed
# (e.g. BFB unfolding)
#
# formula between these quantities:
#
# alpha = contamination by normal := [0,1]
# q     = local tumour ploidy = aCN
# Q     = reference tumour ploidy
#
#
# aCN = q
# rCN = q/Q
# rRC = q.(1-alpha) + 2.alpha / Q.(1-alpha) + 2.alpha
#
# -------------------------------------------------
# AF contamination model
#
# let us consider an heterozygous site in normal genome
# (i.e. with normal all. frq. = 0.5)
#
# the observed allelic frequency depends upon:
# alpha = contamination by normal := [0,1]
# q     = local tumour ploidy = aCN
# k     := [0, q] the heterozygous balance
#          k = q/2 : perfect heterozygoty
#          k=0 or q : perfect homozygoty
#
# and is given by:
#
# AF = (k.(1-alpha) + alpha) / (q.(1-alpha) + 2.alpha)
#

# ---------------------------------
# internal constants
#
.SQRT2    = sqrt(2)
.SQRT2SPI = sqrt(2/pi)   # sqrt(2/pi)
.ERRCST   = 0.674489750  # error constant = sqrt(2)*erf-1(1/2)

# ---------------------------------
# compute rRC as a function of aCN (=q)
#
# q: local tumour ploidy (=aCN)
# alpha: contamination by normal
# Q: reference tumour ploidy
#
asdog.relRC <- function(q, alpha, Q) {
  alpha <- max(min(alpha, 1), 0)
  .fun <- function(x) x*(1-alpha) + 2*alpha
  .fun(q)/.fun(Q)
}

# ---------------------------------
# compute aCN (=q) as a function of rRC
# (inverse of previous formula)
#
# rRC: relative read counts
# alpha: contamination by normal
# Q: reference tumour ploidy
# 
# note: there is a singularity at alpha=1
#
asdog.absCN <- function(rRC, alpha, Q) {
  alpha <- max(min(alpha, 0.999), 0)
  rRC*Q + 2*alpha*(rRC - 1)/(1 - alpha)
}

# ---------------------------------
# predicted allelic frequency
#
# q: local tumour ploidy (=aCN)
# alpha: contamination by normal
# k: 0:q heterozygoty
#
asdog.relAF <- function(q, alpha, k=0:q) {
  alpha <- max(min(alpha, 1), 0)
  .fun <- function(x, C) x*(1-alpha) + C*alpha
  .fun(k,1)/.fun(q,2)
}

# ---------------------------------
# theoretical RC-AF points in RC-AF diagram
# alpha: contamination by normal
# Q: reference tumour ploidy
# qrange: range of local tumour ploidy
# with.weight: use binomial weighting
# .upper: keep only AF >= 1/2
#
asdog.theo.RCAF <- function(alpha, Q, qrange=0:6, with.weight=FALSE, .upper=TRUE) {
  
  .repl <- function(c, k) paste(rep(c, k), sep="", collapse="")
  
  pt <- lapply(qrange, function(q) {
    x <- asdog.relRC(q, alpha, Q)
    y <- sapply(0:q, function(k) asdog.relAF(q, alpha, k))
    w <- if (with.weight) dbinom(0:q, q, 0.5) else rep_len(1, q+1)
    l <- sapply(0:q, function(k) paste0(.repl('A',k), .repl('B', q-k)))
    p <- rep_len(q, q+1)
    list(rc=rep_len(x,length(y)), af=y, weight=w, pl=p, label=l)
  })
  
  pt <- list(alpha=alpha, q0=Q,
             rc=unlist(sapply(pt, function(x) x$rc), use.names=F),
             af=unlist(sapply(pt, function(x) x$af), use.names=F),
             weight=unlist(sapply(pt, function(x) x$weight), use.names=F),
             ploidy=unlist(sapply(pt, function(x) x$pl), use.names=F),
             label=unlist(sapply(pt, function(x) x$label), use.names=F))
  
  if (.upper) {
    ok <- (! is.na(pt$af)) & (pt$af >= 0.5)
    pt$rc <- pt$rc[ok]
    pt$af <- pt$af[ok]
    pt$weight <- pt$weight[ok]
    pt$ploidy <- pt$ploidy[ok]
    pt$label  <- pt$label[ok]
  }
  pt
}

# -------------------------------------------------
# AF folding/unfolding formulas
# -------------------------------------------------

# -------------------------------------------------
# error function
#
.erf <- function(x) 2 * pnorm(x * .SQRT2) - 1

# -------------------------------------------------
# mean of |X|
#
asdog.mu.fold <- function(mu, sd) {
  if ((mu == 0) && (sd == 0)) return(0)
  mu * .erf(mu/.SQRT2/sd) + sd * .SQRT2SPI * exp(-(mu*mu)/2/sd/sd)
}

# -------------------------------------------------
# median of |X|
# 
asdog.md.fold <- function(mu, sd) {
  if ((mu == 0) && (sd == 0)) return(0)
  mu * .erf(mu/.SQRT2/sd) + sd * .ERRCST * exp(-(mu*mu)/2/sd/sd)
}

# -------------------------------------------------
# standard deviation of |X|
#
asdog.sd.fold <- function(mu, sd) {
  sqrt(mu^2 + sd^2 - asdog.mu.fold(mu, sd)^2)
}

# -------------------------------------------------
# unfold
# solve mu/md and sd for fold
# by recursive refinement
#
# v2.1: added max recursion to avoid stack overflow
#
asdog.unfold <- function(mu, sd, type=c("mean", "median"), mu0=mu, sd0=sd, .eps=1e-3, 
                         .maxdepth=500, .depth=0) {
  .invert <- function(fun, y0) {
    sol <- lx.zero(function(x) fun(x) - y0, 
                   xtol=.eps, ytol=.eps,
                   with.interval=TRUE,
                   .errorBound=FALSE)
    if (is.na(sol$lower)) sol$lower <- sol$root
    if (is.na(sol$upper)) sol$upper <- sol$root
    sol
  }
  
  type <- match.arg(type)
  fun.fold <- if (type %in% "mean") asdog.mu.fold else asdog.md.fold
  mu1 <- .invert(function(x) fun.fold(x, sd0), mu)$lower
  sd1 <- .invert(function(x) asdog.sd.fold(mu1, x), sd)$upper
  if (   (((abs(mu0-mu1) <= .eps) && (abs(sd0-sd1) <= .eps)))
      || (((abs(mu-mu1)  <= .eps) && (abs(sd-sd1)  <= .eps)))
      || (.depth >= .maxdepth)) {
    lx.warnif(.depth >= .maxdepth, "max recursion depth reached - no convergence achieved at ", .eps)
    pmu <- fun.fold(mu1, sd1)
    psd <- asdog.sd.fold(mu1, sd1)
    return(list(target=list(mu=mu, sd=sd),
                solution=list(mu=mu1, sd=sd1),
                actual=list(mu=pmu, sd=psd),
                error=list(mu=mu-pmu, sd=sd-psd)))
  }
  return(asdog.unfold(mu, sd, type=type, mu1, sd1, .eps=.eps,
                      .maxdepth=.maxdepth, .depth=.depth+1))
}
