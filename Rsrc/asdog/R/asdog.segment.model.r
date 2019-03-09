# -------------------------------------------------
# $Id: asdog.segment.model.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# CN & AF final segmentation
# bivariate normal (CN) & binormal (AF) model
#

# -------------------------------------------------
# <internal> make complex number
#
.c <- function(x,y=x) complex(real=x, imaginary=y)

# -------------------------------------------------
# <internal> binormal distribution
#
.dbinorm <- function(x, mean=0, sd=1, log=FALSE) {
  unlist(mapply(function(m, s) {
    pp <- dnorm(x,  m, s, log=log)
    pm <- dnorm(x, -m, s, log=log)
    if (log) {
      # compute log(exp(pp)+exp(pm)) without overflow
      mp <- cbind(pp, pm)
      mc <- max.col(mp)                   # max in rows, quicker than
      mc <- mp[cbind(seq_along(mc), mc)]  # apply(mp, 1, max)
      mc + log(rowSums(exp(mp-mc)))
    } else {
      pp + pm
    }
  }, mean, sd, SIMPLIFY=F), use.names=F)
}

# -------------------------------------------------
# <internal> binormal random generator
#
.rbinorm <- function(n, mean=1, sd=1) {
  .cycle <- function(x, k) head(rep(x, k), k)
  m <- max(n, length(mean), length(sd))
  mean <- .cycle(mean, m) * sample(c(-1,1), m, replace=T)
  rnorm(n, mean=mean, sd=sd)
}

# -------------------------------------------------
# distribution: bivariate normal + binormal (with 0 covariance)
#               implemented with complex numbers
# note: af is 0 centered
#
drcaf.asdog <- function(x, mean=.c(0), sd=.c(1), log=FALSE) {
  rc <- dnorm(Re(x), mean=Re(mean), sd=Re(sd), log=log)
  af <- .dbinorm(Im(x), mean=Im(mean), sd=Im(sd), log=log)
  if (log) rc+af else rc*af
}

# -------------------------------------------------
# random generator: bivariate normal + binormal (with 0 covariance)
#               implemented with complex numbers
# note: af is 0 centered
#
rrcaf.asdog <- function(n, mean=.c(0), sd=.c(1)) {
  rc <- rnorm(n, mean=Re(mean), sd=Re(sd))
  af <- .rbinorm(n, mean=Im(mean), sd=Im(sd))
  .c(rc, af)
}

