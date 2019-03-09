# -------------------------------------------------
# $Id: lx.statistics.r 396 2019-01-02 22:53:10Z viari $
# lx statistics utilities
#

# -------------------------------------------------
#' save current global random seed
#' @description
#' push current global random seed (.Random.seed) into internal stack
#' this has to be used (in pairs with \link{lx.rnd.pop}) to
#' save current random seed and restore it later
#' @param seed \bold{optional} parameter, if specified then additionnaly
#' \link{set.seed}(seed)
#' @return (invisible) current random seed
#' @examples
#' set.seed(1)
#' x <- sample(letters, 3)
#' set.seed(1)
#' lx.rnd.push() # same as
#' set.seed(2)   # lx.rnd.push(2)
#' y <- sample(letters, 3)
#' lx.rnd.pop()
#' z <- sample(letters, 3)
#' identical(x, z)
#
lx.rnd.push <- function(seed) {
  rnd <- get(".Random.seed", envir=.GlobalEnv)
  if (is.null(.lx.env$rnd.stack))
    .lx.env$rnd.stack <- lx.stack.new()
  assign("rnd.stack", lx.stack.push(.lx.env$rnd.stack, rnd), envir=.lx.env)
  if (! missing(seed)) set.seed(seed)
  invisible(rnd)
}

# -------------------------------------------------
#' restore current global random seed
#' @description
#' pop current global random seed (.Random.seed) from internal stack
#' this has to be used (in pairs with \link{lx.rnd.push}) to
#' save current random seed and restore it later
#' @return (invisible) restored random seed
#' @examples
#' set.seed(1)
#' x <- sample(letters, 3)
#' set.seed(1)
#' lx.rnd.push() # same as
#' set.seed(2)   # lx.rnd.push(2)
#' y <- sample(letters, 3)
#' lx.rnd.pop()
#' z <- sample(letters, 3)
#' identical(x, z)
#
lx.rnd.pop <- function() {
  lx.stopif(lx.stack.is.empty(.lx.env$rnd.stack), "unbalanced push/pop")
  assign("rnd.stack", lx.stack.pop(.lx.env$rnd.stack), envir=.lx.env)
  rnd <- lx.stack.value(.lx.env$rnd.stack)
  assign(".Random.seed", rnd, envir=.GlobalEnv)
  invisible(rnd)
}

# -------------------------------------------------
#' base::sample wrapper
#' @description
#' this function calls \code{base::sample(x, size, prob, replace)} except
#' for the case where \code{prob != NULL and replace == FALSE}. In this
#' case \code{base::sample} is very slow (actually quadratic in the size of
#' \code{x} and of \code{size}. This is replaced by the 'reservoir' technique
#' of Efraimidis & Spirakis (see note)
#' @param x Either a vector of one or more elements from which to choose,
#'        or a positive integer. (see \link{sample})
#' @param size a non-negative integer giving the number of items to choose.
#' @param replace should sampling be with replacement
#' @param prob a vector of probability weights for obtaining the elements
#'        of the vector being sampled.
#' @return a vector of length size with elements drawn from either x
#' or from the integers 1:x
#' 
#' @note
#' \bold{reference}: P. Efraimidis and P.Spirakis, Information Processing Letters,
#' 97, 181-185 (2006).
#'
lx.sample <- function(x, size, replace=FALSE, prob=NULL) {
  
  #    
  # case: replace=TRUE or prob=NULL
  #
  
  if (replace || is.null(prob))
    return (base::sample(x, size, replace=replace, prob=prob))
  
  #    
  # case: replace=FALSE and prob!=NULL
  #
  
  # integer case
  #
  if (length(x) == 1L) x <- seq.int(x)
  
  if (missing(size)) size <- length(x)
  
  lx.stopif(length(prob) != length(x),
            "prob and x must be of the same size")
  
  # remove <= 0 weight probabilities
  #
  valid <- (prob > 0)
  x    <- x[valid]
  prob <- prob[valid]
  valid <- NULL
  
  # sanity check
  #
  lx.stopif(length(x) < size,
            "not enough data points to sample without replacement")
  
  # Efraimidis & Spirakis algo
  # [improved] line 2: we don't need to reorder everything
  #
  key <- runif(length(x)) ^ (1 / prob)
  x[order(key, decreasing=TRUE)][1:size]  
}

# -------------------------------------------------
#' weighted mean
#' @param x a numeric vector
#' @param w a numeric vector of (positive) weights
#' @param na.rm a logical value indicating whether NA values should
#' be stripped before the computation proceeds
#' @return weighted mean of x
#' @examples
#' lx.wt.mean(1:2, 1:2)
#'
lx.wt.mean <- function (x, w=NULL, na.rm=TRUE) {
  if (length(w) == 0)
    return(mean(x, na.rm=na.rm))
  if (na.rm) {
    s <- ! is.na(x + w)
    x <- x[s]
    w <- w[s]
  }
  sum(w * x)/sum(w)
}

# -------------------------------------------------
#' weighted variance
#' @param x a numeric vector
#' @param w a numeric vector of (positive) weights
#' @param as.repeat a logical value indicating whether weights
#' should be interpreted as repeated measures or not (see details).
#' @param na.rm a logical value indicating whether NA values should
#' be stripped before the computation proceeds
#' @details
#' there is no single accepted formula for the weighted variance. It depends
#' whether weights should be interpreted as repeats of the same measure (in which
#' case they usually are integers summing up to the total number of measures) 
#' (\code{as.repeat=TRUE}) or if thet should be interpreted as a 'confidence'
#' attached to each measure (\code{as.repeat=FALSE}).\cr
#' @return weighted variance of x
#' @examples
#' lx.wt.var(1:2, 1:2)
#' var(c(1, 2, 2)) # should be the same as previous
#' lx.wt.var(1:2, 1:2, as.repeat=FALSE) # but this is different
#' lx.wt.var(1:2, rep(1,2))
#' var(1:2) # should be the same as previous
#'
lx.wt.var <- function (x, w=NULL, as.repeat=TRUE, na.rm=TRUE) {
  if (length(w) == 0)
    return(var(x, na.rm=na.rm))
  if (na.rm) {
    s <- ! is.na(x + w)
    x <- x[s]
    w <- w[s]
  }
  if (length(w) <= 1)
    return(NA)
  xm <- sum(w * x)/sum(w)
  if (as.repeat)
    sum(w * (x - xm)^2) / (sum(w) - 1)
  else
    sum(w * (x - xm)^2) * (sum(w)/(sum(w)^2 - sum(w^2)))
}

# -------------------------------------------------
#' Kernel Density Estimation
#' @description
#' this is a wrapper around \link{density} that workarounds a (known) bug
#' with bw="SJ".  The density function generates an error when bw="SJ"
#' and length(x) > 46341. The message is:\cr
#' \code{Error in bw.SJ(x, method = "ste") : sample is too sparse to find TD}\cr
#' the workaround consists in sampling 46341 values. 
#' @note the seed for sampling is always the same and therefore
#' the procedure remains deterministic.
#' @param x the data from which the estimate is to be computed.
#' @param bw the smoothing bandwidth to be used (see \link{density})
#' @param ... any other parameter to \link{density}
#' @return an object of class "density" (see \link{density})
#' @examples
#' \dontrun{
#' x <- lx.density(rnorm(50000), "SJ")
#' }
#' 
lx.density <- function(x, bw="nrd0", ...) {
  .MAGIC.SJ = 46341L
  if ((bw %in% "SJ") && (length(x) > .MAGIC.SJ)) {
    lx.rnd.push()
    set.seed(0)
    x <- sample(x, size=.MAGIC.SJ, replace=FALSE)
    lx.rnd.pop()
  }
  density(x, bw=bw, ...)
}


# -------------------------------------------------
# -------------------------------------------------
# reparametrized standard distributions
# -------------------------------------------------
# -------------------------------------------------

# -------------------------------------------------
#' Normal distribution
#' @description
#' These functions are equivalent to \link{dnorm}, \link{pnorm} and \link{rnorm}.
#' they are just provided for symetry with other re-parametrized distributions.
#' @param x vector of quantiles
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @param n number of observations
#' @param ... other parameters to \link{dnorm}, \link{pnorm} and \link{rnorm}
#' @return \bold{lx.prm.norm} returns the standard parameters
#' of R library functions.
#' \bold{lx.dnorm} gives the density, \bold{lx.pnorm} gives the distribution function
#' and \bold{lx.rnorm} generates random deviates. 
#' @seealso \link{dnorm}, \link{pnorm} and \link{rnorm}
#' 
lx.prm.norm <- function(mean, sd) list(mean=mean, sd=sd)
#' @describeIn lx.prm.norm density function
lx.dnorm <- function(x, mean=0, sd=1, ...) do.call(dnorm, c(list(x=x), lx.prm.norm(mean, sd), list(...)))
#' @describeIn lx.prm.norm distribution function
lx.pnorm <- function(x, mean=0, sd=1, ...) do.call(pnorm, c(list(x=x), lx.prm.norm(mean, sd), list(...)))
#' @describeIn lx.prm.norm random deviate
lx.rnorm <- function(n, mean=0, sd=1, ...) do.call(rnorm, c(list(n=n), lx.prm.norm(mean, sd), list(...)))

# -------------------------------------------------
#' Poisson distribution
#' @description
#' These functions are equivalent to \link{dpois}, \link{ppois} and \link{rpois}.
#' they are just provided for symetry with other re-parametrized distributions.
#' @param x vector of quantiles
#' @param mean vector of (non-negative) means
#' @param n number of observations
#' @param ... other parameters to \link{dpois}, \link{ppois} and \link{rpois}
#' @return \bold{lx.prm.pois} returns the standard parameters
#' of R library functions.
#' \bold{lx.dpois} gives the density, \bold{lx.ppois} gives the distribution function
#' and \bold{lx.rpois} generates random deviates. 
#' @seealso \link{dpois}, \link{ppois} and \link{rpois}
#'
lx.prm.pois <- function(mean) list(lambda=mean)
#' @describeIn lx.prm.pois density function
lx.dpois <- function(x, mean, ...) do.call(dpois, c(list(x=x), lx.prm.pois(mean), list(...)))
#' @describeIn lx.prm.pois distribution function
lx.ppois <- function(x, mean, ...) do.call(ppois, c(list(x=x), lx.prm.pois(mean), list(...)))
#' @describeIn lx.prm.pois random deviate
lx.rpois <- function(n, mean, ...) do.call(rpois, c(list(n=n), lx.prm.pois(mean), list(...)))

# -------------------------------------------------
#' Re-parameterized Beta distribution
#' @description
#' Beta distibution reparameterized with mean and variance
#' @param x vector of quantiles
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @param n number of observations
#' @param ... other parameters to \link{dbeta}, \link{pbeta} and \link{rbeta}
#' @return \bold{lx.prm.beta} returns the standard parameters
#' of R library functions.
#' \bold{lx.dbeta} gives the density, \bold{lx.pbeta} gives the distribution function
#' and \bold{lx.rbeta} generates random deviates. 
#' @seealso \link{dbeta}, \link{pbeta} and \link{rbeta}
#'
lx.prm.beta <- function(mean, sd) list(shape1=(1-mean)*mean*mean/sd/sd - mean,
                                       shape2=(1-mean)*(1-mean)*mean/sd/sd - (1-mean))
#' @describeIn lx.prm.beta density function
lx.dbeta <- function(x, mean, sd, ...) do.call(dbeta, c(list(x=x), lx.prm.beta(mean, sd), list(...)))
#' @describeIn lx.prm.beta distribution function
lx.pbeta <- function(x, mean, sd, ...) do.call(pbeta, c(list(x=x), lx.prm.beta(mean, sd), list(...)))
#' @describeIn lx.prm.beta random deviate
lx.rbeta <- function(n, mean, sd, ...) do.call(rbeta, c(list(n=n), lx.prm.beta(mean, sd), list(...)))

# -------------------------------------------------
#' Re-parameterized Gamma distribution
#' @description
#' Gamma distibution reparameterized with mean and variance
#' @param x vector of quantiles
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @param n number of observations
#' @param ... other parameters to \link{dgamma}, \link{pgamma} and \link{rgamma}
#' @return \bold{lx.prm.gamma} returns the standard parameters
#' of R library functions.
#' \bold{lx.dgamma} gives the density, \bold{lx.pgamma} gives the distribution function
#' and \bold{lx.rgamma} generates random deviates. 
#' @seealso \link{dgamma}, \link{pgamma} and \link{rgamma}
#'
lx.prm.gamma <- function(mean, sd) list(shape=mean*mean/sd/sd, scale=sd*sd/mean)
#' @describeIn lx.prm.gamma density function
lx.dgamma <- function(x, mean, sd, ...) do.call(dgamma, c(list(x=x), lx.prm.gamma(mean, sd), list(...)))
#' @describeIn lx.prm.gamma distribution function
lx.pgamma <- function(x, mean, sd, ...) do.call(pgamma, c(list(x=x), lx.prm.gamma(mean, sd), list(...)))
#' @describeIn lx.prm.gamma random deviate
lx.rgamma <- function(n, mean, sd, ...) do.call(rgamma, c(list(n=n), lx.prm.gamma(mean, sd), list(...)))

# -------------------------------------------------
#' Re-parameterized Negative Binomial distribution
#' @description
#' Negative Binomial  distibution reparameterized with mean and variance
#' @param x vector of quantiles
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @param n number of observations
#' @param ... other parameters to \link{dnbinom}, \link{pnbinom} and \link{rnbinom}
#' @return \bold{lx.prm.nbinom} returns the standard parameters
#' of R library functions.
#' \bold{lx.dnbinom} gives the density, \bold{lx.pnbinom} gives the distribution function
#' and \bold{lx.rnbinom} generates random deviates. 
#' @seealso \link{dnbinom}, \link{pnbinom} and \link{rnbinom}
#'
lx.prm.nbinom <- function(mean, sd) list(size=mean*mean/(sd*sd-mean), prob=mean/sd/sd)
#' @describeIn lx.prm.nbinom density function
lx.dnbinom <- function(x, mean, sd, ...) do.call(dnbinom, c(list(x=x), lx.prm.nbinom(mean, sd), list(...)))
#' @describeIn lx.prm.nbinom distribution function
lx.pnbinom <- function(x, mean, sd, ...) do.call(pnbinom, c(list(x=x), lx.prm.nbinom(mean, sd), list(...)))
#' @describeIn lx.prm.nbinom random deviate
lx.rnbinom <- function(n, mean, sd, ...) do.call(rnbinom, c(list(n=n), lx.prm.nbinom(mean, sd), list(...)))

# -------------------------------------------------
#' Re-parameterized Binomial distribution
#' @description
#' Binomial  distibution reparameterized with mean and variance
#' @param x vector of quantiles
#' @param mean vector of means
#' @param sd vector of standard deviations
#' @param n number of observations
#' @param ... other parameters to \link{dbinom}, \link{pbinom} and \link{rbinom}
#' @return \bold{lx.prm.binom} returns the standard parameters
#' of R library functions.
#' \bold{lx.dbinom} gives the density, \bold{lx.pbinom} gives the distribution function
#' and \bold{lx.rbinom} generates random deviates. 
#' @seealso \link{dbinom}, \link{pbinom} and \link{rbinom}
#'
lx.prm.binom <- function(mean, sd) list(size=mean*mean/(mean - sd*sd), prob=1-sd*sd/mean)
#' @describeIn lx.prm.binom density function
lx.dbinom <- function(x, mean, sd, ...) do.call(dbinom, c(list(x=x), lx.prm.binom(mean, sd), list(...)))
#' @describeIn lx.prm.binom distribution function
lx.pbinom <- function(x, mean, sd, ...) do.call(pbinom, c(list(x=x), lx.prm.binom(mean, sd), list(...)))
#' @describeIn lx.prm.binom random deviate
lx.rbinom <- function(n, mean, sd, ...) do.call(rbinom, c(list(n=n), lx.prm.binom(mean, sd), list(...)))

# -------------------------------------------------
#' Re-parameterized Exponential distribution
#' @description
#' Exponential  distibution reparameterized with mean
#' @param x vector of quantiles
#' @param mean vector of means
#' @param n number of observations
#' @param ... other parameters to \link{dexp}, \link{pexp} and \link{rexp}
#' @return \bold{lx.prm.exp} returns the standard parameters
#' of R library functions.
#' \bold{lx.dexp} gives the density, \bold{lx.pexp} gives the distribution function
#' and \bold{lx.rexp} generates random deviates. 
#' @seealso \link{dexp}, \link{pexp} and \link{rexp}
#'
lx.prm.exp <- function(mean=1) list(rate=1/mean)
#' @describeIn lx.prm.exp density function
lx.dexp <- function(x, mean=1, ...) do.call(dexp, c(list(x=x), lx.prm.exp(mean), list(...)))
#' @describeIn lx.prm.exp distribution function
lx.pexp <- function(x, mean=1, ...) do.call(pexp, c(list(x=x), lx.prm.exp(mean), list(...)))
#' @describeIn lx.prm.exp random deviate
lx.rexp <- function(n, mean=1, ...) do.call(rexp, c(list(n=n), lx.prm.exp(mean), list(...)))


