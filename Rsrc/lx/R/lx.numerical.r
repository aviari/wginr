# -------------------------------------------------
# $Id: lx.numerical.r 396 2019-01-02 22:53:10Z viari $
# lx numerical analysis utilities:
#    smoothing, curve fitting, function inversion
#

# -------------------------------------------------
#' smooth data using local polynomial regression
#' @description
#' this is a wrapper around \link{loess}
#' @param x vector of abscissa if y != NULL or time series values if y == NULL
#' @param y vector of values
#' @param span parameter which controls the degree of smoothing (see \link{loess})
#' @param ... other parameters to \link{loess}
#' @return named list with following fields\cr
#' x : the original abscissa (see Details)\cr
#' y : the smoothed values\cr
#' loess : raw result from \link{loess}\cr
#' @details
#' if just x is provided (i.e. y == NULL) then use x as values and seq_along(x)
#' as abscissa.
#' @seealso \link{loess}
#' @examples
#' x <- hist(rnorm(5000), breaks="fd", plot=FALSE)
#' lx.loess(x$mids, x$counts)
#'
lx.loess <- function(x, y=NULL, span=0.75, ...) {
  
  if (is.null(y)) {
    y <- x
    x <- seq_along(y)
  }
  
  p <- tryCatch(
    loess(y ~ x, span=span, ...),
    error=function(cond) {
      lx.warn(cond$message, up.frame=4)
      lx.warn("lx.loess failure, no smoothing performed", up.frame=4)
      NULL 
    })
  
  list(x=x, y=if(is.null(p)) y else p$fitted, loess=p)
}

# -------------------------------------------------
#' Tukey's median smoothing
#' @description
#' smooth data using iterative Tukey's median smoothing. 
#' this is typically intented for piecewise constant data (step function)
#' to smooth signal whuile preserving edges.
#' see \code{http://dx.doi.org/10.1007/BFb0057597}
#' @param x numeric vector, the \emph{dependent} variable to be smoothed
#' @param k vector of successive smoothing windows
#' @param tol tolerance for smoothing iterations (see details)
#' @return vector of smoothed values of the same length as x
#' @details
#' the procedure runs as follows:\cr
#' \preformatted{
#' foreach window.size in k
#'   repeat
#'     x0 <- x
#'     x  <- runmed(x0, window.size)
#'   while (rss(x0,x) < tol)
#' end
#' }
#' set \code{tol} to Inf to prevent iterations and to 0 (default)
#' for full iterations.
#' @seealso \link{lx.loess}, \link{runmed}
#' @examples
#' x <- c(rnorm(100, -1, 0.1), rnorm(100, 1, 0.1))
#' sx <- lx.smooth.median(x, c(3, 5, 15, 35))
#' \dontrun{
#' plot(x)
#' lines(sx, col=2)
#' }
#
lx.smooth.median <- function(x, k=3L, tol=0) {
  for (w in k) {
    repeat {
      x0 <- x
      x <- runmed(x0, w, endrule="constant", algorithm=NULL)
      rss <- sqrt(sum((x-x0)^2))
      if (rss <= tol) break
    }
  }
  x
}

# -------------------------------------------------
#' get maxima of a curve
#' @param x vector of numerical values
#' @param span pre-smoothing parameter (see details)
#' @param eps min ratio between highest and other reported maxima (see details)
#' @param ... other parameters to \link{loess}
#' @return vector of indices of maxima (ordered by decreasing heights)
#' @details
#' if \code{span > 0} then \code{x} is first smoothed using
#' \link{loess} with parameter \code{span}.
#' Then maxima are searched for. If \code{eps >= 0} then \code{x}
#' is expected to be a vector of positive values (typically a distribution) and only maxima
#' with height >= eps * max(height) are returned. If \code{eps < 0} then all maxima
#' are returned (\code{x} can be any curve with positive and negative values)
#' @note
#' the technique used to locate extrema is quite simple (change of sign
#' of the derivative) and can easily be fooled by noisy data. this is the
#' reason why data is first smoothed. this function displays better behavior
#' on regular distributions provided by \link{lx.density}.
#' @seealso \link{lx.peaks}
#' @examples
#' x <- lx.density(c(rnorm(100, -1, 0.5), rnorm(100, 1, 0.5)))
#' x$x[lx.maxima(x$y)]
#
lx.maxima <- function(x, span=0.2, eps=0.1, ...) {
  if (span > 0)
    x <- loess(x ~ seq_along(x), span=span, ...)$fitted
  y <- diff(c(-Inf, as.numeric(x))) > 0
  y <- cumsum(rle(y)$lengths)[c(T,F)]
  if (eps >= 0) {
    z <- x[y]
    y <- y[z >= max(z) * eps]
  }
  y[order(x[y], decreasing=T)]
}

# -------------------------------------------------
#' get peaks of a curve
#' @description
#' get peaks position and width of a curve
#' @param x vector of numerical values (typically a distribution)
#' @param span pre-smoothing parameter (see details)
#' @param eps.x eps parameter for x maxima (see details)
#' @param eps.dx eps parameter for x derivative maxima (see details)
#' @param na.val default width (see details)  
#' @param ... other parameters to \link{loess}
#' @return list of three components:\cr
#' \preformatted{
#'  - pos   :  vector of indices of peaks position (ordered by decreasing heights)
#'  - left  :  vector of indices of peaks left side
#'  - right :  vector of indices of peaks right side
#' }
#' @details
#' if \code{span > 0} then \code{x} is first smoothed using
#' \link{loess} with parameter \code{span}.
#' Then, maxima of x are search with \link{lx.maxima} using eps.x parameter.
#' Then, foreach maximum, the closest maxima of x derivative are searched
#' for to compute left and right peak boundaries. if no boundary is found
#' then value is set to maximum.pos +/- \code{na.val}.
#' @note
#' if x is a normal distribution then band width is an estimate of 2*sd
#' @note
#' the technique used to locate extrema is quite simple (change of sign
#' of the derivative) and can easily be fooled by noisy data. this is the
#' reason why data is first smoothed. this function displays better behavior
#' on regular distributions provided by \link{lx.density}.
#' @seealso \link{lx.maxima}
#' @examples
#' x <- lx.density(c(rnorm(100, -1, 0.3), rnorm(100, 1, 0.3)))
#' p <- lx.peaks(x$y)
#' \dontrun{
#'   plot(x)
#'   abline(v=x$x[p$pos], col=1)
#'   abline(v=x$x[c(p$left, p$right)], col=2)
#' }
#
lx.peaks <- function(x, span=0.2, eps.x=0.1, eps.dx=eps.x, na.val=NA, ...) {
  .inrange <- function(z) pmin(length(x), pmax(1, z))
  if (span > 0)
    x <- loess(x ~ seq_along(x), span=span, ...)$fitted
  xm <- lx.maxima(x, span=0, eps=eps.x)
  dxm <- lx.maxima(abs(diff(x)), span=0, eps=eps.dx)
  xl <- sapply(xm, function(xm) {
    y <- xm - dxm
    ifelse(any(y<0), xm+y[y<0][which.max(y[y<0])], xm-na.val)
  })
  xr <- sapply(xm, function(xm) {
    y <- xm - dxm
    ifelse(any(y>0), xm+y[y>0][which.min(y[y>0])], xm+na.val)
  })
  list(pos=.inrange(xm), left=.inrange(xl), right=.inrange(xr))
}

# -------------------------------------------------
#' One dimensional Root (Zero) finding
#' @description
#' look for a zero (root) of a continuous monotonic function 
#' by dichotomic search\cr
#' i.e. return value x such that \code{fun(x) = 0}.\cr
#' this function is similar to \link{uniroot} but additionally
#' returns a lower and upper bound of root (see details).
#' @param fun monotonic function on interval xmin, xmax
#' @param xmin minimum value of x
#' @param xmax maximum value of x
#' @param xtol tolerance on x (see details)
#' @param ytol tolerance on y=0 (see details)
#' @param maxiter max number of iterations (see details)
#' @param with.interval logical, provide lower and upper bounds for root
#' @param ... additional arguments to fun
#' @param .errorBound logical, raise error if zero is not between xmin and xmax
#'        boundaries.
#' @return
#' a named list with 4 components:
#' \itemize{
#' \item root : the root value
#' \item lower: root lower bound (see details)
#' \item upper: root upper bound (see details)
#' \item niter: number of iterations
#' }
#' @details \code{fun} should be monotonic (either increasing or decreasing)
#' if not, the result will (probably) be wrong. Since the zero must be
#' located between \code{xmin} and \code{xmax},
#' \code{fun(xmin)} and \code{fun(xmax)} should be
#' of opposite sign else an error is raised, unless \code{.errorBound = FALSE}
#' in which case the closest boundary is returned (and niter is set to \code{NA})
#' 
#' the algorithm proceeds by dichotomic search (bissection) and convergence
#' is achieved when at least one of the following criterion is satisfied:\cr
#' \itemize{
#' \item \code{fun(x) = 0 +/- ytol}
#' \item the change in x for one step is less than \code{xtol}
#' \item the maximum number of iterations is reached
#' }
#' if \code{with.interval == TRUE} then a lower and upper bound for root
#' are additionally provided. Unless the maximum
#' number of iterations has been reached, the result guarantees that
#' \code{[lower,upper]} is the largest interval such that \code{for all x in
#' [lower+/-xtol, upper+/-xtol] fun(x)=0 +/- ytol}\cr
#' @note
#' the algorithm is far from beeing optimal and is usually slower than
#' \link{uniroot}. So, if you don't care about lower and upper bounds, you
#' better have to use to \link{uniroot} instead.\cr
#' 
#' if zero is reached at xmin (resp. xmax) boundary, then the lower
#' (resp. upper) bound is truncated.\cr
#' 
#' either \code{xmin} or \code{xmax} may be a pole 
#' (i.e. \code{fun(x) = Inf}) but not both.\cr
#' 
#' there is no need to set \code{xtol} or \code{ytol} below 
#' \code{.Machine$double.eps}.
#' @examples
#' # simple functions
#' lx.zero(function(x) x^2 - 0.2^2, ytol=1e-3)
#' lx.zero(function(x) log(x) - log(0.3), ytol=1e-3)
#' lx.zero(function(x) x - pi, xmax=10, ytol=1e-3)
#' lx.zero(function(x) pi - x, xmax=10, ytol=1e-3)
#' # out of bounds
#' tryCatch(lx.zero(function(x) x - 2), error=function(e) NA)
#' lx.zero(function(x) x - 2, .errorBound=FALSE)
#' # interval 
#' lx.zero(function(x) x^5, ytol=1e-3) # interval is truncated
#' lx.zero(function(x) x^5, ytol=1e-3, xmin=-1)
#' # poles
#' lx.zero(function(x) (x-0.5)/x/(2-x))
#
lx.zero <- function(fun, xmin=0, xmax=1,
                    xtol=10*.Machine$double.eps,
                    ytol=10*.Machine$double.eps,
                    maxiter=1e4L,
                    with.interval=TRUE,
                    ..., .errorBound=TRUE) {
  # <internal> stop message
  .msg <- function(msg) paste0(msg, " interval [",xmin, ",", xmax, "]")
  
  # <internal> check solution
  .is.zero <- function(fun, x) abs(fun(x, ...)) <= ytol
  
  # <internal> solver
  .solve <- function(fun, xmin, xmax, niter=0) {
    while (niter < maxiter) {
      niter <- niter + 1
      xmid <- (xmin+xmax) / 2
      ymid <- fun(xmid, ...)
      if (.is.zero(fun, xmid) || (xmax-xmin <= 2*xtol))
        break
      if (ymid > 0) 
        xmax <- xmid
      else
        xmin <- xmid
    }
    if (niter >= maxiter)
      warning("max iterations (", maxiter, ") reached")
    
    list(root=xmid, xmin=xmin, xmax=xmax, niter=niter)
  }
  
  # force strictly positive tolerance
  ytol <- max(abs(ytol), .Machine$double.eps)
  xtol <- max(abs(xtol), .Machine$double.eps)
  
  # check function sign and reverse sign if necessary
  ymin <- fun(xmin, ...)
  ymax <- fun(xmax, ...)
  if (is.na(ymin) || is.na(ymax)) stop(.msg("improper"))
  if (is.infinite(ymin) && is.infinite(ymax)) stop(.msg("end poles in"))
  if (ymin > ymax) {
    .fun <- fun
    fun <- function(x, ...) -.fun(x, ...)
    ymin <- -ymin; ymax <- -ymax
  }
  
  # check out sign change in interval - allow for tolerance
  if (ymin > ytol) {
    if (.errorBound) stop(.msg("no sign change in"))
    else return(list(root=xmin, lower=NA, upper=NA, niter=NA))
  }
  if (ymax < -ytol) {
    if (.errorBound) stop(.msg("no sign change in"))
    else return(list(root=xmax, lower=NA, upper=NA, niter=NA))
  }
  
  # call zero-solver
  s0 <- .solve(fun, xmin, xmax)
  
  # no interval requested or no convergence
  if ((! with.interval) || (! .is.zero(fun, s0$root)))
    return(list(root=s0$root, lower=NA, upper=NA, niter=s0$niter))
  
  # left interval
  left  <- s0$xmin
  delta <- s0$root - s0$xmin + xtol
  while (.is.zero(fun, left) && (left > xmin)) {
    left <- left - delta
    delta <- delta * 2
  }
  left <- max(left, xmin)
  lower <- s0$root
  cur   <- .solve(fun, left, lower)$root
  while (.is.zero(fun, cur) && ((lower-cur) > xtol)) {
    lower <- cur
    cur   <- .solve(fun, left, cur)$root
  }
  
  # right interval
  right <- s0$xmax
  delta <- s0$xmax - s0$root + xtol
  while (.is.zero(fun, right) && (right < xmax)) {
    right <- right + delta
    delta <- delta * 2
  }
  right <- min(right, xmax)
  upper <- s0$root
  cur   <- .solve(fun, upper, right)$root
  while (.is.zero(fun, cur) && ((cur-upper) > xtol)) {
    upper <- cur
    cur   <- .solve(fun, cur, right)$root
  }
  
  # root
  root <- (lower+upper)/2
  
  list(root=root, lower=lower, upper=upper, niter=s0$niter)
}
