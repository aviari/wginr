# -------------------------------------------------
# $Id: xlx.utils.r 321 2017-08-18 11:10:19Z viari $
# XLX utilities
#

# =================================================
# API
# =================================================

# -------------------------------------------------
#' get bitfield of pattern matches in sequence
#' @param seq sequence string
#' @param pat pattern to match
#' @param regex pattern \code{pat} is a regular expression (see details)
#' @return bitfield (see package bit) of same size as sequence where
#' TRUE's indicate start positions of pattern.
#' @details
#' if \code{regex==FALSE} then match any char in string \code{pat}
#' @examples
#' seq <- "ACGTACGTAC"
#' x <- patbits(seq, "GC")
#' bit::as.which(x)
#' sum(x)
#' x <- patbits(seq, "[GC]", regex=TRUE)
#' x <- patbits(seq, "TAC", regex=TRUE)
#'
patbits <- function(seq, pat, regex=FALSE) {
  match <- bit::bit(nchar(seq))
  match[if(regex) lx.gregexpr(pat, seq) else lx.strchr(seq, pat)] <- TRUE
  match
}

# -------------------------------------------------
#' smooth data using local polynomial regression
#' @description
#' this is an alias of \link{lx.loess}
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
#' smooth.loess(x$mids, x$counts)
#'
smooth.loess <- function(x, y=NULL, span=0.75, ...) {
  lx.loess(x=x, y=y, span=span, ...)
}

# -------------------------------------------------
#' smooth data using Kalman filter
#' @description
#' smooth data using Kalman filter with SSMtrend model
#' @param x vector of equally spaced data (time series)
#' @param f parameter which controls the degree of smoothing (see details)
#' @param ... other parameters to \link[KFAS]{KFS}
#' @return named list with following fields\cr
#' x : the smoothed values\cr
#' kfs : the detailled kalman filter results (see \link[KFAS]{KFS})
#' @details
#' the model used is \code{KFAS::SSMtrend} of degree 1 (local level) and Q (variance)
#' equals to \code{f} parameter. Namely \code{KFAS::SSMtrend(1, Q=list(matrix(f)))}
#' @note
#' will force load of package KFAS (if available) because of a nasty bug in KFAS::SSMtrend
#' which prevents from using namespace.
#' 
#' @seealso \link[KFAS]{SSModel}, \link[KFAS]{fitSSM}, \link[KFAS]{KFS}
#'
smooth.kalman <- function(x, f=1, ...) {

  kfs <- tryCatch({
      if (! lx.require("KFAS", install=FALSE, load=TRUE, silent=TRUE))
        stop("package KFAS not installed")
      ssModel <- KFAS::SSModel(x~SSMtrend(degree=1, Q=list(matrix(max(0,f)))))
      xvar    <- 0.5 * log(var(x))
      ssFit   <- KFAS::fitSSM(inits=c(xvar, xvar), model=ssModel)
      kfs     <- KFAS::KFS(ssFit$model, smoothing="state", ...) # nsim=length(x) ?
    },
    error=function(cond) {
      lx.warn(cond$message, up.frame=4)
      lx.warn("xlx.kalman failure, no smoothing performed", up.frame=4)
      NULL
    })

  list(x=if(is.null(kfs)) x else kfs$alphahat, kfs=kfs)
}

