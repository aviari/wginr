# -------------------------------------------------
# $Id: lx.backports.r 396 2019-01-02 22:53:10Z viari $
# lx backports for R <= 3.2
#

# -------------------------------------------------
#' backport of get0 for R < 3.2.0
#' @description
#' Look for an R object of the given name and possibly return it
#' @param x a variable name (given as a character string).
#' @param envir an environment to look in.
#' @param mode the mode or type of object sought.
#' @param inherits should the enclosing frames of the environment be searched?
#' @param ifnotfound	the return value when x does not exist.
#' @return The object - as from \link{get}(x, *) - if \link{exists}(x, *)
#' is true, otherwise \code{ifnotfound}
#' @seealso \link{get0}
#' @note code from \url{https://github.com/cran/backports/blob/master/R/get0.R}
#' 

lx.get0 <- function(x, envir=pos.to.env(-1L), mode="any", inherits=TRUE, ifnotfound=NULL) {
  mget(x[1L], envir=envir, mode=mode, inherits=inherits,
       ifnotfound=list(ifnotfound))[[1L]]
}
