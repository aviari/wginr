# -------------------------------------------------
# $Id: lx.algebra.r 396 2019-01-02 22:53:10Z viari $
# lx linear algebra utilities
#

# -------------------------------------------------
#' generalized matrix cross-product
#' @description
#' compute cross-product of conformable matrices using any function
#' of row and col.
#' @param x nxm numerical matrix
#' @param y mxn numerical matrix (if NULL then y = t(x))
#' @param FUN function called for each row and col as \code{FUN(row, col,...)}
#' @param ... other parameters to FUN
#' @return nxn numerical matrix. where entry (i,j) is \code{FUN(x[i,], y[,j],...)}
#' @note usual matrix crosproduct corresponds to \code{FUN=function(x, y) sum(x*y)}
#' (see examples)
#' @seealso \link{crossprod}
#' @examples
#' x <- matrix(1:10, ncol=2)
#' # usual crossproduct (not efficient)
#' lx.crossprod(t(x), x, function(x, y) sum(x*y))
#' # this is the same as (more efficient):
#' t(x) %*% x
#' # and the same as (more efficient):
#' crossprod(x)
#' # cross product with max
#' lx.crossprod(t(x), FUN=function(x, y) max(x*y))
#
lx.crossprod <- function(x, y=NULL, FUN, ...) {
  if (missing(y)) y <- t(x)
  res <- apply(x, 1, function(row) {
    apply(y, 2, function(col) {
      FUN(row, col, ...)
    })
  })
  res
}

# -------------------------------------------------
#' get matrix upper triangle as vector
#' @description
#' get matrix upper triangle as vector
#' @param x nxn square numerical matrix
#' @param diag logical should the diagonal be included?
#' @param bycol logical order result by columns
#' @return numerical vector of matrix upper triangle
#' @note if matrix is not square, a warning is emitted but
#' result (on the smaller dimension) is still returned
#' @seealso \link{upper.tri}, \link{lx.vect2upper}
#' @examples
#' x <- matrix(1:16, ncol=4)
#' y <- lx.upper2vect(x, diag=TRUE)
#' xx <- lx.vect2upper(y, diag=TRUE)
#
lx.upper2vect <-function(x, diag=FALSE, bycol=TRUE) {
  lx.warnif((nrow(x) != ncol(x)), "matrix should be square")
  if (bycol)
    x[upper.tri(x, diag=diag)]
  else
    t(x)[lower.tri(x, diag=diag)]
}

# -------------------------------------------------
#' make matrix from upper triangle vector
#' @description
#' this is the inverse of \link{lx.upper2vect}
#' @param x numerical vector
#' @param diag logical is the diagonal included in x? (see \link{lx.upper2vect})
#' @param bycol logical is x ordered by columns? (see \link{lx.upper2vect})
#' @param symmetrize logical should result be symmetrized
#' @param na.val numerical value for NA (see note)
#' @return numerical matrix
#' @note undefined values (i.e. diagonal if diag=FALSE and lower matrix if symmetrize=FALSE)
#' are set to na.val
#' @seealso \link{lx.upper2vect}
#' @examples
#' x <- matrix(1:16, ncol=4)
#' y <- lx.upper2vect(x, diag=TRUE)
#' xx <- lx.vect2upper(y, diag=TRUE)
#
lx.vect2upper <- function(x, diag=FALSE, bycol=TRUE, symmetrize=TRUE, na.val=NA) {
  n <- ((1 - 2*diag) + sqrt(1 + 8*length(x))) / 2
  mat <- matrix(0, n, n)
  if (bycol) {
    mat[upper.tri(mat, diag=diag)] <- x
  }
  else {
    mat[lower.tri(mat, diag=diag)] <- x
    mat <- t(mat)
  }
  if (symmetrize)
    mat <- mat + t(mat) - diag(diag(mat))
  else
    mat[lower.tri(mat)] <- na.val
  if (! diag)
    diag(mat) <- na.val
  mat
}
