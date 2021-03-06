# -------------------------------------------------
# $Id: lx.table.r 396 2019-01-02 22:53:10Z viari $
# lx counts and contingency tables utilities
#

# -------------------------------------------------
#' @name lx.tables
#' @docType data
#' @title cross tabulation and contigency tables
#' @description
#' there are several lx function to built contingency tables.\cr
#' \itemize{
#'  \item \link{lx.table}         : count or contingency table with full levels
#'  \item \link{lx.table.bycols}  : k-dimensional contingency table(s) from columns
#'  \item \link{lx.table.bypairs} : pairwise contingency table(s) from columns
#'  \item \link{lx.table.byfacts} : count or contingency table by factors in columns
#'  \item \link{lx.table.bysets}  : cross table of sets intersections
#'  \item \link{lx.table.bymsets} : cross table of multisets intersections
#'  \item \link{lx.table.bycoocs} : cross table of co-occurences in sets
#' }
#' \itemize{
#'  \item \link{lx.table.margins} : add margins (total) to a 2-dimensional contingency table
#' }
#'
NULL

# -------------------------------------------------
#' count or contingency table with specified levels
#' @description
#' same as \link{table} but force levels.
#' (call \link{factor} on \code{...} then call \link{table})
#' 
#' @param ... same as \link{table}
#' @param levels levels to force
#' @note
#' 
#' \link{table} is equivalent to \code{lx.table(...)}
#' @seealso \link{lx.tables}
#' @examples
#' x <- round(runif(10, 1, 10))
#' lx.table(x, levels=1:10)
#' y <- round(runif(10, 1, 10))
#' lx.table(x, y, levels=1:10)
#' 
lx.table <- function(..., levels=NULL) {
  if (is.null(levels))
    table(lapply(list(...), factor))
  else
    table(lapply(list(...), factor, levels=levels))
}

# -------------------------------------------------
#' tapply-like with specified levels
#' @description
#' similar to \link{tapply} but force levels and call FUN
#' on empty subsets.
#' 
#' @param x an atomic object, typically a vector
#' @param by a vector of factors (or anything that can be converted to factors)
#'        of same length as \code{x}
#' @param FUN the function to be applied
#' @param levels levels to force
#' @param ... optional arguments to FUN
#'
#' @return a vector of the same length as \code{levels} (or levels(by) if levels==NULL),
#' each value is the result of calling FUN on the subset of x at given level.
#' 
#' @seealso \link{lx.tables}
#' @examples
#' xval  <- c( 1,   2,   3,   0,   1 )   
#' group <- c('a', 'b', 'c', 'a', 'a')
#' lx.tapply(xval, by=group, length)
#' lx.tapply(xval, by=group, sum)
#' lx.tapply(xval, by=group, length, levels=c('a', 'b', 'd'))
#' 
lx.tapply <- function(x, by, FUN, levels=NULL, ...) {
  by   <- if(is.null(levels)) factor(by) else factor(by, levels=levels)
  res  <- tapply(x, by, FUN, ..., simplify=T)
  isna <- is.na(res)
  if (any(isna))
    res[isna] <- FUN(c())
  res
}

# -------------------------------------------------
#' k-dimensional contingency table(s) from columns
#' @param tab a table (matrix or dataframe) of nominal values
#' @param by a list of column indices or names (of length k >= 2)
#' @return k-dimensional contingency table, defined as the
#'         contigency table of the first two columns stratified on other columns
#' @note this is the same as \code{table(tab[,by])}
#' @seealso \link{lx.tables}
#' @examples
#' data(lx.iris)
#' tab <- lx.iris
#' # default: stratify by last dimensions (here sepal)
#' lx.table.bycols(tab)
#' # stratify by species (column index 1)
#' lx.table.bycols(tab, c(2,3,1))
#' # same as
#' lx.table.bycols(tab, c("petal", "sepal", "species"))
#
lx.table.bycols <- function(tab, by=colnames(tab)) {
  if (is.null(by)) by <- 1:ncol(tab)
  table(as.data.frame(tab)[,by])
}

# -------------------------------------------------
#' pairwise contingency table(s) from columns
#' @param tab a table (matrix or dataframe) of nominal values
#' @param by a list of column indices or names (of length k >= 2)
#' @return list of C(k, n) contingency tables crossing all pairs of columns
#'         in \code{by}
#' @seealso \link{lx.tables}
#' @examples
#' data(lx.iris)
#' tab <- lx.iris
#' lx.table.bypairs(tab)
#
lx.table.bypairs <- function(tab, by=colnames(tab)) {
  if (is.null(by)) by <- 1:ncol(tab)
  comb <- combn(by, 2, simplify=FALSE)
  res <- lapply(comb, function(x) lx.table.bycols(tab, by=x))
  names(res) <- sapply(comb, paste0, collapse="_by_")
  res
}

# -------------------------------------------------
#' make contingency table(s) by factors in columns
#' @param tab a table (matrix or dataframe) of nominal values
#' @param by a list of column indices or names (of length k >= 1)
#' @return a 2-dimensional contingency table T of size f x k,
#'         where f = number of levels in \code{tab[,by]}
#'         and T[i,j] equals to the number of rows with factor i in column j
#' @seealso \link{lx.tables}
#' @examples
#' data(lx.iris)
#' tab <- lx.iris
#' lx.table.byfacts(tab)
#
lx.table.byfacts <- function(tab, by=colnames(tab)) {
  if (is.null(by)) by <- seq_len(ncol(tab))
  tab <- tab[,by]
  lev <- sort(unique(factor((unlist(tab)))))
  if (is.null(dim(tab))) tab <- matrix(tab, ncol=1)
  apply(tab, 2, function(x) table(factor(x, levels=lev)))
}

# -------------------------------------------------
#' cross table of sets intersections
#' @param lsets a list of n named sets Si
#' @return n x n table where entry T[i,j] equals the size of Si inter Sj
#' @note aka \code{outer(lsets, lsets, function(x,y) \{ length(intersect(x,y)) \})}
#'       but \code{outer} does not work for this
#' @seealso \link{lx.tables}
#' @examples
#' lx.table.bysets(list(a=c(1,2), b=c(1,3), c=c(2,3,4)))
#' # this the same as 
#' lx.table.bysets(list(a=c(1,2,2), b=c(1,3), c=c(2,3,4,3)))
#' # since a, b, c are considered to as sets
#
lx.table.bysets <- function(lsets) {
  n <- length(lsets)
  res <- matrix(nrow=n, ncol=n)
  for (i in 1:n) {
    for (j in 1:n) { 
      res[i,j] <- length(intersect(unlist(lsets[i]), unlist(lsets[j])))
    }
  }
  res <- data.frame(res)
  names(res) <- row.names(res) <- names(lsets)
  res
}

# -------------------------------------------------
#' cross table of multisets intersections
#' @param lsets a list of n named multisets Mi
#'        each multiset can be specified as a list with repeated elements
#'        e.g. c('a', 'a', 'b', 'c', 'b')
#'        or, with as.card=TRUE, as a numeric list 
#'        e.g. c(a=2, b=2, c=1)
#' @param as.card use cardinality in multiset definition
#' @return n x m table T, where m = number of different elements in all Mi
#'         and T [i, j] equals to the number of elements j in multiset i
#' @seealso \link{lx.tables}
#' @examples
#' lsets <- list(A=c('a', 'a', 'b'), B=c('a'), C=c('a', 'b', 'b', 'c'), D=c('c'))
#' lx.table.bymsets(lsets)
#' lsets <- list(A=c(a=2, b=1), B=c(a=1), C=c(a=1, b=2, c=1), D=c(c=1))
#' lx.table.bymsets(lsets, as.card=TRUE)
#
lx.table.bymsets <- function(lsets, as.card=FALSE) {
  if (! as.card) lsets <- sapply(lsets, table)
  lev <- sort(unique(unlist(sapply(lsets, names))))
  res <- sapply(lsets, function(x) { 
    sapply(lev, function(y) { x[y] }) })
  res <- apply(res, c(1,2), function(x) { if (is.na(x)) 0 else x })
  res <- data.frame(t(res))
  names(res) <- lev
  res
}

# -------------------------------------------------
#' cross table of co-occurences in sets
#' @param lsets a list of sets Si
#' @param levels elements to keep (if NULL keeps all elements)
#' @return n x n table where n = |union(Si)| and entry T[i,j] equals the number
#'         of co-occurences of elements i and j in sets.
#'         diagonal elements are the number of occurences of each element.
#' @seealso \link{lx.tables}
#' @examples
#' l <- list(c('a'), c('a', 'b'), c('a', 'b', 'c'))
#' lx.table.bycoocs(l)
#'
lx.table.bycoocs <- function(lsets, levels=NULL) {
  if (is.null(levels))
    levels <- sort(unique(unlist(lsets)))
  
  nlev <- length(levels)
  cmat <- matrix(0, ncol=nlev, nrow=nlev, dimnames=list(levels, levels))
  
  for (e in lsets) {
    e <- intersect(e, levels)
    if (length(e) > 1) {
      apply(combn(e, 2), 2, function(x) {
        i <- factor(x[1], levels)
        j <- factor(x[2], levels)
        cmat[i, j] <<- cmat[j, i] <<- cmat[i, j] + 1
      })
    }
    for (ee in e) {
      i <- factor(ee, levels)
      cmat[i, i] <- cmat[i, i] + 1
    }
  }
  cmat
}

# -------------------------------------------------
#' add margins (total) to a 2-dimensional contingency table
#' @param ctab 2-dimensional contingency table
#' @param margin which margin to add
#'     1 (total by rows), 
#'     2 (total by columns) 
#'     c(1,2) both margins
#'     0 do nothing
#' @seealso \link{lx.tables}
#' @examples
#' data(lx.iris)
#' lx.table.margins(lx.table.bycols(lx.iris, by=1:2))
#'
lx.table.margins <- function(ctab, margin=c(1,2)) {
  if (2 %in% margin) ctab <- cbind(ctab, total=apply(ctab, 1, sum))
  if (1 %in% margin) ctab <- rbind(ctab, total=apply(ctab, 2, sum))
  ctab
}

# -------------------------------------------------
# <internal> <no-export>
# for lx.condsum - run-length-encoding version
#
.condsum.rel <- function(xvar, yvar, zero.rm, mx) {
  
  # add fake data if zero counts should be included
  #
  if (! zero.rm) {
    zero <- which(tabulate(xvar-mx) == 0)
    xvar <- c(zero+mx, xvar)
    yvar <- c(rep(0, length(zero)), yvar)
  }
  
  # tabulate using rle
  #
  
  ordx <- order(xvar)
  rle  <- rle(xvar[ordx])
  csy  <- cumsum(yvar[ordx])
  csl  <- cumsum(rle$lengths)
  res  <- cbind(rle$values, rle$length, diff(c(0, csy[csl])))
  
  # fix zero counts
  #
  if (! zero.rm) {
    res[zero+1,2] <- res[zero+1,3] <- 0
  }
  
  res
}


# -------------------------------------------------
# <internal> <no-export>
# for lx.condsum - cross product version
#
.condsum.cross <- function(xvar, yvar, zero.rm, mx, my, nx, ny) {
  
  # shift values to 0
  #
  xvar <- xvar - mx
  yvar <- yvar - my
  
  # cross-tabulate xvar and yvar
  #
  xtab <- tabulate(xvar+1)
  cros <- array(tabulate(yvar * nx + xvar + 1, nbins=(nx*ny)), dim=c(nx, ny))
  cnt  <- 0:(ny-1)
  ytab <- apply(cros, 1, function(r) r %*% cnt)
  ytab <- ytab + xtab * my
  
  res <- cbind(seq_along(ytab)+mx-1, xtab, ytab)
  
  # remove empty bins if zero should not be included
  #
  if (zero.rm) {
    isok <- xtab != 0
    res <- res[isok,,drop=F]
  }
  
  res
}

# -------------------------------------------------
#' tabulate conditional sum of integer values.
#' @description 
#' let xvar and yvar two integer vectors, this function
#' tabulates the conditional \code{sum(yvar|xvar)}. This is
#' similar to \link{aggregate}\code{(y,x,FUN=sum)} but much quicker
#' and limited to integer variables.
#' @param xvar,yvar integer vectors of the same length
#' @param zero.rm should zero counts be removed from results (as in \link{aggregate})
#' @param .maxrange,.maxrate internal parameters (see details)
#' @return an nx3 numeric matrix with columns namdes \code{x, cnt and sy}.\cr
#' \code{x}   : tabulated values of x\cr
#' \code{cnt} : counts for each x\cr
#' \code{sy}  : sum(y|x) the sum of y values for each x
#' @details 
#' the function uses two different algorithms (called rle and crossprod)
#' depending upon the length and range of xvar and yvar. crossprod is
#' usually quicker when x-range and y-range are small and n is large 
#' but uses more memory. rle runs in \code{O(n.log(n))} and depends only on
#' x-range.\cr
#' the choice is done on the following (simplified) condition\cr
#' \code{if ((xrange*yrange > .maxrange) || (xrange*yrange/n.log(n) > .maxrate))
#' then rle else crossprod}. this is a tradeoff between memory usage and speed.\cr
#' if you want to experiment with both algorithms to fit your particular
#' data, you may therefore force rle by setting \code{.maxrange=0} and force
#' crossprod by setting \code{.maxrange=Inf and .maxrate=Inf}
#' @examples
#' n = 1e4
#' x <- floor(runif(n=n, min=10, max=1000))
#' y <- rpois(n=n, lambda=x)
#' ax <- aggregate(y, by=list(x), FUN=sum)
#' bx <- lx.condsum(x, y)
#' cx <- lx.condsum(x, y, .maxrange=0) # force rle
#' all.equal(as.matrix(ax), bx[,c(1,3)], check.attributes=FALSE)
#' identical(bx, cx)
#' 
#' y <- y[x!=500]
#' x <- x[x!=500]
#' bx <- lx.condsum(x, y, zero.rm=FALSE)
#' cx <- lx.condsum(x, y, zero.rm=FALSE, .maxrange=0)
#' identical(bx, cx)
#' 
lx.condsum <- function(xvar, yvar, zero.rm=TRUE,
                       .maxrange=1e9, .maxrate=1) {
  
  colnames <- c("x", "cnt", "sy")
  
  # remove NA's
  #
  isok <- ! is.na(xvar+yvar)
  xvar <- xvar[isok]
  yvar <- yvar[isok]
  n    <- length(xvar)
  
  if (n == 0)
    return(matrix(nrow=0, ncol=3, dimnames=list(NULL, colnames)))
  
  lx.stopif(length(xvar) != length(yvar),
            "xvar and yvar must have same length") 
  
  # get range
  #
  mx <- min(xvar, na.rm=T)
  my <- min(yvar, na.rm=T)
  nx <- max(xvar, na.rm=T) - mx + 1
  ny <- max(yvar, na.rm=T) - my + 1
  
  
  # choose proper algorithm
  #
  res <- if ((nx*ny > .maxrange) || (nx*ny/n/log(n,10) > .maxrate))
    .condsum.rel(xvar, yvar, zero.rm, mx)
  else
    .condsum.cross(xvar, yvar, zero.rm, mx, my, nx, ny)
  
  colnames(res) <- colnames
  res
}
