# -------------------------------------------------
# $Id: lx.language.r 396 2019-01-02 22:53:10Z viari $
# lx misc langage utilities
#

# -------------------------------------------------
# <internal> digits for base conversion
#
.lx.DIGITS <- c(0:9, letters)

# =================================================
# low level langage utilities
#

# -------------------------------------------------
#' get function actual arguments
#' @description
#' get list of actual arguments of function call.
#' this is mostly useful to 1) retrieve arguments from function with
#' undefined number of arguments and 2) to retrieve actual arguments
#' as symbols (instead of values).
#' this should be called within a function (not from top level).
#' returned arguments are not evaluated but are returned
#' as symbols, constants or language expression.
#' @param pos position of argument to extract. if NULL returns all arguments, including
#' function name at pos=1. you can retrieve all arguments but function name
#' by using pos=-1.
#' @param up.frame, number of frame to go up to get caller. default (up=0)
#' means to retrieve arguments from the function calling \code{lx.args};
#' up=1 means the function calling the function calling \code{lx.args}
#' and so on up to top level.
#' @return
#' named list of actual (unevaluated) arguments\cr
#' or NULL if called from top-level
#' @note
#' pos is the position of argument i.e as declared in the function definition
#' not in the function call.
#' @note
#' avoid calling \code{lx.args} as an argument of another function
#' unless you are sure when/where it will actually be evaluated. 
#' @note
#' don't confuse this function with \link{lx.getargs} that handle command line arguments.
#' @examples
#' foo <- function(...) {
#'   args <- lx.args(-1)
#'   print(args)
#' }
#' foo(first=1, second=anything, "third", FUN=function(x) x+1)
#'
#' foo <- function() {
#'   caller <- lx.args(1, 1)[[1]]
#'   cat("foo caller is:", caller, "\n")
#' }
#' bar <- function() foo()
#' bar()
#'
lx.args <- function(pos=NULL, up.frame=0L) {
  if ((sys.nframe() - up.frame) < 2) # called from top level
    return(NULL) 
  parent <- sys.parent(up.frame+1)
  def    <- sys.function(parent)
  cal    <- sys.call(parent)
  # trouble when called within a lambda expression [fixme]
  args <- as.list(tryCatch(
    match.call(definition=def, call=cal),
    error=function(cond) "closure"))
  if (is.null(pos)) args else args[pos]
}

# -------------------------------------------------
#' set or get lx options
#' @param ... arguments of the form \code{key} or \code{key=value} (see details)
#' @param default default value for retrieving a single key (see details)
#' @return
#' for set/get of \bold{single key}: returns the associated value
#' or (in get mode) \code{default} or \code{NULL} if option cannot be found
#' and has no default\cr
#' for set/get of \bold{multiple keys}: returns a named list of
#' key:value. (note: in get mode if the key is not found, the name is \code{<NA>} and
#' value \code{NULL})\cr
#' @details
#' \subsection{Set form}{
#' \code{lx.options(key=value [, key2=value, ...])}\cr
#' will set option key to value\cr
#' note: to remove a key then just set its value to NULL (\code{lx.options(key=NULL)}
#' }
#' \subsection{Get form}{
#' \code{lx.options(key, [key2, ... ] [, default=value])}\cr
#' return the value(s) associated to key(s)\cr
#' note that \code{key} is either a symbol or a string, see examples below.\cr
#' a special form for retrieving a single key with a default value is:\cr
#' \code{lx.options(key, default=value)}\cr
#' finally, if called without argument:\cr
#' \code{lx.options()}, then the function returns the list of all current
#' options.
#' }
#' \subsection{Returned value}{
#' for set/get of \bold{single key}: returns the associated value
#' or (in get mode) \code{default} or \code{NULL} if key cannot be found\cr
#' for set/get of \bold{multiple keys}: returns a named list of
#' key:value.\cr
#' in get mode, if the key is not found, then the corresponding
#' name is \code{<NA>} and value \code{NULL}\cr
#' }
#'
#' note that set and get mode can be intermixed
#' (although this is a bit strange to do)\cr
#'
#' @note
#' as for \link{options}, key is a (uninterpreted) symbol or a character string.\cr
#' (for developpers: to use a variable value, use a \link{do.call})
#'
#' @examples
#' lx.options(foo=123, bar=234)
#' lx.options(foo)
#' lx.options("foo")
#' lx.options(foobar, default=456)
#' lx.options(foo=NULL)
#' lx.options(foo, default=pi)
#' lx.options()
#' @note
#' see \link{HELP.LX.OPTIONS} for a list of current builtin options
#' 
lx.options <- function(..., default=NULL) {
  args <- lx.args(-1)
  args$default <- NULL  # remove 'default'
  if (length(args) == 0) args <- names(.lx.env$options)
  nams <- names(args)
  if (is.null(nams))  nams <- rep("", length(args))
  opts <- mapply(function(name, arg) {
    if (nchar(name) == 0) { # get mode
      .lx.env$options[as.character(arg)]
    }
    else {                  # set mode
      as.list(.lx.env$options[[as.character(name)]] <- eval(arg, envir=parent.frame(3)))
    }}, nams, args)
  if (length(opts) == 1) opts <- unlist(opts, recursive=FALSE)
  if (is.null(opts) && ! is.null(default)) opts <- c(default=default)
  opts
}

# -------------------------------------------------
#' create an S3 instance of given class
#' @param classname string or string vector of classnames
#' @param ... anything accepted by \link{list}
#' @return a named list of \code{...} named fields and class(es) classname
#' @seealso \link{class}
#' @examples
#' x <- lx.new("people", name="alain", age=10)
#' class(x)
#' x$age
#'
lx.new <- function(classname, ...) {
  obj <- list(...)
  class(obj) <- c(classname, class(obj))
  obj
}

# -------------------------------------------------
#' set/get information on object
#' @description
#' to get object information:\cr
#' \code{lx.info(obj)}\cr
#' to set object information:\cr
#' \code{lx.info(obj) <- "value"}\cr
#' @param obj an R object
#' @seealso \link{lx.doc}
#' @examples
#' x <- 1:3
#' lx.info(x) <- "my array"
#' lx.info(x)
#' lx.doc(x)
#'
lx.info <- function(obj) attr(obj, ".lx.info")

#' @name lx.info
#' @aliases lx.info
#' @param value option value
#'
"lx.info<-" <- function(obj, value) {
  if (! is.null(obj))
    attr(obj, ".lx.info") <- value
  obj
}

# -------------------------------------------------
#' minimalist documentation about object
#' @description
#' return a minimalist description of an object
#' (including lx.info if available) and its
#' subfields (if any)
#' @param obj an R object
#' @param depth maximum depth of subfields recursion
#' @param maxfields maximum number of subfields
#' @param .name name to print in header (usually internal)
#' @param .prefix header prefix (usually internal)
#' @param .indent current indentation level (usually internal)
#' @seealso \link{lx.info}
#' @examples
#' x <- lx.new("people", name="alain", age=10, hobbies=c("scuba", "guitar"))
#' lx.info(x) <- "this is people class"
#' lx.info(x$age) <- "people age"
#' lx.doc(x)
#'
lx.doc <- function(obj, depth=2L, maxfields=10L, 
                   .name=deparse(substitute(obj)), .prefix=">", .indent=0L) {
  if (depth <= 0) return(invisible(NULL))
  .pas <- function(..., collapse="") paste0(..., collapse=collapse)
  .cat <- function(...) cat(..., "\n", sep="")
  .def <- function(s, def="") ifelse(is.null(s)||(s=="NULL")||(s==""), def, s)
  .ind <- function(s, n=.indent) .pas(.pas(rep(" ", 2*n)), s)
  .sub <- function(s, n) ifelse(n >= 0, substr(s, 1, n), substr(s, nchar(s)+n, nchar(s)))
  .trc <- function(s, n=30) ifelse(nchar(s)<=n, s, .pas(.sub(s,n/2), "...", .sub(s,-n/2)))
  typz  <- .def(typeof(obj))
  dimz  <- .pas("[", .def(.pas(dim(obj), collapse=" "), length(obj)), "]")
  clasz <- .pas("{", .def(.pas(class(obj), collapse=" "), length(obj)), "}")
  infoz <- ifelse(is.null(lx.info(obj)), "", .pas(" => ", lx.info(obj)))
  if ((infoz == "") && is.atomic(obj)) infoz <- .trc(.pas(" value: ", .pas(obj, collapse=" ")))
  .cat(.ind(.prefix), .name, " type:", typz, " dim:", dimz, " class:", clasz, infoz)
  for (e in head(names(obj), maxfields))
    lx.doc(obj[[e]], depth-1, maxfields, e, "$", .indent+1)
  if ((length(names(obj)) > maxfields) && (depth > 1))
    .cat(.ind(.prefix, .indent+1), "... ", length(names(obj))-maxfields, " other fields not printed...")
  invisible(NULL)
}

# =================================================
# high level langage utilities
# mostly R alternative replacements
#

# -------------------------------------------------
#' mixin named lists
#' @description 
#' replace key values in lout by key values in lin.
#' @param lout named list
#' @param lin named list
#' @param keys character array of keys to replace.
#'        default is \code{intersect(names(lout), names(lin))}
#' @return named list
#' @details
#' this is equivalent to
#' \preformatted{lout[keys] <- lin[keys]}
#' and is typically used to override formal arguments
#' by user's arguments to allow specifying only part
#' of a list argument (see example below).
#' @examples
#' # replace
#' lx.mixin(list(a=1, c=3), list(a=10, b=20))
#' 
#' # replace and add
#' lx.mixin(list(a=1, c=3), lin<-list(a=10, b=20), names(lin))
#' 
#' # select
#' lx.mixin(NULL, lin<-list(a=10, b=20), intersect(names(lin), c("a", "c")))
#' 
#' # typical usage with function args
#' foo <- function(arg=list(a=1, b="foo")) {
#'    dft <- formals()
#'    arg  <- lx.mixin(dft$arg, arg)
#'    arg
#' }
#' foo(list(a=2)) # => list(a = 2, b = "foo")
#' 
#
lx.mixin <- function(lout, lin, keys=intersect(names(lout), names(lin))) {
  lout[keys] <- lin[keys]
  lout
}

# -------------------------------------------------
#' convert named list to character string
#' @description 
#' convert named list to character string of the form
#' "key1<sep>value1<collapse>key2<sep>value2...."
#' @param lst named list
#' @param sep separator between key and value
#' @param collapse sep separator elements
#' @param quote character to use for quoting character values(use NA to prevent quoting)
#' @param ... arguments to \link{format} for formating elements
#' @note list values should be atomic. vectors are printed as '[x,y,...]'.
#'       recursive lists or matrices are not (yet) handled.
#' @return character string
#' @examples
#' ll <- list(a=1, b="you", c=1:3)
#' lx.list2str(ll)
lx.list2str <- function(lst, sep="=", collapse=", ", quote=NA, ...) {
  lst <- sapply(lst, function(x) {
            if (length(x) > 1) paste0("[", paste(x, collapse=","), "]")
            else if (is.character(x) && (! is.na(quote))) paste0(quote, x, quote)
            else format(x, ...) })
  paste(lx.napply(lst, function(nam, val) paste(nam, val, sep=sep)),
        collapse=collapse)
}

# -------------------------------------------------
#' translate keys fom dictionnary
#' @description
#' translate keys in vector \code{x} to values from dictionnary \code{dict}
#' @param x key or vector of keys
#' @param dict dictionnary (list indexed by keys, plus optional default value)
#' @examples
#' lx.key.trans("red", c(red="rouge", blue="bleu"))
#' lx.key.trans(c("red", "blue"), c(red=1, green=2, blue=3))
#' lx.key.trans(c("red", "pink"), c(red=1, green=2, blue=3))
#' lx.key.trans(c("red", "pink"), c(red=1, green=2, blue=3, 0))
#
lx.key.trans <- function(x, dict) {
  res <- as.character(sapply(x, function(e) { do.call(switch, as.list(c(e, dict))) }))
  mode(res) <- mode(dict)
  res
}

# -------------------------------------------------
#' reverse dictionnary
#' @param dict dictionnary : a list indexed by keys
#' (plus optional default value that is ignored)
#' @return reverse dictionary : a list indexed by values
#' @examples
#' dict <- list(warm=c("red"), cold=c("blue", "green"), basic=c("red", "blue", "green"), "white")
#' lx.rev.dict(dict)
#' # => list(red=c("warm", "basic"), blue=c("cold", "basic"), green=c("cold", "basic"))
#
lx.rev.dict <- function(dict) {
  res <- list()
  for (nam in names(dict)) {
    for (key in unique(dict[[nam]])) {
      res[[key]] <- c(res[[key]], nam)
    }
  }
  res
}

# -------------------------------------------------
#' quicker \code{\%in\%} for numeric vectors
#' @description
#' works as \link{\%in\%} but much quicker and for numeric vectors only.
#' @param x numeric vector: the values to be matched
#' @param table numeric vector: the values to be matched against.
#' (better if sorted increasingly)
#' @param .threshold internal threshold on table length to switch
#' between standard \%in\% and quicker algorithm.
#' @return A logical vector, indicating if a match was located for each element of x.
#' @note table should be sorted increasingly. if not it will be sorted
#' internally.
#' @examples
#' lx.in(1:10, 3:5)
#' lx.in((1:10)/1000, (3:5)/1000)
#' lx.in(5e6, 1:1e7)  # quicker than 5e6 %in% 1:1e7
#' 
lx.in <- function(x, table, .threshold=500) {
  if (length(table) < .threshold)
    return(x %in% table)
  if (is.unsorted(table)) table <- sort(table)
  table[pmax(1, findInterval(x, table))] == x
}

# -------------------------------------------------
#' open ends version of findInterval
#' @description
#' works as \link{findInterval} but always consider that end
#' breakpoints are infinite (i.e vec[1]=-Inf and vec[N]=+Inf).
#' This always return indices in range [1, N].
#' @param x numeric vector: the values to find
#' @param vec numeric, sorted (weakly) increasingly, of length N.
#' see \link{findInterval}
#' @return integer vector of length length(x) with values in 1:N (and NA).
#' @note this is simply implemented as \code{pmax(1, findInterval(x, vec))}
#' @seealso \link{findInterval}
#' @examples
#' lx.findInterval(0:11, 1:10)
#' lx.findInterval(c(-Inf, Inf), 1:10)
#' 
lx.findInterval <- function(x, vec) {
  pmax(1, findInterval(x, vec, rightmost.closed=F, all.inside=F))
}

# -------------------------------------------------
#' head of vector
#' @description 
#' same as \link{head} but limited to vectors and quicker
#' @param x vector
#' @param n if positive take n first elements, if negative take all but n last elements
#' @return vector of size n if n >= 0, or of size \code{length(x) - n} if n < 0
#' @examples
#' lx.head(1:10, 2)  # => 1 2
#' lx.head(1:10, -2) # => 1 2 3 4 5 6 7
#' @seealso \link{head} \link{lx.tail}
#
lx.head <- function(x, n=1L) {
  m <- length(x)
  if ((n == 0) || (n <= -m))
    integer(0)
  else if (n > 0)
    x[1:min(n,m)]
  else
    x[1:min(m+n,m)]
}

# -------------------------------------------------
#' tail of vector
#' @description 
#' same as \link{tail} but limited to vectors and quicker
#' @param x vector
#' @param n if positive take n last elements, if negative take all but n first elements
#' @return vector of size n if n >= 0, or of size \code{length(x) - n} if n < 0
#' @examples
#' lx.tail(1:10, 2)  # => 9 10
#' lx.tail(1:10, -2) # => 3 4 5 6 7 8 9 10
#' @seealso \link{tail} \link{lx.head}
#
lx.tail <- function(x, n=1L) {
  m <- length(x)
  if ((n == 0) || (n <= -m))
    integer(0)
  else if (n > 0)
    x[max(m-n+1,1):m]
  else
    x[max(1-n,1):m]
}

# -------------------------------------------------
#' binned sum 
#' @description 
#' compute the jumping sum of k consecutive elements in vector.
#' this is equivalent to
#' \code{sapply(seq.int(1,length(x),by=k), function(i) sum(x[i:(i+(k-1))], na.rm=T))}
#' but is quicker.
#' @param x vector
#' @param k number of consecutive elements to sum (k >= 1)
#' @param drop drop the last element of result if length(x) is not a multiple
#' of k.
#' @return vector of size ceiling(length(x) / k) if drop=FALSE or
#' floor(length(x) / k) if drop=TRUE, containing the
#' sum of k consecutive elements.
#' @note if drop=FALSE and length(x) is not a multiple of k 
#' the last element will sum up less than k elements.
#' @examples
#' lx.binsum(1:10, 2)                 # => 3 7 11 15 19
#' lx.binsum(rep(1,10), 3)            # => 3 3 3 1
#' lx.binsum(rep(1,10), 3, drop=TRUE) # => 3 3 3
#' @seealso \link{lx.rollsum} for a rolling (instead of jumping) version
#'
lx.binsum <- function(x, k, drop=FALSE) {
  if (! drop) {
    pad <- (k - (length(x) %% k)) %% k
    if (pad != 0)
      x <- c(as.numeric(x), rep(0, pad))
  }
  to <- length(x)-k+1
  if (to <= 0) return(numeric(0))
  y <- cumsum(as.numeric(x))
  i <- seq.int(1, to, k)
  y[i+k-1]-y[i]+x[i]
}

# -------------------------------------------------
#' rolled sum 
#' @description 
#' compute the rolling sum of k consecutive elements in vector.
#' this is equivalent to
#' \code{sapply(seq_along(x), function(i) sum(x[i:(i+(k-1))], na.rm=T))}
#' but is much quicker since it runs in \code{O(n)}.
#' @param x vector
#' @param k number of consecutive elements to sum (k >= 1)
#' @param drop drop the last (k-1) elements of result 
#' @return vector of size length(x) if (drop==F) or length(x)-k+1
#' if (drop==F), containing the sum of k consecutive elements.
#' @note with drop==F the last (k-1) elements will
#' sum up less than k elements.
#' @examples
#' lx.rollsum(1:5, 2)          # => 3 5 7 9 5
#' lx.rollsum(rep(1,10), 3, TRUE) # => 3 3 3 3 3 3 3 3
#' @seealso \link{lx.binsum} for a jumping (instead of rolling) version
#'
lx.rollsum <- function(x, k, drop=FALSE) {
  pad <- rep(0, k-1)
  x   <- cumsum(c(as.numeric(x), pad))
  x   <- x - c(0, pad, lx.head(x, -k))
  x   <- if (k > 1) lx.tail(x, 1-k) else x
  if (drop && (k > 1)) lx.head(x, 1-k) else x
}

# -------------------------------------------------
#' rotate vector circularly n times
#' @param x vector
#' @param n number of rotations.\cr 
#'        if > 0 rotate to left else rotate to right
#' @return
#' rotated vector
#' @examples
#' lx.rotate(1:5, 2)  # => 3 4 5 1 2
#' lx.rotate(1:5, -2) # => 4 5 1 2 3
#
lx.rotate <- function(x, n=1L) {
  right <- (n < 0)
  m <- length(x)
  n <- abs(n) %% m
  from <- (if (right) (m - n) else n) + 1
  c(x,x)[from:(2*m)][1:m]
}

# -------------------------------------------------
#' shift vector n times and pad with fill value
#' @param x vector
#' @param n number of shifts.\cr
#'        if >= 0 shift to left else shift to right
#' @param fill value to fill
#' @return
#' shifted vector
#' @examples
#' lx.shift(1:5, 2, fill=0)  # => 3 4 5 0 0
#' lx.shift(1:5, -2, fill=0) # => 0 0 1 2 3
#
lx.shift = function(x, n=1L, fill=NA) {
  if (n == 0)
    x
  else if (n > 0)
    c(lx.tail(x, -n), rep(fill, n))
  else
    c(rep(fill, -n), lx.head(x, n))
}

# -------------------------------------------------
#' expand by recycling or shrink vector to given size
#' @description 
#' given a vector x either shrink or expand by recycling to size n
#' @param x vector
#' @param n positive integer
#' @return vector of size n
#' @examples
#' lx.recycle(1:3, 6)
#' lx.recycle(1:10, 6)
#' lx.recycle(1:10, 0)
#' lx.recycle(NULL, 2)
#' @seealso \link{lx.rotate}
#
lx.recycle <- function(x, n) {
  lx.stopif(n < 0, "n argument should be positive or null")
  if (length(x) == 0) x else head(rep(x, ceiling(n/length(x))), n)
}

# -------------------------------------------------
#' decimate vector
#' @description
#' downsample vector to length \code{length.out} by selecting
#' equally spaced indices from 1 to \code{length(x)}.
#' @param x vector
#' @param length.out desired length of the resulting vector.
#' @param as.index if TRUE return indices else returns values
#' @note if \code{length(x) <= length.out} then the original
#' vector x (or indices) is returned.
#' @examples 
#' lx.decimate(1:100, 10)
#' lx.decimate(letters, 5)
#' lx.decimate(letters, 5, as.index=TRUE)
#
lx.decimate <- function(x, length.out, as.index=FALSE) {
  n <- length(x)
  if (n <= length.out) {
    if (as.index) seq_along(x) else x
  }
  else {
    indx <- round(seq.int(1, n, length.out=length.out))
    if (as.index) indx else x[indx]
  }
}

# -------------------------------------------------
#' get maximum in rows
#' @description find the maximum for each row of a matrix
#' @param x numerical matrix
#' @param ties.method a character string specifying how ties are handled. (see \link{max.col})
#' @param what a character string specifying what should be returned (see Value)
#' @return \itemize{
#' \item if \code{what == "value"} return a maximal value for each row
#' \item if \code{what == "index"} return index of a maximal value for each row
#' }
#' @note
#' with \code{what == "index"} this function is identical to \link{max.col}
#' 
#' with \code{what == "value"} this function is identical to 
#' \code{apply(1, x, max)} but much quicker for large matrices
#' 
#' @seealso \link{lx.rowMins}
#
lx.rowMaxs <- function(x, ties.method=c("random", "first", "last"),
                      what=c("value", "index")) {
  ties.method <- match.arg(ties.method)
  what <- match.arg(what)
  imax <- max.col(x, ties.method=ties.method)
  switch(what,
         index=imax,
         x[cbind(seq_along(imax), imax)])
}

# -------------------------------------------------
#' get minimum in rows
#' @description find the minimum for each row of a matrix
#' @param x numerical matrix
#' @param ties.method a character string specifying how ties are handled. (see \link{max.col})
#' @param what a character string specifying what should be returned (see Value)
#' @return \itemize{
#' \item if \code{what == "value"} return a minimum value for each row
#' \item if \code{what == "index"} return index of a minimum value for each row
#' }
#' @note
#' with \code{what == "index"} this function is identical
#' to \link{max.col}(-x)
#' 
#' with \code{what == "value"} this function is identical to 
#' \code{apply(1, x, min)} but much quicker for large matrices
#' 
#' @seealso \link{lx.rowMaxs}
#
lx.rowMins <- function(x, ties.method=c("random", "first", "last"),
                      what=c("value", "index")) {
  x <- lx.rowMaxs(-x, ties.method=ties.method, what=what)
  switch(match.arg(what), index=x, -x)
}

# -------------------------------------------------
#' linearly rescale numeric vector to specified range.
#' @description
#' rescale numeric vector x to x' from range [f1, f2] to range [t1, t2]
#' according to:\cr
#' \code{x}'\code{ = a.x + b}\cr
#' where a=(t2-t1)/(f2-f1) and b = t1 - a.f1
#' @param x numeric vector
#' @param to output range (numeric vector of length two)
#' @param from input range (numeric vector of length two).\cr
#'        if not specified, use the range of x
#' @param zerob force b=0. this is useful for rescaling differences
#' @return rescaled vector
#' @examples
#' lx.scale(1:10)
#' lx.scale(1:10, c(1,10))
#
lx.scale <- function(x, to=c(0, 1), from=range(x, na.rm=TRUE), zerob=FALSE) {
  if ((length(from) < 2) || (abs(from[1]-from[2]) < .Machine$double.eps))
    rep(mean(to), length(x))
  else if (zerob)
    x * (to[2]-to[1]) / (from[2]-from[1])
  else
    (x - from[1]) * (to[2]-to[1]) / (from[2]-from[1]) + to[1]
}

# -------------------------------------------------
#' always TRUE function
#' @description
#' this function always returns TRUE, whatever
#' the arguments.
#' @param ... anything (ignored arguments)
#' @return TRUE
#'
lx.true  <- function(...) { TRUE }

# -------------------------------------------------
#' always FALSE function
#' @description
#' this function always returns FALSE, whatever
#' the arguments.
#' @param ... anything (ignored arguments)
#' @return FALSE
#'
lx.false <- function(...) { FALSE }

# -------------------------------------------------
#' a prettier \code{summary} for dataframes
#' @param df a dataframe
#' @note only works with numeric columns
#' @examples
#' lx.summary(iris[,-5])
#
lx.summary <- function(df) {
  res  <- summary(df)
  rnam <- sapply(res[,1], function(x) { gsub(" *:[^:]*$", "", x) })
  cnam <- colnames(res)
  res <- data.frame(apply(res, c(1,2), function(x) { gsub("^[^:]*: *", "", x) }))
  rownames(res) <- rnam
  colnames(res) <- cnam
  res
}

# -------------------------------------------------
#' get number representation
#' @description
#' get base representation of a positive numeric
#' (accurate up to 53 bits)
#' @param num positive (unsigned) numeric value
#' @param base representation base (2 <= base <= 36)
#' @param prefix prefix to add (e.g. '0x' for hexadecimal)
#' @return string
#' @examples
#' lx.tobase(123, prefix="0x")  # 0x7b
#
lx.tobase <- function(num, base=16L, prefix="") {
  s <- ""
  base <- min(36, max(2, base))
  while (num > 0) {
    s <- paste0(.lx.DIGITS[(num %% base) + 1], s)
    num <- num %/% base
  }
  paste0(prefix, ifelse(nchar(s) == 0, "0", s))
}
