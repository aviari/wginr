# -------------------------------------------------
# $Id: xlx.dna.r 321 2017-08-18 11:10:19Z viari $
# XLX Dna sequences
#

# -------------------------------------------------
#' @name HELP.DNA
#' @docType data
#' @title Dna Class
#' @description
#' \bold{Dna} is an S3 Class that let you manipulate DNA sequences
#' with a more memory-efficient way than usual character strings.\cr
#' More precisely \bold{Dna} stores sequence with a 3, 2 or 1 bits/symbol
#' instead of 8 bits for character strings.\cr
#'
#' \bold{Dna} currently works with the following restrictions:
#' \itemize{
#'   \item the DNA alphabet is lowercase and restricted to "acgtryn" or "acgt"
#'         depending upon the storage mode.
#'   \item maximum sequence size is 2^31-1 (this is the same limitation as for
#'         character strings, until R internally goes to 64 bits vector indexes)
#' }
#' \subsection{creation}{
#' Dna sequences are created by the \link{Dna} constructor or \link{as.Dna} coercion.
#' }
#' \subsection{manipulation}{
#' Dna sequences can be transformed to strings by \link{as.character}\cr
#' Access to sequence components is performed by subscripting (either as
#' extracting or replacing):\cr
#' \code{dna[index]} and \code{dna[index] <- seq}\cr
#' }
#' \subsection{misc}{
#' other \bold{Dna} operations include:\cr
#' \link{c}, \link{length}, \link{subseq}, \link{summary},
#' \link{rev}, \link{compl}, \link{revcompl}, \link{plot} etc.
#' }
#'
#' @note
#' \bold{Dna} requires the \link{bit} library
#' @note
#' the \code{print} S3 method has been redefined. For debugging purpose, you may
#' use \code{unclass(obj)} to see the actual internal components.
#'
#' @examples
#' # generate a 10 Mb sequence
#' n <- 1e7
#' seq <- paste(sample(c("a", "c", "g", "t"), n, replace=TRUE), collapse="")
#' x <- Dna(seq, "strict")
#' length(x)
#' summary(x)
#' s <- as.character(x)  # identical to seq
#' # extract or replace subsequences 
#' x[1:20]
#' x[20:1]
#' x[1:20] <- "acgt"
#' x[1:20] <- "acgtn"  # will complain
#' x[]  # same as Dna(x) or as.Dna(x) or x
#' # some operations
#' revcompl(x)
#' countsymb(x, "gc")
#' countsymb(x, "W")
#' plot(x) # guess what 
#' # some funny constructors
#' x <- Dna(15)
#' x[] <- "acg"
#' as.character(x[seq.int(1, length(x), by=3)])
#'
NULL

# =================================================
# internals <no_export>
# =================================================
#
# this section contains all function related to the
# internal bit encoding.
#

# -------------------------------------------------
#' @name HELP.DNA.ENCODING
#' @docType data
#' @title Dna Internal Encoding Scheme
#' @description
#' this section describes the internal Dna bits encoding schemes
#' and is intended for developpers only.\cr
#'
#' \tabular{ll}{
#' \bold{code}\tab\bold{length(bits)}\cr
#' standard\tab 3\cr
#' strict\tab 2\cr
#' pattern\tab 1\cr
#' }
#'
#' \subsection{standard encoding}{
#' \tabular{ll}{
#' \bold{i}\tab\bold{bits[[i]]}\cr
#' 1\tab[acgt]\cr
#' 2\tab[cgny]\cr
#' 3\tab[gtnr]\cr
#' }
#' \tabular{llll}{
#' \bold{symb}\tab\bold{bit1}\tab\bold{bit2}\tab\bold{bit3}\cr
#' g\tab 1\tab 1\tab 1\cr
#' c\tab 1\tab 1\tab 0\cr
#' t\tab 1\tab 0\tab 1\cr
#' a\tab 1\tab 0\tab 0\cr
#' \tab\tab\tab\cr
#' n\tab 0\tab 1\tab 1\cr
#' y\tab 0\tab 1\tab 0\cr
#' r\tab 0\tab 0\tab 1\cr
#' x\tab 0\tab 0\tab 0\cr
#' }}
#'
#' \subsection{strict encoding}{
#' \tabular{ll}{
#' \bold{i}\tab\bold{bits[[i]]}\cr
#' 1\tab[gc]\cr
#' 2\tab[gt]\cr
#' }
#' \tabular{lll}{
#' \bold{symb}\tab\bold{bit1}\tab\bold{bit2}\cr
#' g\tab 1\tab 1\cr
#' c\tab 1\tab 0\cr
#' t\tab 0\tab 1\cr
#' a\tab 0\tab 0\cr
#' }}
#'
#' \subsection{pattern encoding}{
#' \tabular{ll}{
#' \bold{i}\tab\bold{bits[[i]]}\cr
#' 1\tab pattern\cr
#' }
#' \tabular{ll}{
#' \bold{symb}\tab\bold{bit1}\cr
#' pattern\tab 1\cr
#' !pattern\tab 0\cr
#' }}
#'
#'
NULL

# -------------------------------------------------------
# <internal> <no_export>
# init Dna bits
#
.init <- function(size, code, pattern) {
  bsiz <- switch(code, standard=3, strict=2, 1)
  bits <- lapply(seq.int(bsiz), function(x) bit::bit(size))
  res <- list(code=code, pattern=pattern, bits=bits)
  class(res) <- "Dna"
  res
}

# -------------------------------------------------------
# <internal> <no_export>
# truncate or recycle string
#
.adjust <- function(seq, size) {
  len <- nchar(seq)
  while (len < size)
    len <- nchar(seq <- paste0(seq, seq))
  substr(seq, 1, size)
}

# -------------------------------------------------------
# <internal> <no_export>
# encode method
#
# @todo : do by chunks to avoid excessive memory usage
#

.encode <- function(dna, seq) {
    UseMethod(".encode")
}

.encode.Dna <- function(obj, seq) {

  # truncate or recycle string if needed

  seq <- .adjust(tolower(seq), length(obj))

  # internal complain 
  
  .check <- function(seq, pat, by) {
    err = grepl(paste("[^", pat, "]", sep=""), seq)
    lx.warnif(err, "sequence contains other symbol(s) than '", pat,
                   "', will be replaced by '", by, "'")
  }

  # go ahead
  
  if (obj$code == "standard") {
    .check(seq, "acgtnry", "x")
    obj$bits[[1]][lx.strchr(seq, "acgt")]<- TRUE
    obj$bits[[2]][lx.strchr(seq, "cgny")]<- TRUE
    obj$bits[[3]][lx.strchr(seq, "gtnr")]<- TRUE
  }
  else if (obj$code == "strict") {
    .check(seq, "acgt", "a")
    obj$bits[[1]][lx.strchr(seq, "cg")]<- TRUE
    obj$bits[[2]][lx.strchr(seq, "gt")]<- TRUE
  }
  else {
    if (is.null(obj$pattern)) obj$pattern <- "[acgt]"
    obj$bits[[1]][lx.gregexpr(obj$pattern, seq)]<- TRUE
  }
  obj  
}

# -------------------------------------------------------
# <internal> <no_export>
# decode method
#

.decode <- function(dna) {
    UseMethod(".decode")
}

.decode.Dna <- function(obj) {
  bits <- obj$bits
  len <- length(bits[[1]])
  seq <- character(len)
  if (obj$code == "standard") {
    seq[bit::as.which(  bits[[1]] &  bits[[2]] &  bits[[3]])] <- "g"
    seq[bit::as.which(  bits[[1]] &  bits[[2]] & !bits[[3]])] <- "c"
    seq[bit::as.which(  bits[[1]] & !bits[[2]] &  bits[[3]])] <- "t"
    seq[bit::as.which(  bits[[1]] & !bits[[2]] & !bits[[3]])] <- "a"
    seq[bit::as.which( !bits[[1]] &  bits[[2]] &  bits[[3]])] <- "n"
    seq[bit::as.which( !bits[[1]] &  bits[[2]] & !bits[[3]])] <- "y"
    seq[bit::as.which( !bits[[1]] & !bits[[2]] &  bits[[3]])] <- "r"
    seq[bit::as.which( !bits[[1]] & !bits[[2]] & !bits[[3]])] <- "x"
  }
  else if (obj$code == "strict") {
    seq[bit::as.which(  bits[[1]] &  bits[[2]])] <- "g"
    seq[bit::as.which(  bits[[1]] & !bits[[2]])] <- "c"
    seq[bit::as.which( !bits[[1]] &  bits[[2]])] <- "t"
    seq[bit::as.which( !bits[[1]] & !bits[[2]])] <- "a"
  }
  else {
    seq[bit::as.which( bits[[1]])] <- "x"
    seq[bit::as.which(!bits[[1]])] <- "."
  }
  paste0(seq, collapse="")
}

# -------------------------------------------------------
# <internal> <no_export>
# complement method
#

.complement <- function(dna) {
    UseMethod(".complement")
}

.complement.Dna <- function(obj) {
  bits <- obj$bits
  if (obj$code == "standard") {
    newbits <- bits[[3]]
    newbits[bit::as.which( bits[[1]] & !bits[[2]] &  bits[[3]])] <- FALSE # - 't'
    newbits[bit::as.which( bits[[1]] &  bits[[2]] & !bits[[3]])] <- TRUE  # + 'c'
    obj$bits[[2]] <- newbits
    newbits <- bits[[2]]
    newbits[bit::as.which( bits[[1]] &  bits[[2]] &  bits[[3]])] <- FALSE # - 'g'
    newbits[bit::as.which( bits[[1]] & !bits[[2]] & !bits[[3]])] <- TRUE  # + 'a'
    obj$bits[[3]] <- newbits
  }
  else if (obj$code == "strict") {
    newbits <- bits[[1]]
    newbits[bit::as.which( bits[[1]] &  bits[[2]])] <- FALSE # - 'g'
    newbits[bit::as.which(!bits[[1]] & !bits[[2]])] <- TRUE  # + 'a'
    obj$bits[[2]] <- newbits
  }
  else {
    obj$bits[[1]] <- !obj$bits[[1]]
  }
  
  obj
}

# -------------------------------------------------------
# <internal> <no_export>
# count method
#

.countbase <- function(obj, symb) {
  bits <- obj$bits

  if (obj$code == "standard") {
    switch(symb,
      g=sum(  bits[[1]] &  bits[[2]] &  bits[[3]]),
      c=sum(  bits[[1]] &  bits[[2]] & !bits[[3]]),
      t=sum(  bits[[1]] & !bits[[2]] &  bits[[3]]),
      a=sum(  bits[[1]] & !bits[[2]] & !bits[[3]]),
      n=sum( !bits[[1]] &  bits[[2]] &  bits[[3]]),
      y=sum( !bits[[1]] &  bits[[2]] & !bits[[3]]),
      r=sum( !bits[[1]] & !bits[[2]] &  bits[[3]]),
      x=sum( !bits[[1]] & !bits[[2]] & !bits[[3]]),
      0)
  }
  else if (obj$code == "strict") {
    switch(symb,
      g=sum(  bits[[1]] &  bits[[2]]),
      c=sum(  bits[[1]] & !bits[[2]]),
      t=sum( !bits[[1]] &  bits[[2]]),
      a=sum( !bits[[1]] & !bits[[2]]),
      0)
  }
  else {
    switch(symb,
      x=sum(  bits[[1]]),
      .=sum( !bits[[1]]),
      0)
  }
}

.count <- function(dna, symb) {
    UseMethod(".count")
}

.count.Dna <- function(obj, symb) {
  if (symb == toupper(symb)) {
    switch(symb,
      R=sum(.countbase(obj, "a"), .countbase(obj, "g"), .countbase(obj, "r")),
      Y=sum(.countbase(obj, "c"), .countbase(obj, "t"), .countbase(obj, "y")),
      M=sum(.countbase(obj, "c"), .countbase(obj, "a")),
      K=sum(.countbase(obj, "g"), .countbase(obj, "t")),
      W=sum(.countbase(obj, "a"), .countbase(obj, "t")),
      S=sum(.countbase(obj, "c"), .countbase(obj, "g")),
      B=sum(.countbase(obj, "c"), .countbase(obj, "t"), .countbase(obj, "g")),
      D=sum(.countbase(obj, "a"), .countbase(obj, "t"), .countbase(obj, "g")),
      H=sum(.countbase(obj, "a"), .countbase(obj, "t"), .countbase(obj, "c")),
      V=sum(.countbase(obj, "a"), .countbase(obj, "c"), .countbase(obj, "g")),
      N=length(obj) - .countbase(obj, "x"),
      .countbase(obj, tolower(symb)))
  }
  else
    .countbase(obj, symb)
}

# =================================================
# API Dna S3 class
# =================================================

# ---------------------------------------------
# encoding types
#  standard = acgt nryx : 3 bits
#  strict   = acgt      : 2 bits
#  pattern  = <pat>     : 1 bit
#
# 1: 1=[acgt]
# 2: 1=[gcny]
# 3: 1=[gtnr]
#
# N: 0 11
# Y: 0 10
# R: 0 01
# X: 0 00
#
# G : 1 11
# C : 1 10
# T:  1 01
# A:  1 00
#
# setOldClass("Dna")
#

# ---------------------------------------------
#' Dna Class constructor
#' @param x either an integer, a character string or another Dna object
#' @param code encoding scheme (see details)
#' @param pattern pattern to use (only used if code="pattern")
#' @details
#' \subsection{x parameter}{
#' if x is an integer, creates an empty Dna sequence of size x\cr
#' if x is a character string, creates a Dna sequence representing x\cr
#' if x is a Dna object, same as \code{Dna(as.character(x), ...)}\cr
#' }
#' \subsection{code parameter}{
#' if code == "standard" use 3 bits/symbol to represent
#' symbols in [acgtryn]\cr
#' if code == "strict" use a 2 bits/symbol to represent
#' symbols in [acgt]\cr
#' if code == "pattern" use a 2 bits/symbol to represent
#' pattern by [x.]\cr
#' }
#' @examples
#' x <- Dna("acgtnnactg")
#' x <- Dna("acgtacgt", "strict")
#' x <- Dna("acgtnnnacgtg", "pattern", "[gc]")
#' x <- Dna(10)
#' x[] <- "accn"
#' x <- Dna(10, "strict")
#' x <- Dna(10, "pattern", "[gc]")
#' x[] <- "gca"
#'
Dna <- function(x=0L, code=c("standard", "strict", "pattern"),
                pattern=NULL) {
    
  ucode <- match.arg(code)

  if ((ucode == "pattern") && is.null(pattern))
    pattern <- "[agct]"
  
  # case Dna : copy or re-encode
  #
  if (is.Dna(x)) {
    xcode <- if (missing(code)) x$code else ucode
    xpatt <- if (missing(pattern)) x$pattern else pattern
    if ((xcode == x$code) && identical(xpatt, x$pattern))
      return(x)
    # we should re-encode
    return(.encode(.init(length(x), xcode, xpatt), .decode(x)))
  }

  # case character string : encode
  #
  if (is.character(x)) {
    return(.encode(.init(nchar(x), ucode, pattern), x))
  }
  
  # case integer : make empty Dna of given length
  #
  .init(as.integer(x), ucode, pattern)
}

# ---------------------------------------------
# overload generics
# ---------------------------------------------

# ---------------------------------------------
#' test for Dna Class
#' @param obj object to be tested
#'
is.Dna <- function(obj) {
  return(class(obj) == "Dna")
}

# ---------------------------------------------
#' @name print
#' @title Print method for Dna
#' @description Print Dna sequence
#' @param x a Dna object
#' @param ... further arguments passed to or from other methods.
#'
print.Dna <- function(x, ...) {
  seq <- if (length(x) > 20)
            paste0(.decode(subseq(x, 1, 10)), "...",
                   .decode(subseq(x, -10)))
         else
            .decode(x)
    
  cat(sep="", "<DNA sequence>", 
              " code=", x$code,
              "; length ", length(x), 
              "; pattern='", x$pattern,
              "'; seq='", seq, "'")
  invisible(x)
}

# ---------------------------------------------
#' @name length
#' @title Length method for Dna
#' @description Length of Dna sequence
#' @param x a Dna object
#' @return length of Dna sequence
#'
length.Dna <- function(x) {
  length(x$bits[[1]])
}

# ---------------------------------------------
#' @name [.Dna
#' @title Subscript extract method for Dna
#' @description extract subscript from Dna sequence
#' @param obj Dna object
#' @param index any indexing expression (except for negative indices - see details)
#' @return Dna subsequence
#' @details
#' \bold{negative indices} are not interpreted the usual way (i.e. as tail)
#' but as \bold{drop} (like in Python). this is more convenient to delete
#' symbols.
#' @seealso \link{subseq} \link{[<-.Dna}
#' @examples
#' x <- Dna("acgtnacgtn")
#' x[1:5]
#' x[5:1]
#' x[seq.int(1, 10, by=2)]
#' x[-3]
#' x[-3:-5]
#'
"[.Dna" <- function(obj, index) {
  if (missing(index)) index <- as.ri(obj)
  obj$bits <-lapply(obj$bits, function(b) bit::as.bit(b[index]))
  obj
}

# ---------------------------------------------
#' @name [<-.Dna
#' @title Subscript replace method for Dna
#' @description replace subscript in Dna sequence
#' @param obj Dna object to be subscripted
#' @param index any indexing expression (except for negative indices - see details)
#' @param value a character string (recycled if necessary)
#' @details
#' if \code{value} is shorter than index range, then it is recycled.\cr
#' if \code{value} is larger than index range, then it is truncated.\cr
#'
#' \bold{negative indices} are not allowed here (since they are
#' interpreted as \bold{drop} see \link{[.Dna})\cr
#'
#' you cannot currently use this to \bold{insert} symbol within sequence (since
#' value is truncated - I'll improve this in next versions). For the moment,
#' use \link{c} instead.
#'
#' @seealso \link{[.Dna}
#' @examples
#' x <- Dna("acgtnacgtn")
#' x[1:5] <- "a"
#' x[5:1] <- "cga"
#' x[seq.int(1, 10, by=2)] <- "n"
#' 
"[<-.Dna" <- function(obj, index, value) {
  if (! is.character(value)) {
    lx.warn("rhs should be a character string")
    return(obj)
  }

  if (missing(index)) index <- as.ri(obj)

  # truncate or recycle string if needed
  value <- .adjust(tolower(value), length(index))

  ins <- Dna(value, obj$code, obj$pattern)
  obj$bits <- mapply(function(b, r) {b[index] <- r; b}, obj$bits, ins$bits)
  obj
}

# ---------------------------------------------
#' @name as.character
#' @title Coerce Dna to character string
#' @description Coerce Dna sequence to character string
#' @param x Dna object to be coerced to character
#' @param ... further arguments passed to or from other methods.
#' @examples
#' x <- Dna("acgtnacgtn")
#' as.character(x)
#'
as.character.Dna <- function(x, ...) {
  .decode(x)
}

# ---------------------------------------------
#' @name c
#' @title Catenate two or more Dna sequences
#' @description Catenate two or more Dna sequences
#' @param obj Dna object
#' @param ... Dna objects or character strings (may be mixed)
#' @return a Dna sequence
#' @examples
#' x <- Dna("acgtnacgtn")
#' c(x, "rryy", Dna("gg"))
#'
c.Dna <- function(obj, ...) {
  arg <- list(...)
  
  #
  # recurse if more than one dot argument
  if (length(arg) > 1)
    return(Reduce(c, arg, obj))

  #
  # case one argument
  arg <- arg[[1]]
  
  #
  # case character string
  if (is.character(arg))
    return(c(obj, Dna(arg, obj$code, obj$pattern)))
    
  #
  # case Dna object : we shuold force Dna re-encoding
  arg <- Dna(arg, obj$code, obj$pattern)
  obj$bits <- mapply(base::c, obj$bits, arg$bits)

  obj
}

# ---------------------------------------------
#' @name rev
#' @title Rev method for Dna
#' @description Reverse method for Dna
#' @param x Dna object
#' @return Dna sequence reversed (but not complemented)
#' @seealso \link{revcompl}
#' @examples
#' x <- Dna("acgtnry")
#' rev(x)
#'
rev.Dna <- function(x) {
  x$bits <-lapply(x$bits, function(b) bit::as.bit(rev(b)))
  x
}

# ---------------------------------------------
#' @name summary
#' @title Summary method for Dna
#' @description Make a summary of Dna sequence
#' @param object Dna object
#' @param ... additional arguments affecting the summary produced.
#'
summary.Dna <- function(object, ...) {
  hdr <- list(Length=length(object), Class=class(object), Code=object$code,
              Pattern=ifelse(is.null(object$pattern), "NULL",object$pattern))
  cnt <- countsymb(object, "acgt")
  len <- length(object)
  other <- list(other=len-sum(cnt))
  gc  <- countsymb(object, "S") * 100 / ifelse(len!=0, len, 1)
  gc  <- list("%GC"=sprintf("%.2f", gc))
  as.data.frame(c(hdr, cnt, other, gc), row.names="", optional=TRUE)
}

# ---------------------------------------------
#' @name plot
#' @title Plot method for Dna
#' @description
#' just for fun... this is for Jean ;-)
#' @param x Dna object
#' @param step walking step (in bp)
#' @param compass integer vector of length 4 giving the direction for (a c g t)
#' @param ... additional arguments to \link{plot}
#'
plot.Dna <- function(x, step=ceiling(length(x)/100),
                     compass=list(x=c(0L, -1L, 1L,  0L), y=c(1L, 0L, 0L, -1L)),
                     ...) {
  len <- length(x)
  xy <- sapply(seq.int(1, len, by=step), function(from) {
    sub <- subseq(x, from, from+step-1)
    tab <- (tab <- countsymb(sub, "acgt")) / max(sum(tab), 1)
    c(sum(tab * compass$x), sum(tab * compass$y))
  })
  
  .lab <- function(x, alpha=c("a", "c", "g", "t"))
    paste0(alpha[which(x==-1)], " <-> ", alpha[which(x==1)])

  plt <- plot(xy[1,], xy[2,], type="l",
              main=paste0("strand bias N=", len),
              xlab=.lab(compass$x), ylab=.lab(compass$y), ...)
  abline(v=0, h=0, col=2, lty=2)
  invisible(plt)
}

# ---------------------------------------------
# new methods
# ---------------------------------------------

# ---------------------------------------------
#' Generic method to coerce to range
#' @description
#' coerce to range-index (\link{ri}) object from library \link{bit}
#' @param obj object to coerce to \link[bit]{ri}
#' @seealso  \link[bit]{ri} in library \link{bit}, \link{as.ri.Dna}
#'
as.ri <- function(obj) {
    UseMethod("as.ri")
}

# ---------------------------------------------
#' Coerce Dna to ri range [1, length(dna)]
#' @inheritParams as.ri
#' @seealso \link{as.ri}, \link[bit]{ri}
#'
as.ri.Dna <- function(obj) {
  n <- length(obj)
  bit::ri(1, n, n)
}

# ---------------------------------------------
#' Coerce bit::ri to bit::ri
#' @description
#' just a trivial helper (absent from bit)
#' @inheritParams as.ri
#' 
as.ri.ri <- function(obj) {
  obj
}

# ---------------------------------------------
#' Generic method to coerce to Dna
#' @description
#' coerce character string or Dna object to Dna object\cr
#' this is basically the same as the \link{Dna} constructor.\cr
#' for Dna object this allows to force a re-encoding
#' @param obj object to coerce to Dna
#' @param code see \link{Dna}
#' @param pattern see \link{Dna}
#' @return Dna object
#' @seealso  \link{Dna}, \link{as.Dna.Dna}, \link{as.Dna.character}
#'
as.Dna <- function(obj, code, pattern)  {
    UseMethod("as.Dna")
}

# ---------------------------------------------
#' Coerce Dna to Dna
#' @description
#' see \link{as.Dna}\cr
#' this is basically used to force Dna reencoding.
#' @inheritParams as.Dna
#' @examples
#' x <- Dna("acgtacgt")
#' as.Dna(x, code="strict")
#'
as.Dna.Dna <- function(obj, code=obj$code, pattern=obj$pattern) {
  Dna(obj, code=code, pattern=pattern)
}

# ---------------------------------------------
#' Coerce character string to Dna
#' @description
#' see \link{as.Dna}\cr
#' this is basically the same as the \link{Dna} constructor.
#' @param obj character string to coerce to Dna
#' @param ... any argument to  \link{Dna}
#' @examples
#' as.Dna("acgtacgt", code="strict")
#'
as.Dna.character <- function(obj, ...) {
  Dna(obj, ...)
}

# ---------------------------------------------
#' Generic method to extract subsequence
#' @description
#' extract subsequence in range [from, to] (endpoints included)\cr
#' \code{to} may be omitted (in which case it equals the length of obj)\cr
#' \code{from} and \code{to} may be negative. they are interpreted as
#' \code{length - from + 1} or \code{length - to +1}.\cr
#' @param obj object to extract a subsequence
#' @param from start index (1:based, endpoint included)
#' @param to end index (1:based, endpoint included)
#' @seealso \link{subseq.Dna}, \link{subseq.character}
#' @note the term 'subsequence' is a misnommer, this is
#' actually a substring. So this function should be renamed
#' 'substr' or 'substring'.\cr
#' see \link{[.Dna} for an actual subsequence
#' 
subseq <- function(obj, from, to) {
    UseMethod("subseq")
}

# ---------------------------------------------
#' Extract Dna subsequence
#' @description
#' see \link{subseq}
#' @seealso \link{[.Dna}
#' @inheritParams subseq
#' @examples
#' x <- Dna("acgtnacgtn")
#' subseq(x, 1, 3)
#' subseq(x, 1, -3)
#'
subseq.Dna <- function(obj, from, to) {
  len <- length(obj)
  if (missing(to)) to <- len
  if (from < 0) from <- len + from + 1
  if (to <  0)  to <- len + to + 1
  rng <- bit::ri(min(max(from, 1), len), min(max(to, 1), len), len)
  obj$bits <-lapply(obj$bits, function(b) bit::as.bit(b[rng]))
  obj
}

# ---------------------------------------------
#' Extract subsequence from character string
#' @description
#' this is equivalent to \link{substring}
#' @inheritParams subseq
#'
subseq.character <- function(obj, from, to) {
  substring(text=obj, first=from, last=to)
}

# ---------------------------------------------
#' Generic method to complement a sequence
#' @description
#' just complement sequence (not reverse complement)
#' @param obj a Dna sequence to complement
#' @seealso \link{compl.Dna}, \link{revcompl}
#'
compl <- function(obj) {
    UseMethod("compl")
}

# ---------------------------------------------
#' Complement Dna sequence
#' @description
#' just complement sequence (not reverse complement)
#' @inheritParams compl
#' @seealso \link{revcompl.Dna}
#' @examples
#' x <- Dna("acgtnry")
#' compl(x)
#'
compl.Dna <- function(obj) {
  .complement(obj)
}

# ---------------------------------------------
#' Generic method to reverse complement Dna subsequence
#' @param obj a Dna sequence to reverse complement
#' @seealso \link{revcompl.Dna}, \link{compl}
#'
revcompl <- function(obj) {
    UseMethod("revcompl")
}

# ---------------------------------------------
#' Reverse Complement Dna subsequence
#' @inheritParams revcompl
#' @seealso \link{compl.Dna}
#' @examples
#' x <- Dna("acgtnry")
#' revcompl(x)
#'
revcompl.Dna <- function(obj) {
  rev(compl(obj))
}

# ---------------------------------------------
#' Generic method to count symbols in sequence
#' @param obj sequence object (usually \link{Dna})
#' @param symb a character string containing symbols to count
#' @return count table
#'
#' @seealso \link{countsymb.Dna} for Dna sequence
#'
countsymb <- function(obj, symb) {
    UseMethod("countsymb")
}

# ---------------------------------------------
#' Count symbols in Dna Sequence
#' @param obj a \link{Dna} sequence
#' @param symb a character string containing symbols to count
#' @note
#' To count using IUPAC degenerated codes, use uppercase
#' symbols (eg symb="W" will count g + c).
#' Therefore symb="r" will count the number of strict r's
#' whereas "R" will sum counts for r's and g's and a's.
#' @examples
#' seq <- paste(sample(c("a", "c", "g", "t"), 1e7, replace=TRUE), collapse="")
#' system.time(cnt <- length(gregexpr("a", seq)[[1]]))
#' x <- Dna(seq, "strict")
#' system.time(cnt <- countsymb(x, "a"))
#' system.time(tab <- countsymb(x))
#' countsymb(x, "W")
#' 
countsymb.Dna <- function(obj, symb="acgt") {
  symb <- unique(unlist(strsplit(symb, NULL)))
  sapply(symb, function(s) .count(obj, s))
}

# ---------------------------------------------
#' Generic method to compute Hamming distance between two sequences
#' @param obj1 first sequence object (usually \link{Dna})
#' @param obj2 second sequence object (usually \link{Dna})
#' @seealso \link{hamming.Dna} for Dna sequence
#'
hamming <- function(obj1, obj2) {
  UseMethod("hamming")
}

# ---------------------------------------------
#' Hamming distance between two Dna sequences
#' @param obj1 a \link{Dna} sequence
#' @param obj2 a \link{Dna} sequence of same size and encoding as obj1
#' @return number of differences between obj1 and obj2 or -1 if obj1 and obj2
#' are not of same size or encoding
#' @note
#' sequence case and IUPAC codes are ignored
#' @examples
#' hamming(Dna("acgtacgtacgt"), Dna("acggacggacgg"))
#' 
hamming.Dna <- function(obj1, obj2) {
  
  if ((length(obj1) != length(obj2)) || (obj1$code != obj2$code)) {
    lx.warn("incompatible sequences (length or code differ)")
    return(-1)
  }
  
  bits <- mapply(function(x, y) x != y, obj1$bits, obj2$bits)
  bits <- Reduce(function(x, y) x | y, bits[-1], bits[[1]])
  
  sum(bits)
}

