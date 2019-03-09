# -------------------------------------------------
# $Id: lx.strings.r 396 2019-01-02 22:53:10Z viari $
# lx strings utilities
#

# -------------------------------------------------
#' trim leading or trailing blanks (spaces or tabs)
#' in string
#' @param s string or string vector
#' @param side c('left', 'right', 'both'). which end to trim
#' @examples
#' paste(">", lx.strtrim(" the lazy dog  "), "<", sep="")
#
lx.strtrim <- function(s, side=c("both", "left", "right")) {
  side <- match.arg(side)
  pat <- switch(side, left="^( |\t)+", right="( |\t)+$", "^( |\t)+|( |\t)+$")
  gsub(pat, "", s)
}

# -------------------------------------------------
#' truncate string
#' @description
#' truncate long string as "abcd...wxyz", usually for pretty printing
#' @param s string
#' @param width maximum width
#' @param between between string
#' @examples
#' lx.strtrunc("the lazy dog", 6)
#
lx.strtrunc <- function(s, width, between="...") {
  slen <- nchar(s)
  width <- min(width, slen)
  if (nchar(between) >= width)
    between <- substr(between, 1, width-1)
  if (slen > width) {
    chk <- max(0, (width - nchar(between))/2)
    s <- paste0(substr(s, 1, ceiling(chk)), between, substr(s, slen-floor(chk)+1, slen))
  }
  substr(s, 1, width)
}


# -------------------------------------------------
#' split string around regexp
#' @description 
#' this is awrapper around \link{strsplit} to simplify
#' call when both s and split are simple strings.
#' @details
#' the type of result depends upon the length of s. if s is a vector (length > 1)
#' then this function behave exactly as \link{strsplit} and returns a list of
#' string vectors. if s is a simple string (as this is often the case) then this
#' function returns a simple string vector.
#' @param s string (or string vector)
#' @param split string (or string vector) containing regular expression(s)
#' (unless you specify \code{fixed = TRUE} in ...) to use for splitting.
#' see \link{strsplit}
#' @param trim also trim results using \link{lx.strtrim}
#' @param ... other argument to \link{strsplit}
#' @return vector of strings (or list of vectors of strings, see Details)
#' @examples
#' lx.strsplit("the lazy dog")
#' # is same as
#' strsplit("the lazy dog", " ")[[1]]
#' lx.strsplit(c("the lazy", "black dog"))
#' lx.strsplit("the lazy dog", "y")
#' lx.strsplit("the lazy dog", "y", fixed=TRUE)
# 
lx.strsplit <- function(s, split="( |\t)+", trim=TRUE, ...) {
  s <- strsplit(s, split=split, ...)
  if (trim) s <- lapply(s, lx.strtrim)
  if (length(s) == 1) s <- s[[1]]
  s
}

# -------------------------------------------------
#' quote special characters used in regex
#' @param s string or string vector
#' @examples
#' lx.regex.quote("the (lazy) dog")
#
lx.regex.quote <- function(s) {
  gsub("(\\(|\\)|\\[|\\]|\\.|\\+|\\*)", "\\\\\\1", s)
}

# -------------------------------------------------
#' reverse string
#' @param s string or string vector
#' @return vector of reversed strings
#' @examples
#' lx.strrev("the lazy dog")
#
lx.strrev <- function(s) {
  sapply(lapply(strsplit(s, NULL), rev), paste0, collapse="")
}

# -------------------------------------------------
#' find occurences of character(s) in string
#' @param s string where matches are sought
#' @param chr string containing any character to find
#' @return integer vector of positions where any char from chr is found or empty vector if no match
#' @seealso \code{\link{lx.gregexpr}}
#' @examples
#' lx.strchr("mississipi", "i")
#' lx.strchr("mississipi", "imsp")
#' lx.strchr("mississipi", "o")
#
lx.strchr <- function(s, chr="") {
  match.table <- logical(128)
  match.table[as.integer(charToRaw(chr))] <- TRUE
  which(match.table[as.integer(charToRaw(s))])
}

# -------------------------------------------------
#' gregexpr wrapper
#' @description
#' similar to \link{gregexpr} but argument \code{text} is a single character
#' string and function returns a (possibly empty) integer vector of match positions.
#' @param pattern character string containing a regular expression
#' @param text character string where matches are sought
#' @param ... any parameter of \link{gregexpr}
#' @return integer vector of positions where pattern is found or empty vector if no match
#' @note the order of parameters is the same as in \link{gregexpr}, i.e. pattern first
#' @seealso \code{\link{lx.strchr}}
#' @examples
#' lx.gregexpr("i", "mississipi")
#' lx.gregexpr("[imsp]", "mississipi")
#' lx.gregexpr("o", "mississipi")
#'
lx.gregexpr <- function(pattern, text, ...) {
  res <- gregexpr(pattern, text, ...)[[1]]
  if ((length(res) == 1) && (res == -1))
    res <- integer(0)
  attributes(res) <- NULL
  res
}
