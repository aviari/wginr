# -------------------------------------------------
# $Id: mdb.base.r 321 2017-08-18 11:10:19Z viari $
# lx generic databases utilities
#

# =================================================
# database querying
#
# the main function is 'lx.mdb.find' that grep
# pattern in the specifed records key of db
#
# db can be either a flat db or a structured db
# in which case the actual db used is db$db
#
# usually pattern is a regex expression
#
# paramter 'full.match' will force the match to span
# all the key value
# this is equivalent to '^pattern$'
# but when used with regex=FALSE && ignore.case=FALSE the engine may use
# precompiled entries for speedup
#

# -------------------------------------------------
# internal utility

.mdb.query <- function(db, fun) {
  names(which(lapply(db, fun)==TRUE))
}

# -------------------------------------------------
#' grep pattern in specified records of database
#' @param db a flat db or a structured db (in the later case the actual db used is db$db)
#' @param key record key to search in
#' @param pat pattern to search for 
#' @param regex pat is a regular expression
#' @param ignore.case ignore case during search
#' @param full.match pattern should span all the key value.
#'        equivalent to '^pattern$' but when used with regex=FALSE and ignore.case=FALSE
#'        the engine may use precompiled entries for speedup
#' @return vector (possible of 0 length) of records id (as strings)
#' @seealso \code{\link{mdb.swiss.load}} \code{\link{mdb.obo.load}}
#' @examples
#' db <- mdb.swiss.load(lx.system.file("samples/test_swiss", "xlx"))
#' mdb.find(db, "KW", "gluconate", ignore.case=TRUE)
#
mdb.find <- function(db, key, pat, regex=TRUE, ignore.case=FALSE, full.match=FALSE) {
  if (   full.match 
      && (! regex) 
      && (! ignore.case) 
      && (! is.null(db$compile[[key]])) ) { # speedup
      return(db$compile[[key]]$rindex[[pat]])
  }
  # perform normal search
  if (! regex)    pat <- lx.regex.quote(pat) # quote special chars
  if (full.match) pat <- paste("^", pat, "$", sep="") # add borders
  fun <- function(rec) {
    any(grepl(pat, rec[[key]], ignore.case=ignore.case))
  }
  .mdb.query(if (is.null(db$db)) db else db$db, fun)
}
