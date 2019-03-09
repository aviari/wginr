# -------------------------------------------------
# $Id: asdog.pdf.report.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# pdf reporters
#

# ---------------------------------------------
#' Generic method to report object as tex/pdf
#' @param x R object to be reported
#' @param fname a character string naming a file
#' @param params optional params for tex driver
#' @param ... specific arguments to reporting functions
#'
asdog.pdf.report <- function(x, fname, params=NULL, ...) {
  UseMethod("asdog.pdf.report")
}

#
# default implementation
#
asdog.pdf.report.default <- function(x, fname, params=NULL) {
  lx.stopif(TRUE, "no tex/pdf reporter for class: ", class(x))
}

#
# generic tex/pdf wrapper for various implementations 
#

.report.pdf <- function(x, fname, tex.reporter, params=x$params) {
  
  asdog.tex.driver(params)
  
  lx.out("pdf reporting to: '", fname, ".pdf'")
  
  res <- tryCatch({
    tex <- tex.open(fname)
    tex <- tex.reporter(tex, x)
    tex.close(tex, clean=FALSE)
  }, error=function(e) lx.warn(e$message))

  !is.logical(res)
}


#
# GCModel implementation
#
asdog.pdf.report.GCModel <- function(x, fname) {
  .report.pdf(x, fname, asdog.tex.report.gcmodel)
}

#
# GCCorrect implementation
#
asdog.pdf.report.GCCorrect <- function(x, fname) {
  .report.pdf(x, fname, asdog.tex.report.gccorrect)
}

#
# PLModel implementation
#
asdog.pdf.report.PLModel <- function(x, fname) {
  .report.pdf(x, fname, asdog.tex.report.plmodel)
}


#
# Segment implementation
#
asdog.pdf.report.Segment <- function(x, fname) {
  .report.pdf(x, fname, asdog.tex.report.segment)
}
