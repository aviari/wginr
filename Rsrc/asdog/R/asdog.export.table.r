# -------------------------------------------------
# $Id: asdog.export.table.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# export tables
#

# ---------------------------------------------
#' Generic method to export object as table
#' @param x R object to be exported
#' @param file either a character string naming a file or a connection
#' open for writing. "" indicates output to the console.
#' @param ... arguments to \link{write.table}
#'
asdog.write.table <- function(x, file, ...) {
  UseMethod("asdog.write.table")
}

#
# default implementation
#
asdog.write.table.default <- function(x, file, quote=FALSE, row.names=FALSE, .scipen=10, ...) {
  #
  # force writing integers less than 10^.scipen with all digits,
  # this is necessary for coordinates
  #
  oldsci <- options('scipen')[[1]]
  options(scipen=.scipen)

  #
  # use write.table
  #
  write.table(x, file, quote=quote, row.names=row.names, ...)

  #
  # restore scipen
  #
  options(scipen=oldsci)
  
  invisible()
}

#
# GCCorrect implementation
#
asdog.write.table.GCCorrect <- function(x, file, ...) {
  
  hsize <- floor(x$params$gccorrect.binsize/2)
  
  tab <- lx.napply(x$bychr, function(chr, cnd) {
    data.frame(chr      = chr,
               from     = cnd$pos-hsize,
               to       = cnd$pos+hsize-1,
               gc       = sprintf("%.3f", cnd$gc),
               cov.raw  = sprintf("%.1f", cnd$raw),
               cov.pred = sprintf("%.1f", cnd$pred),
               rrc.raw  = sprintf("%.3f", cnd$rrc.raw),
               rrc.cor  = sprintf("%.3f", cnd$rrc.cor),
               stringsAsFactors=F)
  }, SIMPLIFY=F, USE.NAMES=F, use.threads=F)
  
  tab <- do.call(rbind, tab)
  
  asdog.write.table(tab, file, ...)
}

#
# Segment implementation
#
asdog.write.table.Segment <- function(x, file, what=c("rcaf", "rrc"), ...) {
  
  what <- match.arg(what)
  
  tab <- x[[paste0(what, ".segs")]]
  
  fmt <- c(lev.rrc="%.3f",  lev.baf="%.3f", ppm.mark="%.3f",
           med.rrc="%.3f",  mad.rrc="%.3f",
           zscor.rrc="%.3f", pval.rrc="%.2e",
           med.cn="%.3f", 
           med.baf="%.3f", mad.baf="%.3f",
           zscor.baf="%.3f", pval.baf="%.2e")
  
  for (nam in names(tab)) {
    if (nam %in% names(fmt))
      tab[,nam] <- sprintf(fmt[[nam]], tab[,nam])
  }
  
  tab <- tab[order(tab$chr, tab$from, tab$to),]

  asdog.write.table(tab, file, ...)
}

