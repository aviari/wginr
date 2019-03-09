#!/usr/bin/env Rscript
# $Id: asdog.patch.1.x_2.0.r 396 2019-01-02 22:53:10Z viari $
#
# Asdog - patch 2.0 to 2.x objects
#
# * this script is for development only  *
# * it shoud not be part of distribution *
#

suppressMessages(library(asdog))

prog.usage <- function() {
  .cat <- function(...) cat(..., '\n', file=stderr())
  .cat("       asdog.patch.2.0_2x.r --file=<rds_file>")
  .cat("usage: asdog.patch.2.0_2x.r --dir=<directory>")
}

#
# class patch
#

patch.class <- function(obj) {
  if (! ("Asdog" %in% class(obj))) {
    class(obj) <- c(class(obj)[1], "Asdog", class(obj)[-1])
  }
  obj
}

#
# version patch
#

patch.version <- function(obj) {
  if (! (".version" %in% names(obj))) {
    obj$.version <- asdog.version()
    lx.info(obj$.version) <- "asdog version number"
  }
  obj
}

##
## Main
##

#
# get arguments
#

args <- asdog.main.args(NULL)

if ((! is.character(args$dir)) && (! is.character(args$file))) {
  asdog.main.exit(1, usage=TRUE)
}

if (is.character(args$dir)) {
  files <- list.files(args$dir, ".*.rds$", full.names=T)
} else {
  files <- asdog.check.file(args$file, "rds", .bgz=NULL)
}

for (f in files) {
  lx.out("reading ", f)
  obj <- tryCatch(readRDS(file=f), error=function(e) NULL)
  if (is.null(obj)) {
    lx.warn("cannot read rds file ", f)
    next
  }
  lx.out("patching object of class ", class(obj))
  obj <- patch.class(obj)
  obj <- patch.version(obj)
  lx.out("writing back to file ", f)
  saveRDS(obj, f)
}

#
# end
#

asdog.main.exit(0)
