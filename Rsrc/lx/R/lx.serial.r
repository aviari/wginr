# -------------------------------------------------
# $Id: lx.serial.r 396 2019-01-02 22:53:10Z viari $
# lx serialization utilities
#

# -------------------------------------------------
# internal utilities
# make dump filename
#
.lx.dump.path <- function(name, ext="rds") {
  paste(as.character(name), ext, sep=".")
}
.lx.save.path <- function(name, ext="RData") {
  .lx.dump.path(name, ext=ext)
}

# -------------------------------------------------
#' serialize single object into file
#' @param object object to serialize
#' @param name file name (without extension)
#'        if NULL then use symbol object
#' @param ... additionnal options to \code{\link{saveRDS}}
#' @details
#' object is serialized into file named \code{name.rds}
#' @seealso \code{\link{lx.unserialize}}
#' @examples
#' data(lx.iris)
#' tab <- lx.iris
#' lx.serialize(tab)  # create file \code{tab.rds}
#' duptab <- lx.unserialize("tab")
#
lx.serialize <- function(object, name=NULL, ...) {
  if (is.null(name)) name <- as.character(lx.args("object"))
  path <- .lx.dump.path(name)
  lx.out("Serializing : ", path)
  saveRDS(object=object, file=path)
}

# -------------------------------------------------
#' unserialize object from \code{name.rds}
#' @param name of object used in \code{lx.serialized}
#' @param ... additionnal options to \code{\link{readRDS}}
#' @seealso \code{\link{lx.serialize}}
#' @examples
#' data(lx.iris)
#' tab <- lx.iris
#' lx.serialize(tab)  # create file tab.Rdata
#' duptab <- lx.unserialize("tab")
#
lx.unserialize <- function(name, ...) {
  path <- .lx.dump.path(name)
  lx.out("Unserializing : ", path)
  readRDS(file=path, ...)
}

# -------------------------------------------------
#' tells if serialized object does exist
#' @param name of object used in \code{lx.serialized}
#' @examples
#' data(lx.iris)
#' tab <- lx.iris
#' lx.serialize(tab)  # create file tab.Rdata
#' lx.serialized("tab")
#' @seealso \code{\link{lx.serialize}} 
#
lx.serialized <- function(name) {
  path <- .lx.dump.path(name)
  file.exists(path)
}

# -------------------------------------------------
#' save objects into dump file
#' @param ... list of objects to save
#' @details the dump file name is <obj1>.RData
#'          where obj1 is the first object in \code{...}
#' @examples
#' data(lx.iris)
#' tab1 <- tab2 <- lx.iris
#' lx.save(tab1, tab2)
#' remove(tab1, tab2)
#' lx.restore("tab1")
#' @seealso \code{\link{lx.restore}}
#

lx.save <- function(...) {
  path <- .lx.save.path(as.character(lx.args(2)))
  lx.out("Saving : ", path)
  save(..., file=path, envir=.GlobalEnv)
}

# -------------------------------------------------
#' restore objects from dump file
#' @param name of first object used in \code{lx.save}
#' @seealso \code{\link{lx.save}}
#' @examples
#' data(lx.iris)
#' tab1 <- tab2 <- lx.iris
#' lx.save(tab1, tab2)
#' remove(tab1, tab2)
#' lx.restore("tab1")
#
lx.restore <- function(name) {
  path <- .lx.save.path(name)
  lx.out("Restoring : ", path)
  load(file=path, envir=.GlobalEnv)
}

# -------------------------------------------------
#' tells if object has been previously saved
#' @param name of first object used in \code{lx.save}
#' @examples
#' data(lx.iris)
#' tab <- lx.iris
#' lx.save(tab)
#' lx.saved("tab")
#' @seealso \code{\link{lx.save}}
#
lx.saved <- function(name) {
  path <- .lx.save.path(name)
  file.exists(path)
}

# -------------------------------------------------
#' erase previously saved or serialized object
#' @param name of first object used in \code{lx.save} or \code{lx.serialize}
#' @examples
#' data(lx.iris)
#' tab <- lx.iris
#' lx.save(tab)
#' lx.erase("tab")
#' @seealso \code{\link{lx.save} \link{lx.serialize}}
#
lx.erase <- function(name) {
  .erase <- function(path) {
    if (file.exists(path)) {
      lx.out("Erasing : ", path)
      file.remove(path)
    } else {
      FALSE
    }
  }
  ok1 <- .erase(.lx.save.path(name))
  ok2 <- .erase(.lx.dump.path(name))
  ok1 || ok2
}

# -------------------------------------------------
#' conditionaly restore or set object
#' @description
#' conditionaly restore or set object depending upon
#' previously saved value
#' @param object to set (existing or not)
#' @param setfun function to set object
#' @param ... arguments of setfun
#' @param force force reexecution of setfun even if dump file does exist
#' @details
#' if file <object>.RData does exist then restore value from this file
#' (unless force=TRUE) else execute setfun(...) and save result into dump
#' file for later retrieval.
#' @examples
#' foo <- function(n) sum(rnorm(n))  # dummy 
#' n <- 100000000
#' system.time(lx.lazy(mynorm, foo, n)) # 10 s
#' system.time(lx.lazy(mynorm, foo, n)) # <1 s
#'   
#
lx.lazy <- function(object, setfun, ..., force=FALSE) {
  varname <- as.character(lx.args("object"))
  funname <- as.character(lx.args("setfun"))
  path <- .lx.save.path(varname)
  if (! exists("dontCheck")) dontCheck <- identity  # for R < 3.1
  if ((! force) && file.exists(path)) {
    lx.out("Restoring : ", path)
    load(file=path)
    dontCheck(assign(varname, tmp, envir=.GlobalEnv))
  }
  else {
    tmp <- do.call(funname, list(...), envir=.GlobalEnv)
    dontCheck(assign(varname, tmp, envir=.GlobalEnv))
    lx.out("Saving : ", path)
    save(tmp, file=path)
  }
  lx.out("you can now use global variable '", varname, "'")
  invisible(TRUE)
}
