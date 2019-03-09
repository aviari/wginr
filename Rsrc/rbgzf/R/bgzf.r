#
# Bgzf API
#

# -------------------------------------------------
#' @name rbgzf
#' @docType package
#' @title BGZF file format R interface
#' @author Alain Viari
#' @details
#' \tabular{ll}{
#' Package: \tab rbgzf\cr
#' Type: \tab Package\cr
#' Version: \tab 1.0\cr
#' Date: \tab 2016-11-02\cr
#' License: \tab GPL\cr
#' }
#' @description
#' Bgzf (Blocked GNU Zip Format) is a variant of GZip format that allows
#' direct access into compressed file using offset in the uncompressed file.
#' The basic idea is to zip file by blocks (of 65K by default) and to maintain
#' an index between blocks offsets in compressed and uncompressed data.\cr
#' This R package provides a (thin) interface between R and the bgzf
#' implementation of the htslib library available at:\cr
#' \url{https://github.com/samtools/htslib}.
#' (appropriate LICENSE file is available in the src directory).\cr
#' The code has been slightly modified to remove dependencies with other
#' library components (SAM, CRAM, ...).\cr
#' Note: for WGInR users: WGInR contains an older (but compatible) version of bgzip.
#' the code for multithreading read operations has unfortunately not (yet) been transfered
#' from this version (to be done in future).
#'
NULL

#
# internal utilities
#

.BGZF.NULL  <- "0x0"
.BGZF.CLASS <- "BGZFPtr"
.BGZF.MAX.INT53 <- 2^53

# -------------------------------------------------
# new BGZFPtr object
#
.bgzf.new <- function(path, mode, ptr) {
  obj <- list(path=path, mode=mode, ptr=ptr)
  class(obj) <- c(.BGZF.CLASS, class(obj))
  obj
}

# -------------------------------------------------
# check BGZFPtr object
#
.bgzf.check <- function(obj) {
  if (! (.BGZF.CLASS %in% class(obj)))
    stop("bgzf should be an object of class ", .BGZF.CLASS)
}

#
# API
#

# -------------------------------------------------
#' print method for BGZFPtr object
#' @param x a BGZFPtr object
#' @param ... further arguments passed to or from other methods
#
print.BGZFPtr <- function(x, ...) {
  cat(sep="", "<BGZF file pointer>",
              " path='", x$path, "'",
              " mode='", x$mode, "'")
  invisible(x)
}

# -------------------------------------------------
#' check if a file is in the BGZF format
#' @param path file pathname
#' @return 1 if file is BGZF; 0 if not or on I/O error
#' @examples
#' path <- file.path(path.package(package="rbgzf"), "samples/test.bin.bgz")
#' bgzf.is.bgzf(path)
#
bgzf.is.bgzf <- function(path) {
  .C("C_bgzf_is_bgzf", as.character(path), status=integer(1))$status
}

# -------------------------------------------------
#' open bgzf file
#' @param path file pathname
#' @param mode open mode, one of "r" or "w"
#' @return BGZFPtr object or NULL on error
#' @examples
#' fh <- bgzf.open(file.path(path.package(package="rbgzf"), "samples/test.bin.bgz"))
#' bgzf.close(fh)
#
bgzf.open <- function(path, mode="r") {
  ptr <- .C("C_bgzf_open", as.character(path),
                           as.character(mode),
                           ptr=character(1))$ptr
  if (ptr == .BGZF.NULL) {
    warning("cannot open file: ", path, "in mode: ", mode)
    return(NULL)
  }

  .bgzf.new(path, mode, ptr)
}

# -------------------------------------------------
#' close bgzf file
#' @param bgzf BGZFPtr object created by \link{bgzf.open}
#' @return 0 on success and -1 on error
#' @examples
#' fh <- bgzf.open(file.path(path.package(package="rbgzf"), "samples/test.bin.bgz"))
#' bgzf.close(fh)
#
bgzf.close <- function(bgzf) {
  .bgzf.check(bgzf)
  .C("C_bgzf_close", as.character(bgzf$ptr), status=integer(1))$status
}

# -------------------------------------------------
#' read raw bytes from bgzf file
#' @param bgzf BGZFPtr object created by \link{bgzf.open}
#' @param nbytes double number of bytes to read
#' @return raw array of size nbytes (or less if EOF reached)
#' @note to overcome the 32 bits integer limitation, nbytes is interpreted
#' as a double. this is valid up to 53 bits (i.e 9007199254740992), larger
#' values will result in incorrect number of bytes read.
#' @examples
#' fh <- bgzf.open(file.path(path.package(package="rbgzf"), "samples/test.bin.bgz"))
#' x <- bgzf.read(fh, 128)
#' identical(as.integer(x[(1:8)%%8==1]), 0:15)
#' bgzf.close(fh)
#
bgzf.read <- function(bgzf, nbytes) {
  .bgzf.check(bgzf)
  nbytes <- as.double(nbytes)
  if (nbytes > .BGZF.MAX.INT53)
    warning("nbytes too large (> 2^53)")
  res <- .C("C_bgzf_read", as.character(bgzf$ptr), nbytes,
                           buffer=raw(nbytes), nread=double(1))
  head(res$buffer, res$nread)
}

# -------------------------------------------------
#' write raw bytes to bgzf file
#' @param bgzf BGZFPtr object created by \link{bgzf.open}
#' @param data array of raw data
#' @return number of bytes written (negative on error)
#' @examples
#' tmp <- tempfile()
#' fh <- bgzf.open(tmp, "w")
#' bgzf.write(fh, as.raw((1:1024)%%256))
#' bgzf.flush(fh)
#' bgzf.close(fh)
#' fh <- bgzf.open(tmp, "r")
#' x <- bgzf.read(fh, 255)
#' identical(as.integer(x), 1:255)
#' bgzf.close(fh)
#' unlink(tmp)
#
bgzf.write <- function(bgzf, data) {
  .bgzf.check(bgzf)
  if (! is.raw(data))
    stop("data should be raw")
  .C("C_bgzf_write", as.character(bgzf$ptr), data, as.double(length(data)),
                     nwrite=double(1))$nwrite
}

# -------------------------------------------------
#' flush bgzf file
#' @param bgzf BGZFPtr object created by \link{bgzf.open}
#' @return 0 on success and -1 on error
#
bgzf.flush <- function(bgzf) {
  .bgzf.check(bgzf)
  .C("C_bgzf_flush", as.character(bgzf$ptr), status=integer(1))$status
}

# -------------------------------------------------
#' tell byte position in uncompressed file
#' @param bgzf BGZFPtr object created by \link{bgzf.open}
#' @return the current offset in uncompressed file on success and -1 on error
#' @note the file should have been indexed (at least up to current position)
#' @seealso \link{bgzf.reindex}, \link{bgzf.index.load}
#
bgzf.utell <- function(bgzf) {
  .bgzf.check(bgzf)
  .C("C_bgzf_utell", as.character(bgzf$ptr), res=double(1))$res
}

# -------------------------------------------------
#' position file at uncompressed file offset
#' @param bgzf BGZFPtr object created by \link{bgzf.open}
#' @param uoffset offset in uncompressed file
#' @return 0 on success and -1 on error
#' @note the file should have been indexed (at least up to requested position)
#' @note to overcome the 32 bits integer limitation, uoffset is interpreted
#' as a double. this is valid up to 53 bits (i.e 9007199254740992), larger
#' values may result in incorrect position.
#' @seealso \link{bgzf.reindex}, \link{bgzf.index.load}
#' @examples
#' fh <- bgzf.open(file.path(path.package(package="rbgzf"), "samples/test.bin.bgz"))
#' bgzf.reindex(fh)
#' bgzf.useek(fh, 1024)
#' x <- bgzf.read(fh, 255)
#' identical(as.integer(x[(1:8)%%8==1])-127L, 1:32)
#' bgzf.close(fh)
#
bgzf.useek <- function(bgzf, uoffset) {
  .bgzf.check(bgzf)
  uoffset <- as.double(uoffset)
  if (uoffset > .BGZF.MAX.INT53)
    warning("offset too large (> 2^53)")
  .C("C_bgzf_useek", as.character(bgzf$ptr), as.double(uoffset), status=integer(1))$status
}

# -------------------------------------------------
#' setup indexing
#' @description this instructs bgzf to build index while compressing or uncompressing file.
#' @note this function is mostly used when creating a file for writing. when reading from an existing
#' file you may consider using \link{bgzf.reindex} instead.
#' @param bgzf BGZFPtr object created by \link{bgzf.open}
#' @return 0 on success and -1 on error
#' @seealso \link{bgzf.reindex}, \link{bgzf.index.load}
#
bgzf.index.build.init <- function(bgzf) {
  .bgzf.check(bgzf)
  .C("C_bgzf_index_build_init", as.character(bgzf$ptr), status=integer(1))$status
}

# -------------------------------------------------
#' tell if index is present
#' @param bgzf BGZFPtr object
#' @return logical TRUE if index is present
#' @note this function just check for the index presence not its
#' validity
#' @examples
#' fh <- bgzf.open(file.path(path.package(package="rbgzf"), "samples/test.bin.bgz"))
#' bgzf.has.index(fh)
#' bgzf.reindex(fh)
#' bgzf.has.index(fh)
#' bgzf.close(fh)
#
bgzf.has.index <- function(bgzf) {
  .bgzf.check(bgzf)
  .C("C_bgzf_has_index", as.character(bgzf$ptr), status=integer(1))$status == 1
}

# -------------------------------------------------
#' remove index
#' @param bgzf BGZFPtr object created by \link{bgzf.open}
#' @return 0 on success and -1 on error
#
bgzf.index.remove <- function(bgzf) {
  .bgzf.check(bgzf)
  .C("C_bgzf_index_remove", as.character(bgzf$ptr), status=integer(1))$status
}

# -------------------------------------------------
#' force reindexing whole bgzf file
#' @description
#' this function (re)index bgzf file by performing the following actions:\cr
#' \itemize{
#'  \item remove previous index (if any) and init new index
#'  \item rewind file to origin
#'  \item read by blocks (this force indexing while uncompressing)
#'  \item reposition to initial offset
#' }
#' @param bgzf BGZFPtr object created by \link{bgzf.open}
#' @return 0 on success and -1 on error
#' @note this procedure is quite quick (about 1Gb of uncompressed data per second).
#' So, for files of moderate size it is therefore not useful to save the
#' index on disk. For very large files (> 500 Gb) \link{bgzf.index.load} could
#' be more advantageous.
#' @examples
#' fh <- bgzf.open(file.path(path.package(package="rbgzf"), "samples/test.bin.bgz"))
#' bgzf.reindex(fh)
#' bgzf.useek(fh, 1024)
#' x <- bgzf.read(fh, 255)
#' identical(as.integer(x[(1:8)%%8==1])-127L, 1:32)
#' bgzf.close(fh)
#
bgzf.reindex <- function(bgzf) {
  .bgzf.check(bgzf)
  .C("C_bgzf_reindex", as.character(bgzf$ptr), status=integer(1))$status
}

# -------------------------------------------------
#' load index from file
#' @param bgzf BGZFPtr object created by \link{bgzf.open}
#' @param path path to index file
#' @return 0 on success and -1 on error
#' @seealso \link{bgzf.reindex} for a dynamic version. \link{bgzf.index.dump}
#' @examples
#' fh <- bgzf.open(file.path(path.package(package="rbgzf"), "samples/test.bin.bgz"))
#' bgzf.reindex(fh)
#' tmp <- tempfile()
#' bgzf.index.dump(fh, tmp)
#' bgzf.index.remove(fh)
#' bgzf.index.load(fh, tmp)
#' bgzf.useek(fh, 1024)
#' x <- bgzf.read(fh, 255)
#' identical(as.integer(x[(1:8)%%8==1])-127L, 1:32)
#' bgzf.close(fh)
#' unlink(tmp)
#
bgzf.index.load <- function(bgzf, path) {
  .bgzf.check(bgzf)
  .C("C_bgzf_index_load", as.character(bgzf$ptr), as.character(path),
     status=integer(1))$status
}

# -------------------------------------------------
#' dump index to file
#' @param bgzf BGZFPtr object created by \link{bgzf.open}
#' @param path path of index file
#' @return 0 on success and -1 on error
#' @seealso \link{bgzf.reindex} for a dynamic version. \link{bgzf.index.dump}
#' @examples
#' fh <- bgzf.open(file.path(path.package(package="rbgzf"), "samples/test.bin.bgz"))
#' bgzf.reindex(fh)
#' tmp <- tempfile()
#' bgzf.index.dump(fh, tmp)
#' bgzf.index.remove(fh)
#' bgzf.index.load(fh, tmp)
#' bgzf.useek(fh, 1024)
#' x <- bgzf.read(fh, 255)
#' identical(as.integer(x[(1:8)%%8==1])-127L, 1:32)
#' bgzf.close(fh)
#' unlink(tmp)
#
bgzf.index.dump <- function(bgzf, path) {
  .bgzf.check(bgzf)
  .C("C_bgzf_index_dump", as.character(bgzf$ptr), as.character(path),
     status=integer(1))$status
}

# -------------------------------------------------
# attach
#
.onAttach <- function(libname, pkgname) {
  packageStartupMessage('+ attaching ', pkgname)
}
