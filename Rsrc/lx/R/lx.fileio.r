# -------------------------------------------------
# $Id: lx.fileio.r 396 2019-01-02 22:53:10Z viari $
# lx file io utilities
#

# -------------------------------------------------
#' @name HELP.FILE.HANDLE
#' @docType data
#' @title lx binary file handle
#' @description
#' a file \code{handle} (called handle for short) 
#' is an R object (see notes) wrapping a binary file connection.
#' (do not confuse with a \link{HELP.FILE.HANDLER})\cr
#' It is intended to provide a uniform interface for different
#' file types within \bold{lx} and its extensions.
#'
#' a \code{handle} contains four mandatory fields:
#' \itemize{
#'  \item filename : a character string containing the file name (or url)
#'  \item type     : a character string containing the file content type (default is "none")
#'  \item mode     : a character string containing the file opening mode in \link{lx.open}
#'  \item connect  : an R object containing the file physical connection, the type depends upon file type
#'  \item handler  : a file handler object. see \link{HELP.FILE.HANDLER}
#' }
#'
#' and some optional fields:
#' \itemize{
#'  \item header   : file specific information (header, e.g. index table)
#' }
#' 
#' \code{handle}s are openened and closed thru
#' \link{lx.open} and \link{lx.close} functions.\cr
#'
#' files are opened as binary and lx provides the following
#' binary IO functions to read fixed length bytes from them.
#' \itemize{
#'  \item \link{lx.read.int8}  : 8 bits integer(s)
#'  \item \link{lx.read.int16} : 16 bits integer(s)
#'  \item \link{lx.read.int32} : 32 bits integer(s)
#'  \item \link{lx.read.int64} : 64 (actually 53) bits integer(s)
#'  \item \link{lx.read.string} : basta string
#' }
#'
#' @note
#' in practice a FILE.HANDLE is currently implemented as an R environment
#' @note
#' no write operations yet... time is short.
#' 
NULL

# -------------------------------------------------
#' @name HELP.FILE.HANDLER
#' @docType data
#' @title lx binary file handler
#' @description
#' a file \code{handler} (called handler for short) 
#' is an R object (see notes) wrapping binary file IO operations
#' (do not confuse with a \link{HELP.FILE.HANDLE}).\cr
#'
#' a \code{handler} should implement all the following operations:
#' \itemize{
#'  \item accept   : tell if file can be handled by this handler
#'  \item open     : open file
#'  \item close    : close file
#'  \item seek     : tell/seek into file
#'  \item read     : read n bytes from file
#' }
#' API for these functions:\cr
#' \preformatted{
#'   accept(filename, mode)
#'     filename: character string, name of file (see base::open)
#'     mode: character string, open mode (see base::open)
#'   returns logical TRUE if the file can be handled by this handler
#'     
#'   open(filename, mode)
#'     filename: character string, name of file (see base::open)
#'     mode: character string, open mode (see base::open)
#'   returns physical connection
#'   
#'   close(con)
#'     con: physical connection
#'   returns (ignored)
#'   
#'   seek(con, ...)
#'     con: physical connection
#'     ...: optional position (see lx::lx.seek)
#'   returns current position in file as a (numeric) byte offset from the origin
#'   
#'   read(con, n=1L)
#'     con: physical connection
#'     n: integer The (maximal) number of bytes to be read
#'   returns a raw vector of at most n bytes (less if EOF reached)
#' }
#' @note 
#' handlers have to be registered thru \link{lx.register.file.handler}.\cr
#' \code{lx} maintain a stack of handlers that are used from top to bottom
#' when opening a file (thru \link{lx.open}). the first handler accepting
#' the filename argument (thru \code{accept}) is selected.\cr
#' by default, a R binary file (accepting anything) is registered at the lowest
#' stack position with name "default".
#' @note
#' in practice a FILE.HANDLER is currently implemented as an R environment
#' 
NULL

# -------------------------------------------------
# <internal> default file handler
#
.dft.handler <- function(name) {
  h <- new.env(hash=TRUE, parent=emptyenv())
  h$name    <- name
  h$accept  <- function(...) TRUE # accept anything
  h$open    <- function(filename, mode) file(filename, open=mode,
                                             blocking=TRUE, encoding="",
                                             raw=FALSE)
  h$close    <- close
  h$seek     <- seek
  h$read     <- compiler::cmpfun(function(con, n=1L) readBin(con=con, what="raw", n=n))
  h
}

# -------------------------------------------------
# <internal> lookup file handler by name or accept
#
.lookup.handler <- function(name, mode=NULL) {
  for (h in .lx.env$file.handlers) {
    if (if (is.null(mode)) (h$name %in% name) else h$accept(name, mode))
      return(h)
  }
  return()
}

# -------------------------------------------------
#' create a default file handler
#' @description the default file handler handles usual R
#' binary files.
#' @param name handler name
#' @details a default handler (with name "default") is always 
#' added at the lowest position in handlers stack. So, usually
#' you don't need to call this function. It is mostly intented
#' for the case where you want to design your own handler, not
#' starting from scratch. A typical case is to create a default
#' R file handler and to override the \code{accept} function to
#' specify which files to accept.
#
lx.default.file.handler <- function(name) {
  .dft.handler(name)
}

# -------------------------------------------------
#' register handler
#' @description register handler at topmost position in handlers stack
#' @param handler : file handler
#' @note if a handler with same name already exists in stack it is first
#'       removed and the new handler is added topmost.
#' 
lx.register.file.handler <- function(handler) {
  h <- .lookup.handler(handler$name)
  if (! is.null(h)) lx.remove.file.handler(h$name)
  .lx.env$file.handlers <- c(list(handler), .lx.env$file.handlers)
  invisible(handler)
}

# -------------------------------------------------
#' remove file handler(s)
#' @description remove handler from handlers stack
#' @param name : name(s) of handler(s) to remove
#' @return NULL
#
lx.remove.file.handler <- function(name) {
  indx <- which(sapply(.lx.env$file.handlers, function(h) h$name %in% name))
  .lx.env$file.handlers[indx] <- NULL
  invisible()
}

# -------------------------------------------------
#' open (binary) file
#' @param filename file name
#' @param mode open mode (see \link{HELP.FILE.HANDLE})
#' @param .handler force use of given handler name (internal use only)
#' @return file handle
#' @note if you force handler (thru .handler) the handler$accept test function
#' will not be called.
#' @seealso \link{HELP.FILE.HANDLE}
#'
lx.open <- function(filename, mode="rb", .handler=NULL) {
  handle <- new.env(hash=TRUE, parent=emptyenv())
  handle$filename <- filename
  handle$type <- "none"
  handle$mode <- mode
  handle$handler <- if (is.null(.handler)) .lookup.handler(filename, mode)
                    else .lookup.handler(.handler)
  if (is.null(handle$handler))
    stop("cannot find handler for file: ", filename)
  lx.out("file handler: ", handle$handler$name, level="debug")
  handle$connect <- handle$handler$open(filename, mode)
  handle$is.open <- ! is.null(handle$connect)
  class(handle) <- c("LXFileHandle", class(handle))
  handle
}

# -------------------------------------------------
#' close file handle
#' @param handle file handle (opened by \link{lx.open})
#' @return handle (modified)
#' @note you should reassign handle on return since
#' some internal fields have been modified
#'
lx.close <- function(handle) {
  if (handle$is.open)
    handle$handler$close(handle$connect)
  handle$is.open <- FALSE
  handle
}

# -------------------------------------------------
#' tell/seek file
#' @description
#' reposition file handle opened by \link{lx.open}
#' @details
#' if ... is absent then returns current position (ftell) else
#' set the position to (numeric) argument.
#' @param handle file handle (opened by \link{lx.open})
#' @param ... optional position in file
#' @return current position (before any move) as a (numeric) byte offset from the origin.
#' @note
#' for default (R) handler, position may exceed 32 bit integer.
#' @examples
#' fh <- lx.open(lx.system.file("samples/test_bin.bst"))
#' cod  <- lx.read.int32(fh, n=1)
#' pos <- lx.seek(fh)
#' nseq <- lx.read.int32(fh, n=1)
#' lx.seek(fh, 0)
#' cod  <- lx.read.int32(fh, n=1)
#' nseq <- lx.read.int32(fh, n=1)
#' lx.seek(fh, pos)
#' nseq <- lx.read.int32(fh, n=1)
#' lx.close(fh)
#'
lx.seek <- function(handle, ...) {
  handle$handler$seek(handle$connect, ...)
}

# -------------------------------------------------
#' rewind file handle
#' @description
#' rewind file handle opened by \link{lx.open}
#' @param handle file handle (opened by \link{lx.open})
#' @return handle (invisible)
#' @note
#' this is a shorthand for \code{lx.seek(handle, 0)}
#'
lx.rewind <- function(handle) {
  lx.seek(handle, 0)
}

# -------------------------------------------------
#' duplicate file handle
#' @description
#' duplicate file handle with a new physical connection
#' @details
#' this is mostly used in multithreading when accessing the same file
#' (in read mode) by multiple threads.
#' @param handle file handle (opened by \link{lx.open})
#' @return duplicated file handle
#'
lx.dup.handle <- function(handle) {
  dup <- lx.open(handle$filename, handle$mode, .handler=handle$handler$name)
  handle$connect <- dup$connect
  handle
}

# -------------------------------------------------
#' read binary 8 bits integer(s)
#' @param handle file handle (opened by \link{lx.open})
#' @param n number of integers to read
#' @param signed read signed integer(s)
#' @return (vector of) integer(s)
#' @note n is an integer, therefore limited to 2,147,483,647
#' @examples
#' fh <- lx.open(lx.system.file("samples/test_bin.bst"))
#' x <- lx.read.int8(fh, n=4)
#' lx.close(fh)
#
lx.read.int8 <- function(handle, n=1L, signed=FALSE) {
  # this is quicker than readBin
  r <- as.integer(handle$handler$read(handle$connect, n=n))
  if (signed)
    r[r > 0x7FL] <- r[r > 0x7FL] - 0x100L
  r
}

# -------------------------------------------------
#' read binary 16 bits integer(s)
#' @param handle file handle (opened by \link{lx.open})
#' @param n number of integers to read
#' @param signed read signed integer(s)
#' @param little.endian integer encoding is little endian
#' @return (vector of) integer(s)
#' @note n is an integer, therefore limited to 2,147,483,647
#' @examples
#' fh <- lx.open(lx.system.file("samples/test_bin.bst"))
#' x <- lx.read.int16(fh, n=2)
#' lx.close(fh)
#
lx.read.int16 <- function(handle, n=1L, signed=FALSE, little.endian=TRUE) {
  
  r <- as.integer(handle$handler$read(handle$connect, n=2L*n))
  
  # explicit conversion is quicker than readBin
  
  if (little.endian) {
    s0 <- r[c(T,F)]
    s1 <- r[c(F,T)]
  } else {
    s0 <- r[c(F,T)]
    s1 <- r[c(T,F)]
  }
  
  if (signed)
    s1[s1 > 0x7FL] <- s1[s1 > 0x7FL] - 0x100L
  
  s0 + 0x100L * s1
}

# -------------------------------------------------
#' read binary 32 bits integer(s)
#' @param handle file handle (opened by \link{lx.open})
#' @param n number of integers to read
#' @param signed read signed integer(s)
#' @param little.endian integer encoding is little endian
#' @return (vector of) integer(s) if signed == TRUE
#' else (vector of) double(s) without loss of precision
#' @note n is an integer, therefore limited to 2,147,483,647
#' @examples
#' fh <- lx.open(lx.system.file("samples/test_bin.bst"))
#' cod  <- lx.read.int32(fh, n=1)
#' nseq <- lx.read.int32(fh, n=1)
#' s1 <- lx.read.string(fh)
#' siz1 <- lx.read.int64(fh, n=1) # 24
#' crc1 <- lx.read.int32(fh, n=1)
#' s2 <- lx.read.string(fh)
#' siz2 <- lx.read.int64(fh, n=1) # 32
#' crc2 <- lx.read.int32(fh, n=1)
#' ss1 <- lx.read.char(fh, n=siz1) # "ACGTACGTACGTAAAAACGTACGT"
#' ss2 <- lx.read.char(fh, n=siz2) # "GCGCGCGCGCGCGCGCGCGCGCGCTTTTTATATATATAX"
#' lx.close(fh)
#'
lx.read.int32 <- function(handle, n=1L, signed=FALSE, little.endian=TRUE) {

  r <- handle$handler$read(handle$connect, n=4L*n)
  
  # readBin is quicker than explicit conversion
  r <- readBin(r, "integer", size=4, signed=TRUE, n=n,
               endian=ifelse(little.endian, "little", "big"))

  if (! signed) {
    r <- as.double(r)
    r[r < 0] <- r[r < 0] + 0x100000000
  }
  r  
}

# -------------------------------------------------
#' read binary 64 (actually 53) bits integer
#' @param handle file handle (opened by \link{lx.open})
#' @param n number of integers to read
#' @param signed read signed integer(s)
#' @param little.endian integer encoding is little endian
#' @return (vector of) double(s) with possible precision loss.
#' @note
#' since R double have 53 bits precision the return
#' values are only exact for values in range
#' \code{[-2^53, 2^53] = [-9007199254740992,9007199254740992] for signed long}
#' and \code{[0, 2^53] = [0, 9007199254740992] for unsigned long}
#' @note n is an integer, therefore limited to 2,147,483,647
#' @examples
#' fh <- lx.open(lx.system.file("samples/test_bin.bst"))
#' cod  <- lx.read.int32(fh, n=1)
#' nseq <- lx.read.int32(fh, n=1)
#' s1 <- lx.read.string(fh)
#' siz1 <- lx.read.int64(fh, n=1)
#' crc1 <- lx.read.int32(fh, n=1)
#' s2 <- lx.read.string(fh)
#' siz2 <- lx.read.int64(fh, n=1)
#' crc2 <- lx.read.int32(fh, n=1)
#' ss1 <- lx.read.char(fh, n=siz1)
#' ss2 <- lx.read.char(fh, n=siz2)
#' lx.close(fh)
#
lx.read.int64 <- function(handle, n=1L, signed=FALSE, little.endian=TRUE) {

  r <- lx.read.int32(handle, n=2*n, signed=FALSE, little.endian=little.endian)
  
  if (little.endian) {
    s0 <- r[c(T,F)]
    s1 <- r[c(F,T)]
  } else {
    s0 <- r[c(F,T)]
    s1 <- r[c(T,F)]
  }

  if (signed)
    s1[s1 > 0x7FFFFFFF] <- s1[s1 > 0x7FFFFFFF] - 0x100000000
  
  s0 + s1 * 0x100000000 
}

# -------------------------------------------------
#' read character(s)
#' @description
#' read characters from file handle stopping or not on
#' NULL chars.
#' @param handle file handle (opened by \link{lx.open})
#' @param n integer, (maximum) number of chars to read
#' @param skip.null logical, skip over NULL (see details)
#' @param .raw logical, skip processing (see details)
#' @param .chunk.size (internal parameter) size of chunks to use
#'        when skip.null==FALSE and n < 0 
#' @return character string
#' @details
#' \preformatted{
#' if skip.null==FALSE (default)
#'   if (n >= 0) then input will stop either when
#'               n chars have been read or a NULL character is found
#'               or EOF has been reached.
#'   if (n < 0) then input will stop on first NULL character
#'              (or EOF).
#' if skip.null==TRUE
#'    read will skip over NULL chars until either
#'    n chars have been read (not counting NULLs)
#'    or EOF has been reached.
#' 
#' .raw == TRUE assume that the input stream contains
#' exactly n bytes without embedded NULLs. this will skip
#' all checking and processing (ignoring skip.null parameter).
#' this is intended for high speed reading when you know exactly
#' what is currently in stream.
#' }
#' @note n is an integer, therefore limited to 2,147,483,647
#' @seealso \link{lx.read.int8} to read bytes (including NULL),
#' \link{lx.read.string} as an alternative form.
#' @examples
#' fh <- lx.open(lx.system.file("samples/test_bin.bst"))
#' s <- lx.read.char(fh, n=40)
#' nchar(s)  # 5
#' lx.rewind(fh)
#' s <- lx.read.char(fh, n=40, skip=TRUE)
#' nchar(s, type="bytes")  # 40
#' lx.close(fh)
#'
lx.read.char <- function(handle, n=1L, skip.null=FALSE, .raw=FALSE, .chunk.size=100L) {
  if (.raw) 
    r <- rawToChar(handle$handler$read(handle$connect, n=max(0, n)))
  else if (skip.null) {
    r  <- handle$handler$read(handle$connect, n=max(0, n))
    nr <- length(r)
    r  <- r[r!=0]
    nc <- length(r)
    r  <- rawToChar(r)
    if (nc < nr)
      r <- paste(r, lx.read.char(handle, n=nr-nc, skip.null=FALSE), sep="")
  } else {
    nr <- if (n >= 0) n else .chunk.size
    p  <- handle$handler$seek(handle$connect)
    r  <- handle$handler$read(handle$connect, nr)
    of <- head(which(r == 0), 1)
    if (length(of) > 0) { # NULL found
      r <- rawToChar(head(r, of - 1))
      handle$handler$seek(handle$connect, p+of)
    } else {              # no NULL found
      r <- rawToChar(r)
      if (n < 0)
        r <- paste(r, lx.read.char(handle, n=n, skip.null=TRUE), sep="")
    }
  }
  r  
}

# -------------------------------------------------
#' read binary C-style or basta string
#' @description
#' a C-style string is null terminated character array\cr
#' a basta string is binary encoded in the following way\cr
#' \itemize{
#'  \item int32 : string size
#'  \item chars+NULL : null terminated (C style) character array
#' }
#' @param handle file handle (opened by \link{lx.open})
#' @param with.size if true basta string else simple C-style string 
#' @return character string
#' @note if with.size==FALSE this function is equivalent to
#' \code{lx.read.char(handle, n=-1L, skip=FALSE)}
#' @examples
#' fh <- lx.open(lx.system.file("samples/test_bin.bst"))
#' cod  <- lx.read.int32(fh, n=1)
#' nseq <- lx.read.int32(fh, n=1)
#' s1 <- lx.read.string(fh)
#' siz1 <- lx.read.int64(fh, n=1)
#' crc1 <- lx.read.int32(fh, n=1)
#' s2 <- lx.read.string(fh)
#' siz2 <- lx.read.int64(fh, n=1)
#' crc2 <- lx.read.int32(fh, n=1)
#' ss1 <- lx.read.char(fh, n=siz1)
#' ss2 <- lx.read.char(fh, n=siz2)
#' lx.close(fh)
#
lx.read.string <- function(handle, with.size=TRUE) {
  if (with.size) {
    siz <- lx.read.int32(handle)
    str <- lx.read.char(handle, n=siz+1, skip.null=FALSE)
    lx.warnif(nchar(str) != siz, "invalid string size (corrupted binary file ?)")
  } else {
    str <- lx.read.char(handle, n=-1L, skip.null=FALSE)
  }
  str
}

# -------------------------------------------------
#' print method for LXFileHandle
#' @param x file handle (opened by \link{lx.open})
#' @param ... further arguments passed to or from other methods.
#' @return (invisible) x
#
print.LXFileHandle <- function(x, ...) {
  .p <- function(...) print(paste0(...), quote=F)
  .p("<LXFileHandle>")
  .p(" filename: ", x$filename)
  .p("     type: ", x$type)
  .p("     mode: ", x$mode)
  .p("  is.open: ", x$is.open)
  invisible(x)
}

