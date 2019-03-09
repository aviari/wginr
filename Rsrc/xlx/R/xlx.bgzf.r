# -------------------------------------------------
# $Id: xlx.bgzf.r 290 2017-08-05 09:58:42Z viari $
# xlx bgzf format
# require(rbgzf)
#

# -------------------------------------------------
# internal utilities
#

# -------------------------------------------------
# <internal> locked file access
# lock/unlock file and wait unlock
#
.bgzf.lockname <- function(path) {
  paste0(path, ".lock")
}

.bgzf.lock <- function(path) {
  file(.bgzf.lockname(path), open="w")
}

.bgzf.unlock <- function(file) {
  path <- summary(file)$description
  close(file)
  unlink(path)
}

.bgzf.wait.unlock <- function(path, sleep=0.1, ntry=1200) {
  path <- .bgzf.lockname(path)
  while (file.exists(path) && (ntry >= 0)) {
    Sys.sleep(sleep)
    ntry <- ntry - 1
  }
  lx.warnif((ntry < 0), ".bgzf.lock max try exceeded")
  ntry >= 0
}

# -------------------------------------------------
# <internal> index file access
#
.bgzf.indexname <- function(path, toread) {
  path <- paste0(path, ".idx")
  if (toread) {
    if (file.exists(path)) path else NULL
  } else {
    index.dir <- dirname(path)
    if (file.access(index.dir, mode=3) != 0) # cannot write into
      index.dir <- tempdir()  # this one is ok
    paste0(index.dir, "/", basename(path))
  }
}

# -------------------------------------------------
# <internal> reindex bgzf file
# first try to load index from :
#   - path.idx
#   - tmpdir/basename(path.idx)
# else reindex on the fly
#      and dump index in 
#   - path.idx (if directory is writable)
#   - tmpdir/basename(path.idx)
#
# note: load and dump are locked process
#       just in case this function is called 
#       within a thread (but this should be avoided)
#
.bgzf.index <- function(con, path) {
  index.path <- .bgzf.indexname(path, toread=T)
  if (! is.null(index.path)) {            # index already exists
    if (.bgzf.wait.unlock(index.path)) {  #   and unlocked
      lx.out("reading index file: ", index.path, level="debug")
      status <- bgzf.index.load(con, index.path)
    } else {                              #   and locked
      lx.out("dead lock: resort to in-memory index", level="warning")
      status <- bgzf.reindex(con)
    }
  } else {                                # no index file found
    lx.out("indexing file", level="debug")
    status <- bgzf.reindex(con)           #   reindex in memory
    if (status == 0) {                    #   write index file
      index.path <- .bgzf.indexname(path, toread=F)
      if (.bgzf.wait.unlock(index.path)) {
        lock <- .bgzf.lock(index.path)
        bgzf.index.dump(con, index.path)
        .bgzf.unlock(lock)
      }
    }
  }
  lx.warnif((status != 0), "cannot built index")
}

# -------------------------------------------------
# <internal> accept bgzf file
#
.bgzf.accept <- function(path, mode) {
  if (grepl("r", mode))
    bgzf.is.bgzf(path)
  else
    identical(lx.file.ext(path), "bgz")
}

# -------------------------------------------------
# <internal> open bgzf file
#
.bgzf.open <- function(path, mode="r") {
  con <- bgzf.open(path, gsub("b", "", mode))
  if (is.null(con)) return(con)
  if (grepl("r", mode)) { # read mode => index
    .bgzf.index(con, path)
  } else {                # write mode
    bgzf.index.build.init(con)
  }
  con
}

# -------------------------------------------------
# <internal> seek/tell
#
.bgzf.seek <- function(con, pos=NULL) {
  if (is.null(pos))
    bgzf.utell(con)
  else
    bgzf.useek(con, pos)
}

# -------------------------------------------------
# <internal> make bgzf file handler
#
.bgzf.file.handler <- function(name="bgzf") {
  h <- new.env(emptyenv())
  h$name   <- name
  h$accept <- .bgzf.accept
  h$open   <- .bgzf.open
  h$close  <-  bgzf.close
  h$seek   <- .bgzf.seek
  h$read   <- compiler::cmpfun(function(con, n=1L) bgzf.read(con, nbytes=n))
  h
}

if (FALSE) {

  require(xlx)
  require(microbenchmark)
  mb <- microbenchmark
  
  N <- 100

  foo <- function(fh) {
    n <- sum(fh$header$sizes)
    p <- sample(n, N, replace=T)
    x <- sapply(p, function(p) basta.fetch.coord(fh, c(p, p+100)))
  }

  f  = "/Users/viari/Desktop/LyonCancer/data/HS_GRCh37_72.bst"
  ff = "/Users/viari/Desktop/LyonCancer/data/HS_GRCh37_72.bst.bgz"

  fh <- basta.open(f)
  ffh <- basta.open(ff)
  
  mb(foo(fh), foo(ffh))  

  basta.close(fh)
  basta.close(ffh)
  
}

