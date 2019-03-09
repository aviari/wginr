# -------------------------------------------------
# $Id: mdb.obo.r 321 2017-08-18 11:10:19Z viari $
#' @name HELP.OBO
#' @docType data
#' @title lx in-memory database parser for Obo format
#' @description
#' read and parse Obo database in flat file format and hold
#' results in memory
#'
#' the main functions are : \code{\link{mdb.obo.read}} and \code{\link{mdb.obo.load}}
#'
#' @note
#' these functions hold all the database in memory and are therefore not intented for
#' large databases
#'
#' @examples
#'
#' db <- mdb.obo.load(lx.system.file("samples/test_obo", "xlx"))
#'
#' # get entry names
#'
#' names(db)
#'
#' # get info about specific entry :
#'
#' db$GO.0000001
#' db$GO.0000001$id
#' db$GO.0000001$name
#'
#' # search for entries matching pattern :
#'
#' mdb.find(db, "def", "mitochondrial", ignore.case=TRUE)
#'
NULL

# =================================================
# internal obo parsers
#

.mdb.obo.parse.id <- function(rec) {
  sub("^GO:", "GO.", rec)
}

.mdb.obo.parse.is_a <- function(rec) { 
  res <- c()
  n <- 0
  for (line in lx.strsplit(rec, "\n")) {
     val <- lx.strsplit(line, " ")
     n <- n + 1
     res[n] <- gsub(":", ".", val[1])
  }
  res
}

.mdb.obo.parse.relationship <- function(rec) { 
  res <- list()
  for (line in lx.strsplit(rec, "\n")) {
     val <- lx.strsplit(line, " ")
     res[[val[1]]] <- c(res[[val[1]]], gsub(":", ".", val[2]))
  }
  res
}

.mdb.obo.parse.xref <- function(rec) { 
  res <- list()
  for (line in lx.strsplit(rec, "\n")) {
     val <- lx.strsplit(line, " ")
     key <- lx.strsplit(val[1], ":")
     res[[key[1]]] <- c(res[[key[1]]], paste(
                paste(key[-1], collapse=":"), 
                paste(val[-1], collapse=" "), 
                collapse=" "))
  }
  res
}

# -------------------------------------------------
#' obo main parsing driver (internal use)
#' @param key key to parse (currently AC,OC,KW,DR,seq)
#' @param rec record to process
#' @note call function \code{.mdb.obo.parse.<key> if it exists}
#
mdb.obo.parse <- function(key, rec) {
  fun <- paste(".mdb.obo.parse.", key, sep="")
  if (exists(fun, mode="function"))
    do.call(fun, list(rec))
  else
    rec
}

# =================================================
# obo format reader and parser
#

# -------------------------------------------------
#' read obo flat file and parse fields
#' @param pathname pathname of obo file to read
#' @return a list indexed by GO terms (under the form GO.<number> not GO:<number>)\cr
#'         each element is a list indexed by the record key\cr
#'         each recordkey element is either the raw line(s) or the result of a specific parser\cr
#'         current parsers are provided for : id, is_a, relationship and xref.\cr
#'         in addition a pseudo-key named 'parent_of' is added, representing the reverse
#'         of 'is_a' relationship.
#' @note
#' you may add your own function \code{.mdb.obo.parse.<key>(rec)}
#' to parse other keys than (id, is_a, relationship and xref).\cr
#' \code{rec} is a string containing all lines of the current record 
#' to be parsed (with newlines as \\n) and your function may return whatever
#' is appropriate (usually a list).
#' 
#' the obo file may be provided in plain text or gzipped format. this is checked
#' automaticaly, there is no need to add the .gz extension.
#' @seealso \link{mdb.obo.load}
#' @examples
#'
#' db <- mdb.obo.read(lx.system.file("samples/test_obo.dat", "xlx"))
#'
#' # get entry names
#' names(db)
#'
#' # get info about specific entry :
#'
#' db$GO.0000001
#' db$GO.0000001$id
#' db$GO.0000001$name
#'
#' # search for entries matching pattern :
#'
#' mdb.find(db, "def", "mitochondrial", ignore.case=TRUE)
#' 
#' dc <- mdb.obo.read(lx.system.file("samples/test_obo_compressed.dat", "xlx"))
#' identical(db, dc)
#
mdb.obo.read <- function(pathname) {

  .scat <- function(s0, s1, sep=" ") {
    s1 <- gsub("^ +", "", s1)
    if (is.null(s0)) s1 else paste(s0, s1, sep=sep)
  }
  
  db     <- list()
  nentry <- 0
  okterm <- FALSE
  id     <- ""
  
  # ------------------------
  # open file
  #
  
  # try gzipped version if it exists
  if (file.exists(paste0(pathname, ".gz")))
    pathname <- paste0(pathname, ".gz")

  fil <- file(pathname)
  open(fil)

  # ------------------------
  # read lines
  #
  while(length(line <- readLines(fil, n=1, warn=FALSE)) > 0) {

    # ------------------------
    # skip over empty lines
    #
    if (nchar(line) == 0) next
   
    # ------------------------
    # get header line
    #
    if (grepl("^\\[", line)) {
      okterm <- grepl("^\\[Term]", line)
      id <- ""
      next
    }

    # ------------------------
    # continue only on valid terms
    #
    if (! okterm) next

    # ------------------------
    # get key
    
    field <- lx.strsplit(line, " +")
    key <- gsub(":$", "", field[1])
    sline <- paste(field[-1], collapse=" ")
    
    # ------------------------
    # id line
    #
    if (key == "id") {
      id          <- gsub(":", ".", field[2])
      nentry      <- nentry + 1
      db[[id]]    <- list()
      db[[id]]$id <- sline

      if (nentry %% 100 == 1)
        lx.out("reading entry # ", nentry)
        
      next
    }

    # don't process unknown id's
    if (id == "") next

    # ------------------------
    # any other field : catenate values
    #
    db[[id]][[key]] <- .scat(db[[id]][[key]], sline, sep="\n")
  }

  # ------------------------
  # parse records : we have to do that at the end because OBO
  # has no end of record mark
  #
  for (id in names(db)) {
    rec <- db[[id]]
    nam <- names(rec)
    db[[id]] <- lapply(nam, function(x) {mdb.obo.parse(x, rec[[x]])})
    names(db[[id]]) <- nam
  }

  # ------------------------
  # setup the reverse is_a relationship
  # called parent_of
  #
  for (id in names(db)) {
    for (child in db[[id]]$is_a) {
      if (! is.null(db[[child]]))
        db[[child]]$parent_of = c(db[[child]]$parent_of, id)
    }
  }

  lx.out(nentry, " entries found")
  
  # ------------------------
  # close file
  #
  close(fil)
  
  invisible(db)
}

# -------------------------------------------------
#' quick load obo db
#' @description
#' this is a quicker version of \link{mdb.obo.read}\cr
#' \code{mdb.obo.load} try to recover a previously loaded and serialized 
#' file called : dbname.rds\cr
#' if it does not exist then it reads the flat file called : 'dbname.dat'
#' and further serialize the result into dbname.rds\cr
#' you may force to ignore the serialized version by using force=TRUE
#' @param dbname filename (without extension) of obo flatfile
#' @param force force read and serialize even if serialized file already exists
#' @param local if TRUE (default), serialized DB is saved and/or loaded in
#' current directory else in \code{dirname(dbname)}. 
#' @return a list indexed by GO terms (under the form GO.<number> not GO:<number>)\cr
#'         each element is a list indexed by the record key\cr
#'         each recordkey element is either the raw line(s) or the result of a specific parser\cr
#'         current parsers are provided for : id, is_a, relationship and xref
#' @note
#' you may add your own function \code{.mdb.obo.parse.<key>(rec)}
#' to parse other keys than (id, is_a, relationship and xref).
#' @seealso \code{\link{mdb.obo.read}}
#' @examples
#'
#' db <- mdb.obo.load(lx.system.file("samples/test_obo", "xlx"))
#'
#' # get entry names
#' names(db)
#'
#' # get info about specific entry :
#'
#' db$GO.0000001
#' db$GO.0000001$id
#' db$GO.0000001$name
#'
#' # search for entries matching pattern :
#'
#' mdb.find(db, "def", "mitochondrial", ignore.case=TRUE)
#
mdb.obo.load <- function(dbname, force=FALSE, local=TRUE) {
  dmpname <- if (local) basename(dbname) else dbname
  if ((! force) && lx.serialized(dmpname)) {
    db <- lx.unserialize(dmpname)
  }
  else {
    dbname <- paste(dbname, "dat", sep=".")
    lx.out("reading DB: ", dbname)
    db <- mdb.obo.read(dbname)
    lx.serialize(db, dmpname)
  }
  db
}

# -------------------------------------------------
#' get ancestors of go.id(s)
#' @param db go db opened by \link{mdb.obo.read} or \link{mdb.obo.load}
#' @param go.id entry id
#' @param max.depth maximum depth of ancestor
#' @return list of ancestors go.id's
#' @seealso \link{mdb.obo.index.ancestors}
#' @examples
#'
#' db <- mdb.obo.load(lx.system.file("samples/test_obo", "xlx"))
#' mdb.obo.get.ancestors(db, "GO.0000083")
#
mdb.obo.get.ancestors <- function(db, go.id, max.depth=Inf) {
  if (is.null(go.id) || (max.depth <= 0)) return(NULL)
  parents <- unique(unlist(lapply(db[go.id], function(x) { x$is_a })))
  union(parents, mdb.obo.get.ancestors(db, parents, max.depth-1))
}

# -------------------------------------------------
#' get indexed array of ancestors
#' @description
#' get a list indexed by goids, giving for each entry
#' the list of ancestors for this goid.\cr
#' with goids=names(db) or restrict=TRUE, this is formally equivalent to
#' but much quicker than :\cr
#' \code{sapply(goids, function(x) mdb.obo.get.ancestors(db, x))}\cr
#' with restrict=FALSE the resulting list includes entries
#' for goids as well their ancestors ids.
#' @param db go db opened by mdb.obo.read or mdb.obo.load
#' @param goids set of goid's to index
#' @param restrict result to goids only (do not include entries for their ancestors)
#' @return named list of ancestors for each goids (and optionally their ancestors)
#' @seealso \link{mdb.obo.get.ancestors}
#' @examples
#' db <- mdb.obo.load(lx.system.file("samples/test_obo", "xlx"))
#' anc <- mdb.obo.index.ancestors(db)
#' anc["GO.0000083"]
#' mdb.obo.get.ancestors(db, "GO.0000083")
#
mdb.obo.index.ancestors <- function(db, goids=names(db), restrict=TRUE) {
  anc <- list()
  .rec <- function(id) {
    if (is.null(anc[[id]])) {
      parents <- unique(unlist(lapply(db[id], function(x) x$is_a )))
      anc[[id]] <<- union(parents, unlist(sapply(parents, .rec)))
    }
    if (is.null(anc[[id]])) anc[id] <<- list(NULL) # for roots
    anc[[id]]
  }
  res <- sapply(goids, .rec)
  if (restrict) res else anc
}


# =================================================
# tests
#

if (FALSE) {

  dbo <- mdb.obo.load(lx.system.file("samples/test_obo", "xlx"))
  
  dbo$GO.0000001$name
  
  anc <- mdb.obo.index.ancestors(dbo)
  
  anc["GO.0000083"]
  
  mdb.obo.get.ancestors(dbo, "GO.0000083")
}

