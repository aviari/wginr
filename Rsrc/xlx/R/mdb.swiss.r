# -------------------------------------------------
# $Id: mdb.swiss.r 321 2017-08-18 11:10:19Z viari $
#
#' @name HELP.SWISS
#' @docType data
#' @title lx in-memory database parser for Uniprot/Swissprot format
#' @description
#' read and parse Uniprot/Swissprot database in flat file format
#' and hold results in memory
#'
#' the main functions are : \code{\link{mdb.swiss.read}} and \code{\link{mdb.swiss.load}}
#'
#' @note
#' these functions hold all the database in memory and are therefore not intented for
#' large databases
#'
#' @examples
#'
#' db <- mdb.swiss.read(lx.system.file("samples/test_swiss.dat", "xlx"))
#'
#' # get entry names
#' names(db)
#'
#' # get info about specific entry :
#' db$P04395
#' db$P04395$OC
#' db$P04395$DR$PROSITE
#'
#' # search for entries matching pattern :
#' mdb.find(db, "KW", "gluconate", ignore.case=TRUE)
#'
NULL

# =================================================
# internal swissprot parsers
#
.mdb.swiss.parse.list <- function(rec) {
  lx.strsplit(gsub("\n|\\.$", "", rec), ";")
}

.mdb.swiss.parse.AC <- .mdb.swiss.parse.list
.mdb.swiss.parse.OC <- .mdb.swiss.parse.list
.mdb.swiss.parse.KW <- .mdb.swiss.parse.list

.mdb.swiss.parse.DR <- function(rec) {
  res <- list()
  for (line in lx.strsplit(rec, "\n")) {
     val <- lx.strsplit(gsub("\\.$", "", line), ";")
     res[[val[1]]] <- c(res[[val[1]]], paste(val[-1], collapse=";"))
  }
  res
}

.mdb.swiss.parse.seq <- function(rec) {
  gsub(" +", "", rec)
}

# -------------------------------------------------
#' swiss parse driver
#' @description
#' parse the content of \code{key} in record.\cr
#' @details
#' this function just acts as a selector to call 
#' function \code{.mdb.swiss.parse.<key>} if it exists
#' or return record if it does not.\cr
#' you may add your own function \code{.mdb.swiss.parse.<key>(rec)}
#' to parse other keys than (AC, OC, KW, DR and seq).\cr
#' \code{rec} is a string containing all lines of the current record 
#' to be parsed (with newlines as \\n) and your function may return whatever
#' is appropriate (usually a list). 
#' @param key key to parse (currently AC,OC,KW,DR,seq)
#' @param rec record to process
#' @return anything that should be stored under key in record.
#'
mdb.swiss.parse <- function(key, rec) {
  fun <- paste(".mdb.swiss.parse.", key, sep="")
  if (exists(fun, mode="function"))
    do.call(fun, list(rec))
  else
    rec
}

# =================================================
# swisprot format reader and parser
#

# -------------------------------------------------
#' read swissprot db
#' @description
#' read swissprot db and parse ID,AC,seq + extra fields as requested
#' @param pathname pathname of uniprot file to read
#' @param extra string comma-separated list of additional lines to parse (e.g "DE,OS,OC,KW")\cr
#'         if empty only the default ID, AC, and seq (sequence) lines are parsed\cr
#'         if "ALL" then all keys are parsed
#' @return a list indexed by records primary AC\cr
#'         each element is a list indexed by the record key (sequence is indexed by 'seq')\cr
#'         each recordkey element is either the raw line(s) or the result of a specific parser\cr
#'         current parsers are provided for : AC, OC, KW, DR (see note)\cr
#' @note
#' you may add your own function \code{.mdb.swiss.parse.<key>(rec)}
#' to parse other keys than (AC, OC, KW, DR and seq).\cr
#' \code{rec} is a string containing all lines of the current record 
#' to be parsed (with newlines as \\n) and your function may return whatever
#' is appropriate (usually a list).\cr
#' 
#' the uniprot file may be provided in plain text or gzipped format. this is checked
#' automaticaly, there is no need to add the .gz extension.
#' @seealso \code{\link{mdb.swiss.load}}
#' @examples
#'
#' db <- mdb.swiss.read(lx.system.file("samples/test_swiss.dat", "xlx"))
#'
#' # get entry names
#' names(db)
#'
#' # get info about specific entry :
#' db$P04395
#' db$P04395$OC
#' db$P04395$DR$PROSITE
#'
#' # search for entries matching pattern :
#' mdb.find(db, "KW", "gluconate", ignore.case=TRUE)
#'
#' dc <- mdb.swiss.read(lx.system.file("samples/test_swiss_compressed.dat", "xlx"))
#' identical(db, dc)
#' 
mdb.swiss.read <- function(pathname, extra="ALL") {

  all <- (toupper(extra) == "ALL")
  extra <- lx.strsplit(extra, ",")
  
  scat <- function(s0, s1, sep=" ") {
    s1 <- gsub("^ +", "", s1)
    if (is.null(s0)) s1 else paste(s0, s1, sep=sep)
  }
  
  db     <- list()
  ac     <- ""
  nentry <- 0
  
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
    key   <- substr(line, 1, 2)
    sline <- substr(line, 6, nchar(line))
    field <- lx.strsplit(line, " +")

    # ------------------------
    # ID line : keep for next
    #
    if (key == "ID") { # remember this line 
      idline <- sline
      pkey   <- key
      next
    }

    # ------------------------
    # AC line : init new record
    #
    if (key == "AC") {
      if (pkey == "ID") {
        nentry       <- nentry + 1
        ac           <- gsub(";$", "", field[2])
        db[[ac]]     <- list()
        db[[ac]]$ID  <- idline
        db[[ac]]$AC  <- sline
        db[[ac]]$seq <- ""
      
        if (nentry %% 100 == 1)
          lx.out("reading entry # ", nentry)
      } else { # multiple AC lines
        db[[ac]]$AC  <- scat(db[[ac]]$seq, sline, sep="")
      }
      pkey <- key
      next
    }

    # ------------------------
    # sequence line
    #
    if (key == "  ") {
      db[[ac]]$seq = scat(db[[ac]]$seq, sline, sep="")
      next
    }

    # ------------------------
    # end of record line : time to try parsing record
    #
    if (key == "//") {
      rec <- db[[ac]]
      nam <- names(rec)
      db[[ac]] <- lapply(nam, function(x) {mdb.swiss.parse(x, rec[[x]])})
      names(db[[ac]]) <- nam
      next
    }

    # ------------------------
    # any other field : catenate values
    #
    if (all || length(which(extra == key))) {
      db[[ac]][[key]] <- scat(db[[ac]][[key]], sline, sep="\n")
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
#' quick load swissprot db
#' @description
#' this is a quicker version of \link{mdb.swiss.read}\cr
#' \code{mdb.swiss.load} try to recover a previously loaded and serialized 
#' file called : \code{dbname.<sort_extra>.rds}
#' (where <sort_extra> is a '_' separated string of sorted extra, see below)\cr
#' if it does not exist then it reads the flat file called : \code{dbname.dat}
#' and further serialize the result into \code{dbname.<sort_extra>.rds}\cr
#' you may force to ignore the serialized version by using force=TRUE
#' @param dbname filename (without extension) of uniprot db
#' @param extra string comma-separated list of additional lines to parse (e.g "DE,OS,OC,KW")\cr
#'         if empty only the default ID, AC, and '  ' (sequence) lines are parsed\cr
#'         if "ALL" then all keys are parsed
#' @param force force read and serialize even if serialized file already exists
#' @param local if TRUE (default), serialized DB is saved and/or loaded in
#' current directory else in \code{dirname(dbname)}. 
#' @return a list indexed by records primary AC\cr
#'         each element is a list indexed by the record key (except sequence that is indexed by 'seq')\cr
#'         each recordkey element is either the raw line(s) or the resutl of a specific parser\cr
#'         current parsers are provided for : AC, OC, KW, DR\cr
#' @note
#' you may add your own function \code{.mdb.swiss.parse.<key>(rec)}
#' to parse other keys than (AC, OC, KW, DR and seq).
#' @seealso \link{mdb.swiss.read}
#' @examples
#'
#' db <- mdb.swiss.load(lx.system.file("samples/test_swiss", "xlx"))
#' 
#' # reload serialized version
#' db <- mdb.swiss.load(lx.system.file("samples/test_swiss", "xlx"))
#'
#' # get entry names
#' names(db)
#'
#' # get info about specific entry :
#' db$P04395
#' db$P04395$OC
#' db$P04395$DR$PROSITE
#'
#' # search for entries matching pattern :
#' mdb.find(db, "KW", "gluconate", ignore.case=TRUE)
#' 
#' # remove local serialized DB
#' unlink("test_swiss.ALL.rds")
#'
#
mdb.swiss.load <- function(dbname, extra="ALL", force=FALSE, local=TRUE) {
  ext  <- paste(sort(lx.strsplit(extra, ",")), collapse="_")
  dmpname <- paste(dbname, ext, sep=".")
  if (local) dmpname <- basename(dmpname)
  if ((! force) && lx.serialized(dmpname)) {
    db <- lx.unserialize(dmpname)
  }
  else {
    dbname <- paste(dbname, "dat", sep=".")
    lx.out("reading DB: ", dbname)
    db <- mdb.swiss.read(dbname, extra)
    lx.serialize(db, dmpname)
  }
  db
}

# =================================================
# tests
#

if (FALSE) {

  dbs <- mdb.swiss.load(lx.system.file("samples/test_swiss", "xlx"))

  mdb.find(dbs, "DE", "dehydrogenase", ignore.case=TRUE)
  
  seq <- sapply(dbs, function(r) r$seq)
}
