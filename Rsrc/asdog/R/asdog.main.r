# -------------------------------------------------
# $Id: asdog.main.r 322 2017-08-18 11:20:32Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# utilities to help writing main programs
#

# =================================================
# internals <no_export>
# =================================================

.cat <- function(...) {
  cat(..., '\n', file=stderr())
}

.trunc <- function(x, trunclen=30) {
  lx.strtrunc(lx.strtrim(paste0(format(x), collapse=" ")), trunclen)
}

.help.options <- function(category, default) {
  params <- asdog.filter.params(NULL, in.filter=category, default=default)
  if (length(params) > 0) {
    info <- do.call(asdog.info.params, as.list(names(params)))
    info <- data.frame(info[,c(2,4)])
    colnames(info) <- c('info', 'default')
    info$default <- lapply(info$default, .trunc, 30)
    owidth <- options('width')
    options(width=160)
    print(info)
    options(width=owidth[[1]])
  }
  .cat("")
}

.usage <- function(category="", default, ...) {
  if (exists('prog.usage')) prog.usage(...)
  if (category != "") {
    .cat("available options:")
    .help.options(category, default)
  } else {
    .cat("use --help to get a list of available options")
  }
}

# =================================================
# API
# =================================================

# -------------------------------------------------
# print help usage
#
asdog.main.usage <- function(category="", default=asdog.default.params(), ...) {
  .usage(category, default, ...)
  invisible()
}

# -------------------------------------------------
# quit program
#
asdog.main.exit <- function(status=1, category="", usage=(category != ""), 
                            default=asdog.default.params(), ...) {
  if (usage) .usage(category, default, ...)
  quit(save="no", status=status)
}

# -------------------------------------------------
# handle general user's arguments
#
asdog.main.args <- function(category="", default=asdog.default.params()) {

  args <- lx.getargs()
  dft  <- sapply(default, function(x) x$value)

  .val <- function(key) if (is.null(args[[key]])) dft[[key]] else args[[key]]
    
  # --help
  if ('help' %in% names(args)) {
    asdog.main.exit(0, category=category, default=default)
  }

  # --use.threads=<logical>
  lx.options(use.threads=as.logical(.val("use.threads")))
  
  # --mc.cores=<integer>
  mc.cores <- as.integer(.val("mc.cores"))
  if (mc.cores <= 0) mc.cores <- parallel::detectCores()
  lx.options(mc.cores=mc.cores)

  # --verbose=<character> 
  lx.verbose(.val("verbose"))

  # --seed=<integer>
  seed <- as.integer(.val("seed"))
  if (seed >= 0) set.seed(seed)

  args
}

# -------------------------------------------------
#' get asdog parameters from args and (optionally)
#' from previous params
#
asdog.main.params <- function(args, params=NULL, 
                              default=asdog.default.params()) {
  
  # get optional previous params
  
  if (is.null(params))
    params <- sapply(default, function(x) x$value)
  
  params <- do.call(asdog.new.params, c(params, as.args=F, default=list(default)))

  # parse args 
  
  args.params <- do.call(asdog.new.params, c(args, as.args=T, default=list(default)))

  # override previous parameters by args

  for (e in names(args)) {
    params[[e]] <- args.params[[e]]
  }
  
  params
}

# -------------------------------------------------
# setup tex graphic driver
#
asdog.tex.driver <- function(params) {
  
  driver <- params$report.latex.driver
  res    <- params$report.driver.res
  
  if (driver %in% c('jpeg', 'png', 'tiff'))
    lx.options(tex.driver.options=list(list(type="cairo", units="in", res=res)))
  else
    lx.options(tex.driver.options=list(NULL))

  lx.options(tex.graphics.driver=driver)
  invisible(driver)
}

# -------------------------------------------------
# check file as name name.type name.bgz name.type.bgz 
#
asdog.check.file <- function(name, type, .bgz=c("", ".bgz"), .abort=TRUE) {
  path <- lx.file.resolve(name, list(c("", type), .bgz))
  if (is.null(path)) {
    msg <- paste0(type[[1]], ' file "', name, '" not found')
    lx.stopif(.abort, msg, trace=F, up.frame=1)
    lx.warn(msg, up.frame=1)
  } else {
    lx.out('found ', type[[1]], ' file: ', path, up.frame=1)
  }
  path
}

# -------------------------------------------------
# cleanup suffixes from path
#
asdog.clean.path.suffix <- function(path, suffixes, ignore.case=T) {
  .case <- function(x) if (ignore.case) tolower(x) else x
  suffixes <- sapply(suffixes, .case)
  ext <- .case(lx.file.ext(path))
  while (ext %in% suffixes) {
    path <- lx.file.no.ext(path)
    ext <- .case(lx.file.ext(path))
  }
  path
}

# -------------------------------------------------
# get asdog (or other package) version 
#
asdog.version <- function(package="asdog", as.string=T) {
  v <- as.character(packageVersion(package))
  if (as.string) v else as.integer(lx.strsplit(v, "\\."))
}

# -------------------------------------------------
# create an asdog object 
#
asdog.object <- function(classname, ...) {
  o <- lx.new(c(classname, "Asdog"), .version=asdog.version(), ...)
  lx.info(o$.version) <- "asdog version number"
  o
}

# -------------------------------------------------
# funny banner
#
asdog.banner <- function() {
  .cat <- function(...) cat("+", paste0(...), "\n")
  .catver <- function(x) .cat(x, " v", asdog.version(x))
  vers <- asdog.version()
  blnk <- paste(rep(" ", 9 - nchar(vers)), collapse="")
  .cat()
  .cat("     ,                    ,\" e`--o")
  .cat("    ((                   (  | __,'")
  .cat("     \\\\~----------------' \\_;/")
  .cat("     (     ASDOG v", vers, blnk, "/")
  .cat("     /) ._______________.  )")
  .cat("    (( (               (( (")
  .cat("     ``-'               ``-'")
  .cat(R.version.string)
  .catver("lx")
  .catver("xlx")
  .catver("thmm")
  .cat()
  invisible()
}



