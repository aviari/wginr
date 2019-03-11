# -------------------------------------------------
# $Id: asdog.params.parser.r 495 2019-03-11 08:25:52Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# asdog parameters : create, parse and print parameters
#

# =================================================
# internal utilities
# =================================================

# pmatch nocase
#
.pmatch <- function(x, table) {
  p <- pmatch(tolower(x), tolower(table))
  if (is.na(p)) x else table[p]
}

# parse integer
#
.parse.integer <- function(s) {
  s <- .pmatch(s, c("NA", "Inf", "+Inf", "-Inf"))
  if (grepl("Inf$", s))
    return(.Machine$integer.max * ifelse(grepl("^-", s), -1L, 1L))
  if (grepl("^NA$", s))
    return(NA_integer_)
  if (grepl("L$", s))
    s <- substr(s, 1, nchar(s)-1)
  as.integer(s)
}

# parse numeric
#
.parse.numeric <- function(s) {
  s <- .pmatch(s, c("NA", "NaN", "Inf", "+Inf", "-Inf"))
  if (grepl("^NA$", s))
    return(NA_real_)
  as.numeric(s)
}

# parse any R expression
#
# added: the following form '@a@b@c' 
#        will get parsed to c(a,b,c)
#        (to simplify arguments passing from shell)
#
.parse.any <- function(s) {
  if (grepl("^@", s))
    s <- paste0("c(", gsub("@", ",", substring(s,2)), ")")
  parse(text=s)
}

# =================================================
# API
# =================================================

# -------------------------------------------------
#' new asdog.model parameters with defaults
#' @param ... key=value arguments specifying parameter values (see details)
#' @param default default list of parameters (see details)
#' @param as.args boolean, should ... parameters be treated as args (from shell). 
#' If TRUE they will be parsed from strings else they will be treated as is.
#' @details
#' you may specify parameter value by using '...' arguments as key=value\cr
#' the key should match one of the definitions given in \code{default}.
#' 
#' the \code{default} list contains default names, values and type 
#' as well as a  short description for each default value.
#' see \link{asdog.default.params} as an example of a function
#' providing such a default list.
#'
#' to import default from another Params object :
#'   do.call(asdog.new.params, params)
#' 
#' @note this function is usually called by \link{asdog.main.params}.
#'
#' @seealso \link{asdog.default.params}, \link{asdog.main.params}
#' 

asdog.new.params <- function(..., 
                             default=asdog.default.params(), 
                             as.args=FALSE) {

  # get default values
  #

  params <- sapply(default, function(x) x$value)
  class(params) <- c('Params', class(params))

  pnam <- names(params)
  
  # override defaults by user's arguments
  #
  
  # evaluation of string arguments
  
  .eval <- function(e, v) {
    # special cases
    #   "NULL" => NULL
    #  "=<expression>" => eval R expression
    #
    if (v == "NULL") return()
    if (grepl("^=", v)) return(eval(.parse.any(substring(v,2))))
    # 
    # standard cases
    #
    switch(default[[e]]$type,
           character=v,
           integer=.parse.integer(v),
           double=.parse.numeric(v),
           eval(.parse.any(v)))
  }
  
  # process function arguments (...)
  
  args <- lx.args(-1)
  args['as.args'] <- NULL
  args['default'] <- NULL

  for (e in names(args)) {
     if (! (e %in% pnam)) {
       lx.warn('"', e, '" not a known parameter name')
       params[[e]] <- args[[e]]
     }
     else {
       params[[e]] <- if (as.args) .eval(e, args[[e]])
                      else args[[e]]
     }
  }
  
  # keep defaults definition as attributes
  attr(params, "default") <- default
  
  params
}

# -------------------------------------------------
# filter parameter according to category
#
asdog.filter.params <- function(params,
                                in.filter=".", out.filter="_internal_",
                                default=asdog.default.params()) {
    if (is.null(params))
      params <- sapply(default, function(x) x$value)
    dnam <- names(default)
    for (e in names(params)) {
      if (! (e %in% dnam)) {
       lx.warn('"', e, '" not a known parameter name')
      } else {
        categ <- default[[e]]$category
        if (grepl("*:", categ, fixed=T)) next
        if (grepl(in.filter, categ) && (! grepl(out.filter, categ))) next
        params[[e]] <- NULL
      }
    }
    
    params
}

# -------------------------------------------------
#' print asdog parameters
#
asdog.print.params <- function(params, trunclen=Inf) {
  .pad <- function(s, len) {
    while(nchar(s) <= len) s <- paste(' ', s, sep='')
    s
  }
  .trunc <- function(x, trunclen) {
    lx.strtrunc(lx.strtrim(paste0(format(x), collapse=" ")), trunclen)
  }
  .out <- function(...) {
    cat("+ ", ..., '\n', file = stderr())
  }
    
  .out("---------------------------------------")
  .out(" Parameters")
  .out("---------------------------------------")
  len <- max(nchar(names(params)))
  for (e in names(params)) {
    .out(.pad(e, len), " : ", .trunc(params[[e]], trunclen))
  }
  .out("---------------------------------------")

  invisible(params)
}

# -------------------------------------------------
#' get asdog params info (as dataframe)
#' @param ... names of parameters (if empty return info for all default parameters)
#'

asdog.info.params <- function(..., default=asdog.default.params()) {
  nam <- lx.args(-1)
  nam$default <- NULL
  res <- if (length(nam) > 0) default[unlist(nam)] else default
  
  # reformat as dataframe (for printing)
  if (length(res) > 0) {
    mat <- matrix(unlist(res, recursive=F), ncol=5, byrow=T)
    rownames(mat) <- names(res)
    colnames(mat) <- names(res[[1]])
    res <- data.frame(mat)
  }
  res
}

# -------------------------------------------------
# override print
#
# print.Params <- function(params) asdog.print.params(params)

