# -------------------------------------------------
# $Id: xlx.apply.r 321 2017-08-18 11:10:19Z viari $
# xlx apply variants
#

# =================================================
# API
# =================================================

# -------------------------------------------------
#' apply function to clocations by chromosomes
#' @description
#' split a nx3 matrix of clocations by chromosomes (first column),
#' and apply user's function to each submatrix in turn.\cr
#' this is a pivotal function of the XLX library
#' to split job across chromosomes for multithreading or disk pooling\cr
#' @param clocations nx3 matrix of clocations
#' @param fun : function or function name called as \code{fun(clocs, handle, ...)} (see details)
#' @param ... anything passed to \code{fun}
#' @param handle optional (basta or baf) file handle. if use.threads==TRUE 
#' then handle will be properly duplicated thru calls (as with \link{lx.happly})
#' @param flatten flatten results and reorder them in the same order as clocations (see details)
#' @param use.threads (see \link{lx.use.threads})
#' @param mc.cores number of processes (see \link{HELP.LX.OPTIONS})
#' @return a named list of results of \code{fun} (names are \code{as.character(chrindex)})
#'         or a flattened (unnamed) list if flatten==TRUE (see Details).
#' @details
#' \code{fun} first argument is a matrix of clocations (on a single chromosome)
#' \bold{not} a single clocation. see \link{apply.cloc} for this variant.\cr
#' the \code{flatten} parameter is only meaningful if the results of \code{fun(clocs, handle,...)}
#' is a list of length exactly equals to nrow(clocs). then all the results will be catenated
#' and reordered in the same order as in clocations.\cr
#' be careful with NULL (or empty) elements in results that may be swallowed.\cr
#' if at least one call to \code{fun} does not meet this criterion, then a warning
#' is raised and results will not be flattened at all.
#' (the \link{apply.cloc} version will take care of this).
#' @seealso \link{apply.cloc}
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' slocs <- c("seq1:1-10", "seq2:2-3", "seq1:15-20")
#' clocs <- clocations(lapply(slocs, sloc2cloc, handle=fh))
#' seqs <- apply.clocs(clocs, function(sublocs, handle) {
#'                        apply(sublocs, 1, basta.fetch.cloc, handle=handle)
#'                      }, handle=fh, use.threads=TRUE, mc.cores=2)
#' seqs <- apply.clocs(clocs, function(sublocs, handle) {
#'                        apply(sublocs, 1, basta.fetch.cloc, handle=handle)
#'                      }, handle=fh, flatten=TRUE, use.threads=TRUE, mc.cores=2)
#' basta.close(fh)
#'
apply.clocs <- function(clocations, fun, ..., 
                            handle=NULL,
                            flatten=FALSE,
                            use.threads=lx.use.threads(),
                            mc.cores=lx.options(mc.cores)) {

   clocations <- clocations(clocations)     # make sure this is a clocations
   
   if (length(clocations) == 0)             # empty clocation
     return(list())
  
   if (is.function(fun)) {
     funbody <- fun
     funname <- as.character((substitute(fun)))[1]
   }
   else {
     funbody <- get(fun)
     funname <- fun
   }
   
   if (flatten) {
     lx.out("keeping order", level="debug")
     chrs <- clocations[,1]
     chorder <- unlist(split(seq_along(chrs), chrs), use.names=FALSE)
   }

   nlocs <- nrow(clocations)
   lx.out("splitting ", nlocs, " clocations by chromosomes", level="debug")
   clocations <- clocs.rsplit(clocations)

   duphandle <- use.threads && (! is.null(handle))

   res <- lx.lapply(clocations, function(clocs, handle) {
       chrindex <- as.character(clocs[1,1])
       if (duphandle) handle <- lx.dup.handle(handle)
       lx.out("  running function '", funname, "' on chr ", chrindex, level="debug")
       res <- funbody(clocs, handle=handle, ...)
       if (lx.warnif(flatten && (length(res) != nrow(clocs)),
                     "result from ", funname, " cannot be flattened properly ",
                     length(res), " != ", nrow(clocs)))
         flatten <<- FALSE
       if (duphandle) lx.close(handle)
       res
    }, handle, 
       use.threads=use.threads,
       mc.cores=mc.cores,
       mc.preschedule=FALSE,
       mc.allow.recursive=FALSE)

    if (flatten) {
      lx.out("flattening results", level="debug")
      flatres <- unlist(res, recursive=FALSE, use.names=FALSE)
      if (lx.warnif(length(flatres) != nlocs,
                    "merged results from ", funname, " cannot be flattened properly ",
                     length(flatres), " != ", nlocs))
       flatten <- FALSE
     else
       res <- flatres
    }
    
    if (flatten) {
      lx.out("reordering results", level="debug")
      res <- res[order(chorder)]
   }

   res
}

# -------------------------------------------------
#' apply function to clocations by chromosomes
#' @description
#' this is a variant of \link{apply.clocs} where \code{fun} is
#' called on each clocation (instead of matrix of clocations).
#' it can be viewed as a simple \code{apply(clocations, 1, fun, ...)}
#' except that the job is actually split by chromosome for multithreading.\cr
#' @param clocations nx3 matrix of clocations
#' @param fun : function or function name called as \code{fun(cloc, handle, ...)}
#' @param ... anything passed to \code{fun}
#' @param handle optional (basta or baf) file handle. if use.threads==TRUE 
#' then handle will be properly duplicated thru calls (as with \link{lx.happly})
#' @param keep.order keep \code{fun} results in the same order as clocations
#' @param use.threads (see \link{lx.use.threads})
#' @param mc.cores number of processes (see \link{HELP.LX.OPTIONS})
#' @return a (unnamed) vector of results of \code{fun}
#' @details
#' \code{fun} first argument is a single clocation
#' \bold{not} a matrix of clocations as in \link{apply.clocs}
#' @note
#' if result order is not important, then use keep.order=FALSE (this will slightly
#' speedup the operation and save memory)
#' @seealso \link{apply.clocs}
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' slocs <- c("seq1:1-10", "seq2:2-3", "seq1:15-20") 
#' clocs <- clocations(lapply(slocs, sloc2cloc, handle=fh))
#' seqs <- apply.cloc(clocs, function(cloc, handle) {
#'                        basta.fetch.cloc(handle, cloc)
#'                      }, handle=fh, use.threads=TRUE, mc.cores=2)
#' seqs <- apply.cloc(clocs, function(cloc, handle, foo) {
#'                 paste0(foo, basta.fetch.cloc(handle, cloc))
#' }, handle=fh, foo="seq:", use.threads=TRUE, mc.cores=2)
#'
#' basta.close(fh)
#'
apply.cloc <- function(clocations, fun, ..., 
                        handle=NULL,
                        keep.order=TRUE,
                        use.threads=lx.use.threads(),
                        mc.cores=lx.options(mc.cores)) {

  clocations <- clocations(clocations)     # make sure this is a clocations
  
  if (length(clocations) == 0)             # empty clocation
    return(list())

   # wrap fun in internal apply
  
   .fun <- function(clocs, handle, ...) {
     res <- apply(clocs, 1, function(cloc, handle, ...) {
        list(fun(cloc, handle, ...))
     }, handle=handle, ...)
     unlist(res, recursive=FALSE, use.names=FALSE)
   }
  
   # if no thread, there is no need to make it complicated
   #   if (! use.threads) {
   #     return(.fun(clocations, handle, ...))
   #   }

   # use threads

   res <- apply.clocs(clocations, .fun, ..., 
                          handle=handle,
                          flatten=keep.order,
                          use.threads=use.threads,
                          mc.cores=mc.cores)
   if (! keep.order) {
     lx.out("flattening unordered results", level="debug")
     res <- unlist(res, recursive=FALSE, use.names=FALSE)
   }

   res
}
