# -------------------------------------------------
# $Id: xlx.regions.r 496 2019-03-11 08:26:47Z viari $
# Chromosomal regions definition
#

# =================================================
# internals <no_export>
# =================================================

# -------------------------------------------------
# <internal> <no_export>
# collect binned gc counts for a single clocation
# @description
# return a vector of gc counts in adjacent bins of size binsize
# @param handle basta of baf file handle
# @param cloc clocation
# @param binsize size of bins
# @return a numerical vector of gccounts
# @seealso \link{.bygc.cloc}
#
.gc.bin.cloc <- function(handle, cloc, binsize=1000L) {
  switch(handle$type,
         basta=basta.count.cloc(handle, cloc, sym="GC",
                                binsize=binsize, drop=T),
         baf=baf.bin.cloc(handle, cloc, what="gc", fun=sum,
                          binsize=binsize, drop=T, na.gc=FALSE),
        lx.stopif(T, "invalid file handle type"))
}

# -------------------------------------------------
# <internal> <no_export>
# get regions by gc content for a single clocation
# @description
# return a vector (possibly empty) of clocations restricted to given gc content.
# @param handle basta of baf file handle
# @param cloc clocation to split
# @param binsize size of bins
# @param gcrange gc range
# @param minreg minimum size of resulting regions
# @param use.threads (see \link{lx.use.threads})
# @return a nx3 matrix of (1-based) clocations
# @seealso \link{regions.bygc}
#
.bygc.cloc <- function(handle, cloc,
                       binsize=1000L, gcrange=c(0.0, 1.0),
                       minreg=binsize) {
  
  # default result : empty regions
  #
  regions <- clocs.matrix(NULL)
  
  cloclen <- cloc[3]-cloc[2]+1
  
  if (cloclen < min(binsize, minreg))
    return(regions)

  # get binned gc counts
  #
  gcnt <- .gc.bin.cloc(handle, cloc, binsize)

  pos  <- seq.int(from=1, to=cloclen-binsize+1, by=binsize)

  # filter gc regions
  #
  gcrange <- gcrange * binsize
  pos <- pos[(gcnt >= gcrange[1]) & (gcnt <= gcrange[2])]
  
  # convert to clocations
  #
  regions <- if (length(pos) > 0) cbind(cloc[1], pos, pos + binsize - 1) else NULL
  regions <- clocs.threshold(clocs.matrix(regions), minsize=minreg, use.threads=FALSE)
  
  regions
}

# -------------------------------------------------
# <internal> <no_export>
# get regions by gc content strata for a single clocation
# @description
# stratify adjacent bins of size binsize by their gc content.
# return a vector of regions stratified by gc content.
# @param handle basta or baf file handle
# @param cloc clocation to stratify 
# @param binsize window size to compute gc content
# @param nstrata number of %gc strata (strata go from 0. to 1. by 1/nstrata)
# @param minreg minimum size of resulting regions
# @return a vector of size nstrata. each element is a nx3 matrix of (1-based) clocations
# @seealso \link{regions.strata.bygc}
#
.strata.bygc.cloc <- function(handle, cloc,
                              binsize=1000L, nstrata=10L,
                              minreg=binsize) {

  # default result : empty regions
  #
  bylevel <- rep(list(NULL), nstrata)
  names(bylevel) <- as.character(seq.int(nstrata))
  
  cloclen <- cloc[3]-cloc[2]+1
  
  if (cloclen < binsize) 
    return(bylevel)

  
  # get gc counts and convert to levels in range 1:nstrata
  #
  gcnt <- .gc.bin.cloc(handle, cloc, binsize)
  gcnt <- pmin(nstrata, round(gcnt / binsize * nstrata) + 1)
  
  pos <- seq.int(from=1, to=cloclen-binsize+1, by=binsize)
  
  # split window start position by levels
  #
  pos <- pos + cloc[2] - 1
  pos <- split(pos, gcnt)
  bylevel[names(pos)] <- pos
  
  # convert to clocations
  #
  lapply(bylevel, function(lpos) {
    lpos <- if (is.null(lpos)) NULL else cbind(cloc[1], lpos, lpos + binsize - 1)
    clocs.threshold(clocs.matrix(lpos), minsize=minreg, use.threads=FALSE)
  })
}


# =================================================
# API
# =================================================

# -------------------------------------------------
#' get user's defined regions from bed files
#' @description 
#' loop over provided bed files, intersect their regions, filter out
#' small regions and returns clocations.
#' @param handle basta/baf file handle (as returned by \link{basta.open}
#' or \link{baf.open})
#' @param filenames a character vector of filenames
#' @param init regions (clocations) to start with (default is regions spanning all chromosomes)
#' @param minreg minimum region size (see \link{clocs.inter})
#' @param check check that region boundaries are correct (see \link{bed2clocs})
#' @param file.stop boolean, stops if a bed file is not found
#' @param use.threads (see \link{lx.use.threads})
#' @return a nx3 matrix of (1-based) clocations
#' @note
#' the function also checks if each file exists and will skip over 
#' or stop on non-existing file(s)
#' @seealso \link{bed.read}, \link{bed2clocs} and \link{basta2clocs}
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' bedfile <- lx.system.file("samples/test.bed", "xlx")
#' clocs <- regions.bybed(fh, bedfile)
#' # this is the same as:
#' clocs2 <- clocs.reduce(bed2clocs(fh, bed.read(bedfile)))
#' #
#' identical(clocs, clocs2)
#' basta.close(fh)
#'
regions.bybed <- function(handle, filenames, init=basta2clocs(handle),
                          minreg=1L, check=TRUE, file.stop=FALSE,
                          use.threads=lx.use.threads()) {

  clocs <- init

  lx.out("getting user's regions") 
  # loop over filenames
  for (bedfile in filenames) {
    if (file.exists(bedfile)) {
      lx.out("reading bedfile: ", bedfile)
      bedclocs <- bed2clocs(handle, bed.read(bedfile), check=check)
      clocs <- clocs.inter(clocs, bedclocs, minsize=minreg,
                           use.threads=use.threads)
    }
    else {
      lx.stopif(file.stop, "bedfile not found: ", bedfile)
      lx.out("skipping bedfile: ", bedfile, level="warning")
    }
  }

  clocs
}

# -------------------------------------------------
#' get binned coverage in regions
#' @description
#' loop over given \code{regions}
#' and collect mean coverage in adjacent windows of size \code{binsize}.
#' @param handle baf file handle (as returned by \link{baf.open})
#' @param regions clocations regions to bin (default is regions spanning
#' all chromosomes)
#' @param binsize size of bins
#' @param use.threads (see \link{lx.use.threads})
#' @param fun function to collect cover data. see \link{baf.bin.cloc}.
#' default is \code{mean}.
#' @return a list of length nrow(regions), each element is a numerical
#' vector of mean coverage in adjacent windows of size \code{binsize}
#' in the region.
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' x <- regions.bincover(baf, binsize=1000, use.threads=FALSE)
#' baf.close(baf)
#' @seealso \link{regions.bycover.range}, \link{regions.bycover.band}
#'
regions.bincover <- function(handle, regions=baf2clocs(handle),
                             binsize=10000L, fun=mean,
                             use.threads=lx.use.threads()) {

  # compute bins
  lx.out("computing binned coverage on ", nrow(regions)," regions")
  bins <- apply.cloc(regions, function(cloc, handle, binsize) {
            baf.bin.cloc(handle, cloc, binsize=binsize, what="cover",
                         fun=fun, drop=TRUE, .quick=!use.threads)
          },
          handle=handle, binsize=binsize,
          keep.order=TRUE,
          use.threads=use.threads)

  # keep chr index as names (may not be unique)
  names(bins) <- regions[,1]

  bins
}

# -------------------------------------------------
#' select regions in range of coverage
#' @description
#' loop over given \code{init} regions.
#' foreach of them keep regions with mean coverage 
#' in range [mincover, maxcover].
#' @param handle baf file handle (as returned by \link{baf.open})
#'        (ignored if \code{bins} is provided)
#' @param init clocations regions to start with (default is regions spanning all
#'        chromosomes). if \code{bins} is provided \code{init} should be the
#'        regions used to compute bins (thru \link{regions.bincover}).
#' @param bins list (of length \code{nrow(init)}) of binned coverage in regions
#'        (as returned by \link{regions.bincover}).
#'        if NULL this will be computed using \link{regions.bincover} with
#'        same parameters. (see notes)
#' @param binsize size of bins
#' @param mincover minimum coverage (default 0)
#' @param maxcover maximum coverage (default +Inf).
#' @param minreg minimum region size (should be >= binsize)
#' @param fun function to collect cover. see notes (default to mean)
#' @param keep.bins if TRUE, \code{bins, binsize and binrange} are kept as
#'        attributes in the result (set to FALSE to save memory)
#' @param use.threads (see \link{lx.use.threads})
#' @return a nx3 matrix of (1-based) clocations
#' @note the \code{bins} != NULL form is provided to avoid recomputation and
#' is mostly used internally (by \link{regions.bycover.band}).
#' @note code{fun} is an optional user function used in \link{regions.bincover}
#' to collect coverage. if provided, the range in \code{mincover, maxcover} should
#' scale these values. This parameter is ignored if \code{bins} are already 
#' provided.
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' x <- regions.bycover.range(baf, binsize=1000, mincover=1)
#' y <- attr(x, "binsize")
#' baf.close(baf)
#' @seealso \link{regions.bycover.band}
#'
regions.bycover.range <- function(handle, init=baf2clocs(handle),
                             bins=NULL,
                             binsize=10000L,
                             mincover=0, maxcover=Inf,
                             minreg=binsize,
                             fun=mean,
                             keep.bins=TRUE,
                             use.threads=lx.use.threads()) {
  
  # compute binned mean coverage
  #
  if (is.null(bins))
    bins <- regions.bincover(handle, init, binsize,
                             fun=fun, use.threads=use.threads)
  
  lx.stopif(length(bins) != nrow(init), "bins and init are of incompatible size")

  # get regions with valid counts
  #
  lx.out("gathering ", length(bins), " regions within counts range [",
         format(mincover, digits=2, nsmall=2), ", ", 
         format(maxcover, digits=2, nsmall=2), "]")

  res <- lx.lapply(seq_along(bins), function(i) {
    bin  <- bins[[i]]
    cloc <- init[i,]
    isok <- bit::as.bit((bin >= mincover) & (bin <= maxcover))
    runs2clocs(isok, chr=cloc[1], minsize=minreg, p0=cloc[2], delta=binsize)
  }, use.threads=use.threads,
     mc.preschedule=TRUE)

  res <- clocs.rbind(res)
  
  # keep selection bins as attribute
  #
  attr(res, "binsize")   <- binsize
  attr(res, "binpos")    <- (mincover + maxcover) / 2
  attr(res, "binrange")  <- c(mincover, maxcover)
  if (keep.bins)
    attr(res, "bins") <- bins

  res
}

# -------------------------------------------------
#' select regions in band of coverage distribution
#' @description
#' compute the distribution of (mean) coverage in all \code{init} regions,
#' select a band in this distribution according to different
#' models (see details).\cr
#' then loop over given \code{init} regions.
#' foreach of them keep regions with coverage in that band.
#' @details
#' let us call \code{dist} is the distribution of coverage in all bins of
#' size \code{binsize}.\cr
#' the band of coverage [a, b] is defined by (a combination of) various models,
#' specified by the following formula:\cr\cr
#' \code{ [alpha1 '*'] model1 ['+'|'-' [alphan '*'] modeln]*}\cr\cr
#' where:\cr
#' \itemize{
#' \item model="median": (a,b)=median(dist)-/+alpha*mad(dist)
#' \item model="poisson": (a,b)=mode(dist)+/-alpha*sqrt(mode(dist))\cr
#' where mode(dist) is the coverage value associated to the largest maximum
#' of the distribution. this model correspond roughly to a poisson distributed
#' coverage (when coverage is large enough).
#' \item model="peak": a=pos-alpha*left; b=pos+alpha*right\cr
#' where pos, left and right are the maximum peak parameters returned
#' by \link{lx.peaks}.
#' \item model="twin": a=pos-alpha*(pos-pos.left)/2; b=pos+alpha*(pos.right-pos)/2\cr
#' where pos, pos.left, pos.right are respectively the maximum peak position,
#' the immediately left and right maxima, as returned
#' by \link{lx.maxima}.
#' \item model=<name>: a and b are defined by a user-provided function called
#' as .band.name(bins, alpha, ...) that should returns c(a, b)
#' }
#' in the previous formula, models are evaluated from left to right, each returning
#' an interval (band) that are further combined in the following way:\cr
#' \code{[a,b] + [c,d] = [min(a,c), max(b,d)]} (extension)\cr
#' \code{[a,b] - [c,d] = [max(a,c), min(b,d)]} (reduction)
#' 
#' @param handle baf file handle (as returned by \link{baf.open})
#'        (ignored if \code{bins} is provided)
#' @param init clocations regions to start with (default is regions spanning all
#'        chromosomes). if \code{bins} is provided it should be the regions
#'        used to compute bins (thru \link{regions.bincover}).
#' @param binsize size of bins
#' @param model model formula (see details).
#' @param smooth.k k parameter to \link{lx.smooth.median} to smooth bins before
#'        computing distribution. (use NULL or 0 to disable smoothing)
#' @param minreg minimum region size (should be >= binsize)
#' @param fun function to collect cover. see notes (default to mean)
#' @param keep.bins if TRUE, \code{bins} are kept as
#'        attributes in the result (set to FALSE to save memory)
#' @param ... additional parameters to \code{model} user-defined function
#'        if specified (see details)
#' @param use.threads (see \link{lx.use.threads})
#' @return a nx3 matrix of (1-based) clocations
#' @note result also contains bins (and smoothed bins) statistics
#' such as range, mode and density.\cr\cr
#' if \code{smooth.k != 0} then regions selection is performed
#' on smoothed bins.\cr
#' note that the smoothing is performed on the concatenation of all regions,
#' therefore ignoring chromosomes and regions boundaries. this will be
#' improved in the future.\cr\cr
#' code{fun} is an optional user function used in \link{regions.bincover}
#' to collect coverage.
#' @examples
#' baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
#' x <- regions.bycover.band(baf, binsize=1000, smooth.k=3)
#' x <- regions.bycover.band(baf, binsize=1000, smooth.k=0, model="twin - 4 * poisson")
#' y <- attr(x, "binsize")
#' baf.close(baf)
#' @seealso \link{regions.bycover.range}
#'
regions.bycover.band <- function(handle, init=baf2clocs(handle),
                             binsize=10000L,
                             model="poisson",
                             smooth.k=c(3L, 5L, 15L, 35L, 55L),
                             minreg=binsize,
                             fun=mean,
                             keep.bins=TRUE,
                             ...,
                             use.threads=lx.use.threads()) {

  # ------------------------
  # internal utilities
  
  # internal cleanup
  .clean <- function(bins) {
    bins <- unlist(bins, use.names=FALSE)
    bins[bins != 0]  # remove 0 counts
  }
  
  # internal band models
  .band.median <- function(bins, alpha, ...) {
    bins     <- .clean(bins)
    bins.med <- median(bins)
    bins.mad <- mad(bins, center=bins.med)
    bins.min <- floor(max(0, bins.med - (alpha*bins.mad)))
    bins.max <- ceiling(bins.med + (alpha*bins.mad))
    band <- c(bins.min, bins.max)
    attr(band, "model") <- "median"
    attr(band, "binpos") <- bins.med
    band
  }
  
  .band.poisson <- function(bins, alpha, ...) {
    bins      <- .clean(bins)
    bins.dens <- lx.density(bins, n=1024)
    bins.pos  <-  bins.dens$x[lx.maxima(bins.dens$y, span=0)[[1]]]
    bins.min  <-  max(0, bins.pos - (alpha*sqrt(bins.pos)))
    bins.max  <-  bins.pos + (alpha*sqrt(bins.pos))
    band <- c(bins.min, bins.max)
    attr(band, "model") <- "poisson"
    attr(band, "binpos") <- bins.pos
    band
  }
  
  .band.peak <- function(bins, alpha, ...) {
    bins      <- .clean(bins)
    bins.dens <- lx.density(bins, n=1024)
    bins.peak <- lx.peaks(bins.dens$y, span=0, eps.x=0.1, na.val=100)
    bins.pos  <- bins.dens$x[bins.peak$pos[[1]]]
    bins.min  <- bins.dens$x[bins.peak$left[[1]]]
    bins.max  <- bins.dens$x[bins.peak$right[[1]]]
    bins.min  <- max(0, bins.pos - (bins.pos-bins.min)*alpha)
    bins.max  <- bins.pos + (bins.max-bins.pos)*alpha
    band <- c(bins.min, bins.max)
    attr(band, "model") <- "peak"
    attr(band, "binpos") <- bins.pos
    band
  }
  
  .band.twin <- function(bins, alpha, ...) {
    bins      <- .clean(bins)
    bins.dens <- lx.density(bins, n=1024)
    bins.peak <- lx.peaks(bins.dens$y, span=0, eps.x=0.1, na.val=100)
    bins.pos <- bins.dens$x[bins.peak$pos[1]]
    
    delta <- bins.peak$pos[1] - bins.peak$pos
    delta[delta <= 0] <- Inf
    imin <- which.min(delta)
    imin <- if (is.infinite(delta[imin])) 1 else bins.peak$pos[imin]
    bins.min <- bins.dens$x[imin]
    bins.min <- max(0, bins.pos - (bins.pos - bins.min)*alpha/2)
    
    delta <- bins.peak$pos - bins.peak$pos[1]
    delta[delta <= 0] <- Inf
    imin <- which.min(delta)
    imin <- if (is.infinite(delta[imin])) length(bins.dens$x) else bins.peak$pos[imin]
    bins.max <- bins.dens$x[imin]
    bins.max <- bins.pos + (bins.max - bins.pos)*alpha/2
    
    band <- c(bins.min, bins.max)
    attr(band, "model") <- "twin"
    attr(band, "binpos") <- bins.pos
    band
  }
  
  # band combination
  .combine.plus  <- function(b1, b2) c(pmin(b1, b2)[1], pmax(b1, b2)[2])
  .combine.minus <- function(b1, b2) c(pmax(b1, b2)[1], pmin(b1, b2)[2])
  
  # model parser + caller
  .call.formula <- function(form, bins, ...) {
    form <- gsub("[[:blank:]]+","", form)
    split.form <- lx.strsplit(form, "\\+|-")
    
    oper <- head(cumsum(sapply(split.form, nchar, USE.NAMES=F))
                 + seq_along(split.form), -1)
    oper <- c("-", sapply(oper, function(p) substr(form, p, p)))
    oper <- gsub("-", "minus", gsub("\\+", "plus", oper))
    
    fun  <- sapply(lapply(split.form, function(s) lx.strsplit(s, "\\*")), 
                  function(s) if (length(s) == 1) c(1,s) else s)
    
    res <- apply(fun, 2, function(x)
            do.call(get(paste0(".band.", x[2])),
                    list(bins, alpha=as.integer(x[1]), ...)))

    res <- Reduce(function(p, i) 
              do.call(paste0(".combine.", oper[i]), list(p, res[,i])),
              1:ncol(res), c(-Inf, Inf))
    res
  }
  
  
  # ------------------------
  # function body
  
  # compute binned counts
  #
  bins <- regions.bincover(handle, init, binsize, 
                           fun=fun,
                           use.threads=use.threads)
  # smooth if requested
  #
  smoothed <- ! (is.null(smooth.k) || all(smooth.k==0))
  
  if (smoothed) {
    flat.bins <- lx.smooth.median(unlist(bins, use.names=F), k=smooth.k)
    lens.bins <- sapply(bins, length)
    smooth.bins <- split(flat.bins, rep.int(seq_along(bins), lens.bins)) 
    names(smooth.bins) <- names(bins)
    sel.bins <- smooth.bins
  } else {
    sel.bins <- bins
  }

  # get and call band function
  #
  band <- tryCatch(.call.formula(model, sel.bins, ...),
                   error=function(e) lx.stopif(T, "invalid formula: ", model, 
                                               " ", e$message))
  
  lx.stopif(band[1] > band[2], "invalid cover band ", band)

  # filter regions
  #
  res <- regions.bycover.range(handle, init, sel.bins, binsize,
                                band[[1]], band[[2]], minreg, 
                                fun=NULL, keep.bins,
                                use.threads)
  
  # keep attributes
  #
    
  # copy band attributes
  for (att in setdiff(names(attributes(band)), "names"))
    attr(res, att) <- attr(band, att, exact=T)
  
  # add new attributes
  if (keep.bins)
    attr(res, "bins") <- bins
  attr(res, "bindens") <- lx.density(.clean(bins), n=1024)
  
  if (smoothed) {
    attr(res, "smooth.k") <- smooth.k
    if (keep.bins)
      attr(res, "smooth.bins") <- smooth.bins
    attr(res, "smooth.dens") <- lx.density(.clean(smooth.bins), n=1024)
  }

  res
}

# -------------------------------------------------
#' get regions containing only symbols [agct]
#' @description
#' loop over given \code{init} regions.
#' foreach of them keep regions containing only a,c,g or t's.\cr
#' @param handle basta file handle (as returned by \link{basta.open})
#' @param init regions to start with (default is regions spanning all chromosomes)
#' @param minreg minimum region size
#' @param use.threads (see \link{lx.use.threads})
#' @return a nx3 matrix of (1-based) clocations
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' x <- regions.byacgt(fh, minreg=1, use.threads=FALSE)
#' basta.close(fh)
#'
regions.byacgt <- function(handle, init=basta2clocs(handle),
                           minreg=10000L,
                           use.threads=lx.use.threads()) {

  # compute regions
  lx.out("computing acgt on regions")
  res <- apply.cloc(init, function(cloc, handle) {
              seq <- basta.fetch.cloc(handle, cloc)
              isok <- bit::bit(nchar(seq))
              isok[lx.strchr(seq, "ACGTacgt")] <- TRUE
              seq <- NULL
              runs2clocs(isok, cloc[1], minsize=minreg, p0=cloc[2]) 
            },
            handle=handle,
            use.threads=use.threads)

  clocs.rbind(res)
}

# -------------------------------------------------
#' get regions with specified gc content
#' @description
#' loop over given \code{init} regions.
#' foreach of them keep regions of specified %gc.\cr
#' @details there is a slight difference in the way the gc content is computed
#' depending whether you pass a basta or baf file handle.\cr
#' if a basta file handle is provided then the gc content is computed
#' on the basis of the reference genome.\cr
#' if a baf file handle is provided then the gc content is computed
#' on the basis of the actual observed alleles (see \link{baf.bin.cloc}).
#' note that this may lead to 0 counts in region with no mapping.\cr\cr
#' basta mode is (about 10 times) quicker than baf mode.
#' @param handle basta or baf file handle (as returned by \link{basta.open}
#' or \link{baf.open}). see details
#' @param init regions to start with (default is regions spanning all chromosomes)
#' @param winsize window size to compute gc content
#' @param gcrange percent gc range (should be in [0, 1])
#' @param minreg minimum final region size (should be >= winsize)
#' @param use.threads (see \link{lx.use.threads})
#' @return a nx3 matrix of (1-based) clocations
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' x <- regions.bygc(fh, winsize=3, gcrange=c(0., 0.5))
#' basta.close(fh)
#' @seealso \link{regions.strata.bygc} for a stratified version
#'
regions.bygc <- function(handle, init=basta2clocs(handle),
                         winsize=1000L, gcrange=c(0.0, 1.0),
                         minreg=winsize,
                         use.threads=lx.use.threads()) {
  
  lx.out("filtering regions by gc in [", gcrange[1], ", ", gcrange[2], "]")
  res <- apply.cloc(init, function(cloc, handle) {
    .bygc.cloc(handle, cloc, binsize=winsize,
               gcrange=gcrange, minreg=minreg)
  }, handle=handle, use.threads=use.threads)

  Reduce(rbind, res[-1], res[[1]])
}

# -------------------------------------------------
#' stratify regions by gc content
#' @description
#' stratify subregions from \code{init} regions into gc content
#' @details there is a slight difference in the way the gc content is computed
#' depending whether you pass a basta or baf file handle.\cr
#' if a basta file handle is provided then the gc content is computed
#' on the basis of the reference genome.\cr
#' if a baf file handle is provided then the gc content is computed
#' on the basis of the actual observed alleles (see \link{baf.bin.cloc}).
#' note that this may lead to 0 counts in region with no mapping.\cr
#' basta mode is (about 10 times) quicker than baf mode.
#' @param handle basta or baf file handle (as returned by \link{basta.open}
#' or \link{baf.open}). see details
#' @param init regions to start with (default is regions spanning all chromosomes)
#' @param winsize window size to compute gc content
#' @param nstrata number of \%gc strata (strata go from 0. to 1. by 1/nstrata)
#' @param minreg minimum final region size (should be >= winsize)
#' @param use.threads (see \link{lx.use.threads})
#' @return a vector of size nstrata. each element is a nx3 matrix of (1-based) clocations
#' @examples
#' fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
#' x <- regions.strata.bygc(fh, winsize=3)
#' basta.close(fh)
#' @seealso \link{regions.bygc} for a non stratified version
#' 
regions.strata.bygc <- function(handle, init=basta2clocs(handle),
                         winsize=1000L, nstrata=5L, minreg=winsize,
                         use.threads=lx.use.threads()) {
  
  lx.out("stratifying regions by gc")
  res <- apply.cloc(init, function(cloc, handle) {
    .strata.bygc.cloc(handle, cloc, binsize=winsize,
                      nstrata=nstrata, minreg=minreg)
  }, handle=handle, use.threads=use.threads)
  
  # sum up by strata
  #
  res <- do.call(mapply, c(rbind, res))
  
  # rename strata
  #
  names(res) <- sprintf("%.2f", (seq.int(nstrata) - 1) / nstrata)
  res
}

# -------------------------------------------------
#' trim regions
#' @description
#' remove \code{trim} on both sides of regions
#' and keep only regions whose size is >= minreg
#' @param regions regions to trim
#' @param trim size to remove on both ends
#' @param minreg minimum final region size (should be >= trim)
#' @return a nx3 matrix of (1-based) clocations
#'
regions.trim <- function(regions, trim=1000L, minreg=10000L) {

  regions[,2] <- regions[,2] + trim
  regions[,3] <- regions[,3] - trim
  
  regions <- regions[regions[,2]<=regions[,3],,drop=FALSE]

  clocs.threshold(regions, minsize=minreg)
}

# -------------------------------------------------
#' exclude locations from regions
#' @description
#' remove locations (+/- margin) from regions
#' and keep only regions whose size is >= minreg
#' @param handle basta or baf file handle (as returned by \link{basta.open} 
#' or \link{baf.open}
#' @param coords nx2 matrix of absolute coordinates (1-based) or
#'        vector of n absolute point coordinates to remove
#' @param init regions to start with (default is regions spanning all chromosomes)
#' @param spaceleft size to remove on left side of coords
#' @param spaceright size to remove on right side of coords
#' @param minreg minimum final region size
#' @param use.threads (see \link{lx.use.threads})
#' @return a nx3 matrix of (1-based) clocations
#'
regions.exclude <- function(handle, coords, init=basta2clocs(handle),
                            spaceleft=0L, spaceright=spaceleft, minreg=1L,
                            use.threads=lx.use.threads()) {

  ncols <- ncol(coords)
  if ( is.null(ncols) || (ncols < 2))
    coords <- cbind(coords, coords)

  clocs <- coords2clocs(handle, coords)
  clocs[,2] <- min(1, clocs[,2]-spaceleft)
  clocs[,3] <- clocs[,3]+spaceright
  clocs.inter(init, clocs, minsize=minreg,
              use.threads=use.threads)
}


