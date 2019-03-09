# -------------------------------------------------
# $Id: asdog.gcmodel.build.r 355 2017-12-11 07:50:48Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# GC model contruction
#

# -------------------------------------------------
# sample locations in provided regions
# @param handle baf handle
#
asdog.sample <- function(handle, regions, size=1000000L, minspacin=0L,
                         minreg=10000L, sorted=TRUE, maxloop=50L,
                         use.threads=lx.use.threads()) {

  if (size <= 0)
    return(integer(0))

  tot.size <- sum(regions[,3]-regions[,2]+1)

  if (tot.size == 0) {
    lx.warn("sampling empty regions yield empty sample")
    return(integer(0))
  }
  
  #
  # sample size points
  #
  if (minspacin <= 0) {
    #
    # simple version: without minimum spacing
    #
    lx.out("standard sampling ", size, " points" )
    sampled <- coords.sample(handle, regions, size=size)
  }
  else {
    #
    # more sophisticated version : with minimum spacing
    #
    # first determine sampling factor
    #
    tot.loop <- ceiling(2.7 * minspacin * size / tot.size)
    if (tot.loop > maxloop) {
      lx.warn("minspacin ", minspacin, " too large, for maxloop ", maxloop)
      minspacin <- ceiling(maxloop * tot.size / size / 2.7)
      lx.out("minspacin reduced to  ", minspacin)
    }
    samp.size <- min(1.2 * size, ceiling(tot.size / minspacin))
    #
    # sampling loop
    #
    sampled <- integer(0)
    samp.try <- samp.tot <- 0
    lx.out("discrete sampling ", size , " points by ", samp.size)
    while ((samp.tot < size) && (samp.try < 2*maxloop)) {
      samp.try <- samp.try + 1
      lx.out("subsampling #", samp.try)
      coords <- sort(coords.sample(handle, regions, size=samp.size))
      coords <- coords[diff(coords) > minspacin]
      sampled <- c(sampled, coords)
      samp.tot <- length(sampled)
      lx.out("subsampled ", length(coords), " points, total ", samp.tot, "/", size, " (", round(samp.tot*100/size),"%)")
      regions <- regions.exclude(handle, coords, spaceleft=minspacin,
                                 spaceright=minspacin, minreg=minreg, use.threads=use.threads)
    }
    #
    # resample to requested size
    #
    if (length(sampled) > size) {
      lx.out("final resampling to size ", size)
      sampled <- lx.sample(sampled, size=size, replace=F)
    }
    lx.out("total sampled ", length(sampled))
  }

  lx.out("  sampling done", with.mem=T)
  
  if (sorted) sort(sampled) else sampled
}

# -------------------------------------------------
# sample locations by gc strata in provided regions
#
asdog.sample.bygc <- function(handle.basta, regions, 
                              size=1000000, gcwindow=1000, gcbins=4,
                              minspacin=0, minreg=gcwindow,
                              flatten=TRUE, sorted=TRUE, maxloop=50,
                              use.threads=lx.use.threads()) {
  # split regions by gc
  #
  regions <- regions.strata.bygc(handle.basta, regions, winsize=gcwindow, 
                                 nstrata=gcbins, minreg=minreg,
                                 use.threads=use.threads)

  # init loop
  #
  totsize <- rep(0, gcbins)
  allow   <- rep(T, gcbins)
  sampled <- rep(list(NULL), gcbins)

  while (sum(totsize) < size) {

    left <- size - sum(totsize)
    reqsize <- ceiling(rep(left, gcbins) / sum(allow))
    reqsize[! allow] <- 0
    
    lx.out("sampling by gc ", left, " points left on ", size, " (", round((size-left)*100/size), "% done)")

    # sample within regions
    #
    samp <- lapply(seq_along(regions), function(i) {
      asdog.sample(handle.basta, regions[[i]], size=reqsize[i], minspacin=minspacin,
                 minreg=minreg, sorted=sorted, maxloop=maxloop, use.threads=use.threads)
    })
    sampsize <- sapply(samp, length)
    sampled <- mapply(c, sampled, samp, SIMPLIFY=F)
    
    allow <- allow & (sampsize > reqsize/2)
    totsize <- totsize + sampsize

    if (sum(sampsize) == 0) {
      lx.warn("sampling exhausted at ", sum(totsize), " points")
      break
    }
  }
  
  if (flatten) {
    sampled <- unlist(sampled, use.names=F)
    if (sorted) sampled <- sort(sampled)
  }
  
  sampled
}


# =================================================
# API
# =================================================

# -------------------------------------------------
#' compute GCCorrect model
#' @description
#' Empirical loess GC model
#
asdog.gcmodel <- function(params, use.threads=lx.use.threads()) {
  
  #-------------------------------
  # init results
  #-------------------------------
  
  model <- asdog.object('GCModel', params=params)
  
  asdog.print.params(asdog.filter.params(params, in.filter="gccorrect:"))
  
  #-------------------------------
  # open the basta and baf files
  #-------------------------------
  
  lx.out("---------------------------------------")
  lx.out("Opening Basta and Baf files")
  lx.out("---------------------------------------")

  name <- asdog.check.file(params$ref, "bst")
  basta <- basta.open(name)
  
  name <- asdog.check.file(params$base, "baf")
  baf <- baf.open(name)

  #
  # keep sequence header
  #
  
  model$header <- basta$header
  
  #-------------------------------
  # prepare sampling regions
  #-------------------------------
  
  lx.out("---------------------------------------")
  lx.out("Prepare sampling regions")
  lx.out("---------------------------------------")
  
  .rsize <- function(x) x[,3] - x[,2] + 1
  .tsize <- function(x) sum(.rsize(x))

  #
  # all regions from basta
  #

  regions <- basta2clocs(basta)
  
  chrs <- regions[,1]
  
  lx.out("basta regions: ", length(chrs), " chromosomes")

  if (length(params$chrs) == 0)
    params$chrs = chrs

  chrs <- intersect(chrs, params$chrs)  
  
  lx.stopif(length(chrs) == 0, "empty list of chromosomes")
  
  lx.out("filtering chromosomes indexes [", chrs, "]")
  
  regions <- regions[chrs,,drop=F]

  #
  # collect datasource for speedup
  # 
  
  lx.out("loading rcgc datasource", with.mem=T)
  
  datasrc <- asdog.rcgc.datasrc(basta, baf,
                                chrs=chrs,
                                smooth.k=params$gcmodel.datasrc.smooth,
                                grain=params$gcmodel.datasrc.grain,
                                use.threads=use.threads)
  #
  # read user's regions
  #
  
  regions.names <- lapply(params$gcmodel.regions.name, function(x) {
      x <- sub('@BASE@', params$base, x)
      x <- sub('@base@', basename(params$base), x)
      x <- sub('@REF@',  params$ref, x)
      x <- sub('@ref@',  basename(params$ref), x)
      x
    })
  
  lx.out("user's regions: ", paste(regions.names, collapse=', '))

  regions <- regions.bybed(basta, regions.names, 
                          init=regions,
                          minreg=params$gcmodel.regions.minreg,
                          use.threads=use.threads)
  
  lx.out("yielding ", nrow(regions), 
         " regions covering ", .tsize(regions), " bp")

  #
  # remove regions with N's
  #
  
  lx.out("removing regions with N's")
  regions <- regions.byacgt(basta, init=regions, 
                            minreg=params$gcmodel.regions.minreg,
                            use.threads=use.threads)
  lx.out("yielding ", nrow(regions), 
         " regions covering ", .tsize(regions), " bp")
  
  #
  # make large regions (for large GC window model only)
  #
  
  has.large.window <- max(params$gcmodel.large.winsize) != 0
  
  if (has.large.window) {

    lx.out("making large windows regions")
    
    lwidth <- max(params$gcmodel.large.winsize)
    delta  <- params$gcmodel.regions.joinsize[1]
    regions.large <- clocs.join(regions, delta=delta, minsize=1)
    
    while (median(.rsize(regions.large)) < lwidth) {
      delta <- 2 * delta
      if (delta > params$gcmodel.regions.joinsize[2]) break
      regions.large <- clocs.join(regions, delta=delta, minsize=1)
      lx.out("increasing delta-join to ", delta)
    }
    
    regions.large <- clocs.join(regions, delta=delta, minsize=lwidth)
    regions.large <- regions.trim(regions.large, trim=lwidth/2,
                                  minreg=params$gcmodel.regions.minreg)
    
    lx.out("yielding ", nrow(regions.large), 
           " large regions covering ", .tsize(regions.large), " bp")
  }
  
  #
  # make first rough GC model
  # we first get mean of raw (uncorrected) coverage
  # per chunks of 10K in each region, compute smoothed
  # coverage distribution and select a single band
  # in this distribution.
  # this provides the region pool for sampling points
  # to build the empirical GC model
  #
  
  if (params$gcmodel.regions.win0 != 0) {
    
    lx.out("making stage0 GC model")
    
    regions0 <- regions.bycover.band(baf, init=regions,
                                     binsize=params$gcmodel.regions.binsize,
                                     model=params$gcmodel.regions.band0,
                                     smooth.k=params$gcmodel.regions.smooth,
                                     minreg=params$gcmodel.regions.minreg,
                                     keep.bins=FALSE,
                                     use.threads=use.threads)
    
    sample0 <- asdog.sample(basta, regions0, size=params$gcmodel.sample.size,
                            minspacin=params$gcmodel.sample.spacin,
                            minreg=params$gcmodel.regions.minreg,
                            sorted=TRUE,
                            use.threads=use.threads)
    
    model0 <- asdog.gcModel.gloess(datasrc, sample0,
                                   winsize.gc=params$gcmodel.regions.win0,
                                   winsize.cov=params$gcmodel.regions.win0,
                                   fit=params$gcmodel.gloess.fit,
                                   span=params$gcmodel.loess.span,
                                   base.quant=params$gcmodel.base.quant,
                                   base.trim=params$gcmodel.base.trim,
                                   nband=params$gcmodel.nband[2],
                                   use.threads=use.threads)
    gloess0 <- model0$gloess[[1]]
    
    sample0.baf <- baf.fetch.coord(baf, sample0)
    medcov <- median(rowSums(sample0.baf))
    
    #
    # closure to collect GC corrected coverage instead of raw coverage
    # note : the formal argument should be named as 'count'
    #
    .collect <- function(count) {
      rowcov <- rowSums(count)
      totcov <- sum(rowcov)
      if (totcov == 0) return(0)
      gc <- sum(count[,'G'] + count[,'C'])/totcov
      cov <- mean(rowcov)
      pcov <- gloess0$pvec[round(gc*gloess0$N)+1]
      cov / pcov * medcov
    }
    
  } else {
    .collect <- mean  # standard collect
  }
    
  #
  # now select 'monoploid' regions.
  # to this purpose, we first get mean GC-corrected coverage
  # per chunks of 10K in each region, compute smoothed
  # coverage distribution and select a single band
  # in this distribution.
  # there are several possible flavors for defining this
  # band: => params$gcmodel.regions.band
  #
   
  lx.out("selecting monoploid regions : ", params$gcmodel.regions.band)
  
  regions <- regions.bycover.band(baf, init=regions,
                                  binsize=params$gcmodel.regions.binsize,
                                  model=params$gcmodel.regions.band,
                                  smooth.k=params$gcmodel.regions.smooth,
                                  minreg=params$gcmodel.regions.minreg,
                                  fun=.collect,
                                  keep.bins=TRUE,
                                  use.threads=use.threads)

  lx.out("yielding ", nrow(regions), 
         " regions covering ", .tsize(regions), " bp")
  
  # keep regions bins in model$regions
  #
  
  model$regions <- list()
  
  for (att in setdiff(names(attributes(regions)), c("dim", "names", "dimnames"))) {
    model$regions[[att]] <- attr(regions, att)
    attr(regions, att) <- NULL
  }

  #
  # finally trim 1 kb from each region
  #
  
  lx.out("trimming regions to ", max(params$gcmodel.small.winsize))
  regions <- regions.trim(regions,
                          trim=max(params$gcmodel.small.winsize), ## check this
                          minreg=params$gcmodel.regions.minreg)
  
  lx.out("yielding ", nrow(regions), 
         " regions covering ", .tsize(regions), " bp")
  
  #
  # intersect with large regions
  #
  
  if (has.large.window) {
    lx.out("intersect with large windows")
    regions <- clocs.inter(regions, regions.large, 
                           minsize=params$gcmodel.regions.minreg,
                           use.threads=use.threads)
    lx.out("yielding ", nrow(regions), 
           " regions covering ", .tsize(regions), " bp")
  } 
  
  #
  # keep final regions in model$regions
  #
  
  lx.out("final ", nrow(regions), 
         " regions covering ", .tsize(regions), " bp")

  model$regions$regions <- regions
  
  #-------------------------------
  # sample
  #-------------------------------
  
  lx.out("---------------------------------------", with.mem=T)
  lx.out("Sampling regions")
  lx.out("---------------------------------------")

  if (params$gcmodel.sample.gcbins <= 1) {
    #
    # standard sampling
    #
    sampled <- asdog.sample(basta, regions, size=params$gcmodel.sample.size,
                            minspacin=params$gcmodel.sample.spacin,
                            minreg=params$gcmodel.regions.minreg,
                            sorted=TRUE,
                            use.threads=use.threads)
  }
  else {
    #
    # sampling by gc regions
    #
    sampled <- asdog.sample.bygc(basta, regions,
                                 size=params$gcmodel.sample.size,
                                 gcwindow=params$gcmodel.sample.gcwindow,
                                 gcbins=params$gcmodel.sample.gcbins,
                                 minspacin=params$gcmodel.sample.spacin,
                                 minreg=params$gcmodel.regions.minreg,
                                 flatten=TRUE, sorted=TRUE,
                                 use.threads=use.threads)
  }

  model$sample <- list()
  model$sample$size <- length(sampled)
  model$sample$coords <- sampled

  # stop here on no sampling
  #
  lx.stopif(model$sample$size == 0, "no sampled data", trace=F)
  
  #-------------------------------
  # winsize optimization
  #-------------------------------
 
  lx.out("---------------------------------------", with.mem=T)
  lx.out("GC model winsize optimization")
  lx.out("---------------------------------------")
  
  # collect sample coverage
  #
  lx.out("collecting sample coverage", with.mem=T)
  
  model$sample$pts <- asdog.rcgc.collect(datasrc, sampled,
                                        winsize.gc=params$gccorrect.binsize,
                                        winsize.cov=params$gccorrect.binsize,
                                        use.threads=use.threads)
                                        
  # optimize gc windows
  # 
  lx.out("optimizing windows", with.mem=T)
  
  model$optim <- asdog.gcOptimize.gloess(datasrc, sampled,
                         learn.ratio=params$gcmodel.learn.ratio,
                         winsize.gc=list(params$gcmodel.small.winsize,
                                         params$gcmodel.large.winsize),
                         winsize.cov=params$gccorrect.binsize,
                         fit=params$gcmodel.gloess.fit,
                         objective=params$gcmodel.optim.objective,
                         span=params$gcmodel.loess.span,
                         base.quant=params$gcmodel.base.quant,
                         base.trim=params$gcmodel.base.trim,
                         nband=params$gcmodel.nband[2],
                         use.threads=use.threads)
  
  #
  # end
  #
  
  basta.close(basta)
  baf.close(baf)
  
  lx.out("---------------------------------------", with.mem=T)
  lx.out("End")
  lx.out("---------------------------------------")
  
  lx.info(model)           <- 'GC GLoess correction model'
  lx.info(model$params)    <- 'computation parameters'
  lx.info(model$header)    <- 'chromosomes information'
  lx.info(model$regions)   <- 'selected regions for GC model'
  lx.info(model$sample)    <- 'sampled positions and values'
  lx.info(model$optim)     <- 'optimized model'

  model
}

# -------------------------------------------------
# some S3 helper
#

# print overload

print.GCModel <- function(model) lx.doc(model, .name='GCModel')

# plot overload
# @todo

