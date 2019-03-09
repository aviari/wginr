# -------------------------------------------------
# $Id: asdog.gccorrect.build.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# GC correction of read counts
#

# =================================================
# internals <no_export>
# =================================================

# =================================================
# API
# =================================================

# -------------------------------------------------
# compute GC corrected read counts
#
asdog.gccorrect <- function(model, params=model$params,
                            use.threads=lx.use.threads()) {

  #-------------------------------
  # init results
  #-------------------------------
  
  correct <- asdog.object('GCCorrect', params=params)
  
  asdog.print.params(asdog.filter.params(params, in.filter="gccorrect:"))

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
  
  correct$header <- basta$header

  #
  # read user's regions
  #
  
  .rsize <- function(x) x[,3] - x[,2] + 1
  .tsize <- function(x) sum(.rsize(x))
  
  lx.out("---------------------------------------", with.mem=T)
  lx.out("Read predictable regions")
  lx.out("---------------------------------------")

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
  # read user's regions
  #
  
  regions.names <- lapply(params$gccorrect.regions, function(x) {
      x <- sub('@BASE@', params$base, x)
      x <- sub('@base@', basename(params$base), x)
      x <- sub('@REF@',  params$ref, x)
      x <- sub('@ref@',  basename(params$ref), x)
      x
    })
  
  lx.out("predictable regions: ", paste(regions.names, collapse=', '))

  regions <- regions.bybed(basta,
                          regions.names,
                          init=regions,
                          minreg=params$gccorrect.binsize,
                          use.threads=use.threads)
  
  lx.out("yielding ", nrow(regions), 
         " regions covering ", .tsize(regions), " bp")

  # keep regions in correct$regions
  #
  
  correct$regions <- regions
  
  lx.out("---------------------------------------", with.mem=T)
  lx.out("Get predicted positions")
  lx.out("---------------------------------------")
  
  #
  # position = center of bin
  #

  coords <- apply.clocs(regions, function(clocs, handle, binsize) {
      unlist(apply(clocs2coords(handle, clocs), 1, function(x) {
        seq.int(x[1], x[2]-binsize+1, by=binsize) + floor(binsize/2)
      }), use.names=F)
  }, handle=basta,
     binsize=params$gccorrect.binsize,
     use.threads=F)
  
  relpos <- lapply(coords, function(coords)
                    as.vector(coords2clocs(basta, coords)[,2]))

  lx.out("---------------------------------------", with.mem=T)
  lx.out("Get raw read counts")
  lx.out("---------------------------------------")

  raw <- apply.clocs(regions, function(clocs, handle, binsize, collect) {
      as.vector(unlist(apply(clocs, 1, function(cloc) {
        baf.bin.cloc(handle, cloc, binsize, what="cover", fun=collect, drop=T)
      }), use.names=F))
  }, handle=baf, 
     binsize=params$gccorrect.binsize,
     collect=lx.get0(params$gccorrect.collect, mode="function"),
     use.threads=use.threads)

  lx.out("---------------------------------------", with.mem=T)
  lx.out("Get raw GC")
  lx.out("---------------------------------------")

  gc <- apply.clocs(regions, function(clocs, handle, binsize) {
    as.vector(unlist(apply(clocs, 1, function(cloc) {
      basta.count.cloc(handle, cloc, sym="gcGC", binsize=binsize, drop=T)/binsize
    }), use.names=F))
  }, handle=basta,
  binsize=params$gccorrect.binsize,
  use.threads=use.threads)

  lx.out("---------------------------------------", with.mem=T)
  lx.out("Collecting rcgc datasource")
  lx.out("---------------------------------------")
  
  datasrc <- asdog.rcgc.datasrc(basta, NULL,
                             chrs=chrs,
                             smooth.k=params$gcmodel.datasrc.smooth,
                             grain=params$gcmodel.datasrc.grain,
                             use.threads=use.threads)
  
  lx.out("---------------------------------------", with.mem=T)
  lx.out("Compute predicted read counts")
  lx.out("---------------------------------------")
  
  pred <- lapply(coords, function(coords) {
            asdog.gcPredict.gloess(datasrc, model$optim$model, 
                                   coords, use.threads=F)$cov
          })


  lx.out("---------------------------------------", with.mem=T)
  lx.out("Compute corrected relative read counts")
  lx.out("---------------------------------------")

  EPS <- 1e-6
  
  # prediction normalized relative read counts
  
  rrc.cor <- mapply(function(raw, pred) {
                     raw / (pred + EPS)
                   }, raw, pred, SIMPLIFY=F)

  # median normalized relative read counts

  med <- median(unlist(raw, use.names=F), na.rm=T)
  
  rrc.raw <- lapply(raw, function(raw) {
                    raw / (med + EPS)
                   })

  lx.out("---------------------------------------", with.mem=T)
  lx.out("Reordering by chromosome")
  lx.out("---------------------------------------")

  correct$bychr <- mapply(function(coord, pos, gc, raw, pred, rcr, rcc) 
                            {list(coord=coord, pos=pos, gc=gc, raw=raw, 
                                  pred=pred, rrc.raw=rcr, rrc.cor=rcc)},
                          coords, relpos, gc, raw, pred, rrc.raw, rrc.cor,
                          SIMPLIFY=F)
  
  #
  # end
  #
  
  basta.close(basta)
  baf.close(baf)
  
  lx.out("---------------------------------------", with.mem=T)
  lx.out("End")
  lx.out("---------------------------------------")

  lx.info(correct)           <- 'GC corrected read counts predictions'
  lx.info(correct$params)    <- 'computation parameters'
  lx.info(correct$header)    <- 'chromosomes information'
  lx.info(correct$regions)   <- 'selected (mappable) regions for predictions'
  lx.info(correct$bychr)     <- 'read counts raw and corrected by chromosomes'

  correct  
}

# -------------------------------------------------
# some S3 helper
#

# print overload

print.GCCorrect <- function(correct) lx.doc(correct, .name='GCCorrect')

