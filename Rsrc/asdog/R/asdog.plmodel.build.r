# -------------------------------------------------
# $Id: asdog.plmodel.build.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# Ploidy model : build RCAF model API
# @seealso asdog.plmodel.model : RCAF model equations
#


# -------------------------------------------------
# make ploidy model
#
# rcsource = object of class GCCorrect (new computation) 
#            or PLModel (recompute model)
#

asdog.plmodel <- function(rcsource, params=rcsource$params,
                          use.threads=lx.use.threads()) {

  mode <- if ("PLModel" %in% class(rcsource)) "recompute" else
          if ("GCCorrect" %in% class(rcsource)) "newmodel" else
          NULL
  
  lx.stopif(is.null(mode), 
            "data source should be of class GCCorrect or PLModel")

  #-------------------------------
  # init results
  #-------------------------------
  
  model <- asdog.object('PLModel', params=params)
  
  preseg <- params$plmodel.preseg.mode
  pexcl <- if (preseg == "shmm") "bcp:" else "shmm:"
  asdog.print.params(asdog.filter.params(params, in.filter="cnpredict:", out.filter=pexcl))
  
  #
  # keep sequence header
  #
  
  model$header <- rcsource$header
  
  #-------------------------------
  # Load RCAF data
  #-------------------------------
  
  if (mode == "newmodel") {
    
    #-------------------------------
    # open the baf files
    #-------------------------------
    
    lx.out("---------------------------------------")
    lx.out("Opening Tumour and Normal Baf files")
    lx.out("---------------------------------------")
    
    name <- asdog.check.file(params$base, "baf")
    tum.hdle <- baf.open(name)
    
    name <- asdog.check.file(params$normal, "baf")
    nor.hdle <- baf.open(name)

    lx.out("---------------------------------------")
    lx.out("Loading RC-AF data")
    lx.out("---------------------------------------")
    
    rcaf <- asdog.rcaf.datasrc(nor.hdle, tum.hdle, rcsource, 
                               chrs=params$chrs,
                               lowread=params$plmodel.snp.lowread,
                               mincov=params$plmodel.snp.mincov,
                               deltafreq=params$plmodel.snp.dfreq,
                               use.threads=use.threads)
    
    baf.close(nor.hdle)
    baf.close(tum.hdle)
    
  } else {
    
    lx.out("---------------------------------------")
    lx.out("recovering RC-AF from previous model")
    lx.out("---------------------------------------")
    
    rcaf <- rcsource$rcaf
    
    lx.out("RC-AF data : ", nrow(rcaf), " normal heterozygous sites")
  }
  
  #-------------------------------
  # Get user's regions
  #-------------------------------

  .rsize <- function(x) x[,3] - x[,2] + 1
  .tsize <- function(x) sum(.rsize(x))
  
  regions <- basta2clocs(model)
  
  chrs <- regions[,1]
  
  lx.out("basta regions: ", length(chrs), " chromosomes")
  
  if (length(params$chrs) == 0)
    params$chrs = chrs
  
  chrs <- intersect(chrs, params$chrs)  
  
  lx.stopif(length(chrs) == 0, "empty list of chromosomes")
  
  lx.out("filtering chromosomes indexes [", chrs, "]")
  
  regions <- regions[chrs,,drop=F]

  if (! is.null(params$plmodel.regions)) {
    
    lx.out("---------------------------------------")
    lx.out("Get User's regions")
    lx.out("---------------------------------------")
    
    #
    # read user's regions
    #
    
    regions.names <- lapply(params$plmodel.regions, function(x) {
      x <- sub('@BASE@', params$normal, x)
      x <- sub('@base@', basename(params$normal), x)
      x <- sub('@REF@',  params$ref, x)
      x <- sub('@ref@',  basename(params$ref), x)
      x
    })
    
    lx.out("user's regions: ", paste(regions.names, collapse=', '))
    
    regions <- regions.bybed(model, regions.names, 
                             init=regions,
                             minreg=1L,
                             use.threads=use.threads)
    
    lx.out("yielding ", nrow(regions), 
           " regions covering ", .tsize(regions), " bp")
    
    lx.out("intersecting heterozygous sites with regions")
    
    hs.clocs <- clocations(rcaf[,c(1,2,2)])
    hs.ok <- clocs.covered(hs.clocs, regions, use.threads=use.threads)
    
    lx.out(sum(hs.ok), " / ", length(hs.ok), 
           " normal heterozygous sites kept (",
           sprintf("%.1f", sum(hs.ok)*100/length(hs.ok)),
           " %)")
    
    rcaf <- rcaf[hs.ok,]
  } 
  
  #-------------------------------
  # Pre-Segmentation
  #-------------------------------
  
  # minseg.size  Mb -> sampled size
  
  totsize  <- .tsize(regions)
  rcafsize <- nrow(rcaf)
  sampsize <- params$plmodel.preseg.sampsize
  sampsize <- min(if (sampsize <= 0) rcafsize else sampsize, rcafsize)
  segsize <- params$plmodel.preseg.segsize * 1e6 # param in Mb
  segsize <- max(2, round(segsize * sampsize / totsize))
  
  lx.out("pre-segmentation minseg.size : ", segsize)
  lx.out("pre-segmentation mode : ", preseg)
  

  if (preseg == "shmm") {  
    
    lx.out("HMM smoothing")

    rc.ctrl <- list(rcmax=params$plmodel.preseg.rcmax, 
                    raw=params$plmodel.shmm.rc.raw,
                    smooth=params$plmodel.shmm.rc.smooth,
                    eps.max=params$plmodel.shmm.rc.eps,
                    max.levels=params$plmodel.shmm.rc.maxlev, 
                    sample.size=sampsize,
                    minseg.size=segsize,
                    tau=params$plmodel.shmm.rc.tau)
    
    af.ctrl <- list(raw=params$plmodel.shmm.af.raw,
                    smooth=params$plmodel.shmm.af.smooth,
                    eps.max=params$plmodel.shmm.af.eps,
                    max.levels=params$plmodel.shmm.af.maxlev, 
                    sample.size=sampsize,
                    minseg.size=segsize,
                    tau=params$plmodel.shmm.af.tau)
    
    segres <- asdog.smooth.hmm.segment(rcaf$rrc, rcaf$baf,
                                       rc.ctrl=rc.ctrl, af.ctrl=af.ctrl)
  } else {
    
    lx.out("BCP")
    
    segres <- asdog.bcp.segment(rcaf$rrc, rcaf$baf,
                                rcmax=params$plmodel.preseg.rcmax,
                                smooth.rc=params$plmodel.bcp.rc.smooth,
                                smooth.af=params$plmodel.bcp.af.smooth, 
                                p0=params$plmodel.bcp.p0,
                                sample.size=sampsize,
                                minseg.size=segsize)
                                
  }

  #-------------------------------
  # Parameter fitting
  #-------------------------------
  
  lx.out("---------------------------------------")
  lx.out("Fitting model parameters")
  lx.out("---------------------------------------")
  
  # making obs structure
  
  lx.out("fit segment source : ", params$plmodel.fit.segsrc)
  obs <- segres$seg[,1,drop=F]
  obs$rc <- segres$seg[[params$plmodel.fit.segsrc[1]]]
  obs$af <- segres$seg[[params$plmodel.fit.segsrc[2]]]
  obs$weight <- log10(segres$seg$weight)
  obs <- obs[,-1]

  if (FALSE) {  # testing
    qsd.rc <- quantile(segres$seg$sd.rc, 0.90)
    qsd.af <- quantile(segres$seg$sd.af, 0.90)
    hsd <- (segres$seg$sd.rc > qsd.rc) | (segres$seg$sd.af > qsd.af)
    obs <- obs[!hsd,]
  }
  
  # threshold quantile
  #
  # obs$weight <- obs$weight * sqrt(((obs$rc-1)^2 + (obs$af-0.5)^2))
  #
  quant <- quantile(obs$weight, params$plmodel.fit.weightquant)
  obs <- obs[obs$weight >= quant,,drop=F]
  lx.out("segment weight quantile ", params$plmodel.fit.weightquant*100,
         " : ", sprintf("%.2f", quant))

  # fit
  #
  lx.out("fitting ploidy model", with.mem=T)
  
  fit <- plmodel.fit(obs,
                     min.A=params$plmodel.fit.arange[1],
                     max.A=params$plmodel.fit.arange[2],
                     step.A=params$plmodel.fit.arange[3],
                     min.Q=params$plmodel.fit.qrange[1],
                     max.Q=params$plmodel.fit.qrange[2],
                     step.Q=params$plmodel.fit.qrange[3],
                     theo.weight=params$plmodel.fit.thweight,
                     gamma0=params$plmodel.fit.gamma,
                     rho=params$plmodel.fit.rho,
                     use.threads=use.threads)
  
  fit$status <- "tumour"
  
  #
  # check if we should revert to Normal
  #
  if ((nrow(obs) == 1) || (fit$a0 >= params$plmodel.alpha.normal)) {
    lx.warn("this appears to be a normal sample, status set to 'normal'")
    fit$status <- "normal"
    fit$a0 <- 0
    fit$q0 <- 2
  }
  
  lx.out("best model alpha=", fit$a0, " Q=", fit$q0, 
         " qual=", round(fit$qual, 2), " status=", fit$status)
  
  qrange <- params$plmodel.fit.qrange[1]:params$plmodel.fit.qrange[2]
  fit$theo <- asdog.theo.RCAF(fit$a0, fit$q0, qrange=qrange)

  #-------------------------------
  # compute mean ploidy
  #-------------------------------
  
  rc <- rcaf$rrc
  iq <- quantile(rc, c(0.01, 0.99))
  rc <- mean(rc[(rc >= iq[1]) & (rc <= iq[2])])
  mean.ploidy <- round(asdog.absCN(rc, fit$a0, fit$q0), digits=2)
  
  lx.out("best mean ploidy=", mean.ploidy)
  
  #-------------------------------
  # compute expected profiles
  # @todo
  #-------------------------------
  
  #-------------------------------
  # keep some information in model
  #-------------------------------
  #
  model$regions <- regions
  model$rcaf    <- rcaf
  model$seg     <- segres$seg
  model$fit     <- fit
  model$best    <- list(alpha=fit$a0,
                        q0=fit$q0,
                        qmean=mean.ploidy,
                        qual=fit$qual)

  #
  # end
  #

  lx.out("---------------------------------------", with.mem=T)
  lx.out("End")
  lx.out("---------------------------------------")
  
  lx.info(model)         <- 'Ploidy model'
  lx.info(model$params)  <- 'computation parameters'
  lx.info(model$header)  <- 'chromosomes information'
  lx.info(model$regions) <- 'regions for selecting SNPs'
  lx.info(model$rcaf)    <- 'RC-AF full data'
  lx.info(model$seg)     <- 'RC-AF pre-segments'
  lx.info(model$fit)     <- 'model fit info'
  lx.info(model$best)    <- 'best model parameters (alpha, Q)'

  model
}

# -------------------------------------------------
# some S3 helper
#

# print overload

print.PLModel <- function(model) lx.doc(model, .name='PLModel')

##
## [AV] dev test
##

if (FALSE) {
  
  require(asdog)
  
  plmodel <- readRDS("/TmpStorage/GYNCS.PA16014.PR16020.WGS.plmodel.rds")  
#  gccorrect <- readRDS("/TmpStorage/GYNCS.PA16014.PR16020.WGS.gccorrect.rds")  

  plmodel <- readRDS("/TmpStorage/GYNCS.PA1600A.PR16013.WGS.plmodel.rds")  
  
  params = asdog.new.params()
  plmodel <- asdog.plmodel(plmodel, params, use.threads=T)

  plot(plmodel)
  plot(plmodel, what="fit")
  
}



