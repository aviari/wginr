# -------------------------------------------------
# $Id: asdog.segment.build.r 497 2019-03-11 09:45:44Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# CN & AF final segmentation
# bivariate normal (CN) & binormal (AF) model
#

# -------------------------------------------------
# <internal> collect segments from viterbi states
#            on single chromosome
#
.collect.segments <- function(states, pos, chr, ...) {
  rle <- rle(states)
  ifrom <- head(c(0, cumsum(rle$lengths)) + 1, -1)
  seg <- data.frame(cbind(chr=chr,
                          from=pos[ifrom],
                          to=pos[ifrom+rle$lengths-1],
                          state=rle$values))
  data <- list(...)
  for (e in names(data)) {
    seg[,e] <- data[[e]][rle$values]
  }
  
  seg
}

# -------------------------------------------------
# <internal> filter segments on single chromosome
# note: regions should be on the same chromosome
#
.filter.segments <- function(seg, minsize=1L, joinsize=1L, regions=NULL) {
  
  #
  # intersect with regions if provided
  #
  # note: we cannot simply intersect the segments with regions
  # because the intersection will artificially join contiguous
  # segments with different states. So we have to proceed state
  # by state instead
  #
  if (! is.null(regions)) {
    seg.state <- split(seg, as.factor(seg$state))
    res.state <- lapply(seg.state, function(seg) {
      clocs <- clocations(cbind(chr=seg$chr, from=seg$from, to=seg$to))
      clocs <- clocs.inter(clocs, regions, use.threads=FALSE)
      res  <- data.frame(chr=clocs[,1], from=clocs[,2], to=clocs[,3])
      iseg <- lx.findInterval(clocs[,2], seg$from)
      for (e in setdiff(colnames(seg), colnames(res))) {
        res[,e] <- seg[,e][iseg]
      }
      res
    })
    res <- do.call(rbind, res.state)
  } else {
    res <- seg
  }
  
  #
  # filter out dust segments
  # that may result from intersection
  #
  res <- res[res$to-res$from+1 >= minsize,,drop=F]
  if (nrow(res) == 0) return(res)
  
  #
  # join close segments on same state
  # this is tricky - needs comment
  #
  res <- res[order(res$from),]
  lres <- split(res, seq(nrow(res)))
  lres <- Reduce(function(p, x) {
    fused <- ((x$from - p$to) < joinsize) && (x$state == p$state)
    if (fused) x$from <- p$from
    x }, lres, lres[[1]], accumulate=T)
  res <- do.call(rbind, tail(lres, -1))
  clocs <- clocations(cbind(1, res$from, res$to))
  clocs <- clocs.reduce(clocs, use.threads=F)
  res <- res[lx.in(res$to, clocs[,3]),,drop=F]
  
  res
}

# -------------------------------------------------
# <internal> collect segments statistics
#

.stat.segments <- function(seg, pos, data, lev.ref=c("lev.rrc", "lev.baf")) {
  
  #
  # get data
  #
  lev.ref <- match.arg(lev.ref)
  
  if (lev.ref == "lev.baf") data <- abs(data-0.5)+0.5 # fold af

  #
  # collect statistics
  #
  res <- lapply(seq_len(nrow(seg)), function(i) {
    row   <- seg[i,] # 'apply' sucks
    ifrom <- lx.findInterval(row$from, pos)
    ito   <- lx.findInterval(row$to, pos)
    ilen  <- ito - ifrom + 1
    seg.data <- as.vector(data[seq.int(ifrom, ito)])
    med  <- median(seg.data)
    sd   <- if (ilen > 1) sd(seg.data) else 0
    mad  <- mad(seg.data)
    if (mad == 0) mad <- sd
    if (lev.ref == "lev.baf") {
      cor <- asdog.unfold(med, sd, type="median")$solution
      med <- cor$mu
      mad <- cor$sd
    }
    ref  <- row[, lev.ref]
    zscor <- if (med == ref) 0 else (med - ref)/mad
    pval  <- 2*pnorm(-abs(zscor)) # two-sided
    list(nb.mark=ilen, med=med, mad=mad, zscor=zscor, pval=pval)
  })

  do.call(rbind.data.frame, res)
}

# -------------------------------------------------
# RRC & AF segmentation
#

asdog.segment.rcaf <- function(plmodel, regions, params,
                               use.threads=lx.use.threads()) {

  #
  # get parameters
  #
  alpha  <- params$segment.alpha
  ploidy <- params$segment.ploidy

  if (is.na(alpha))  alpha  <- plmodel$best$alpha
  if (is.na(ploidy)) ploidy <- plmodel$best$q0

  lx.out("ploidy model : alpha=", alpha, " ploidy=", ploidy)
  
  theo <- asdog.theo.RCAF(alpha, ploidy, 0:params$segment.cnmax)
  max.theo.rc <- max(theo$rc)

  lx.out("ploidy model : max.theo.rc = ", max.theo.rc)

  #
  # make HMM
  #
  lx.out("make HMM RCAF")  
  
  .c <- function(x,y=x) complex(real=x, imaginary=y)
  
  means <- .c(theo$rc, theo$af-0.5)
  sd.rc <- lx.wt.mean(plmodel$seg$sd.rc, plmodel$seg$weight)
  sd.af <- lx.wt.mean(plmodel$seg$sd.af, plmodel$seg$weight)
  sds <- .c(sd.rc, sd.af)
  
  hmm.rcaf <- thmm.init(drcaf.asdog, params$segment.tau,
                        mean=means,
                        sd=sds)
  
  #
  # compute segments by chromosomes
  #
  lx.out("computing segments by chromosome", with.mem=T)
  
  chrs <- unique(plmodel$rcaf$chr)
  rcaf <- split(plmodel$rcaf, plmodel$rcaf$chr)[as.character(chrs)]
  
  segs.rcaf <- lx.lapply(rcaf, function(dat) {
    
    ichr <- as.integer(head(dat$chr, 1))
    rrc  <- dat$rrc
    baf  <- dat$baf
    
    rrc[rrc >= max.theo.rc] <- max.theo.rc
    
    v <- thmm.viterbi(hmm.rcaf, .c(rrc, baf-0.5))
    
    seg <- .collect.segments(v$states, dat$pos, ichr,
                             lev.rrc=theo$rc, lev.baf=theo$af,
                             lev.cn=theo$ploidy,
                             geno=theo$label,
                             delta.cn=rep(0, length(theo$ploidy)))
    
    seg <- .filter.segments(seg,
                            minsize=params$segment.minsize, 
                            joinsize=params$segment.joinsize,
                            regions=regions[regions[,1]==ichr,,drop=F])
    
    # add rrc and baf statistics
    #
    stat.rrc <- .stat.segments(seg, dat$pos, dat$rrc, "lev.rrc")
    nb.mark  <- stat.rrc[,1]
    ppm.mark <- nb.mark / (seg$to - seg$from + 1) * 1e6
    stat.rrc <- stat.rrc[,-1]
    colnames(stat.rrc) <- paste0(colnames(stat.rrc), ".rrc")
    stat.rrc$med.cn <- asdog.absCN(stat.rrc$med.rrc, alpha, ploidy)
    
    stat.baf <- .stat.segments(seg, dat$pos, dat$baf, "lev.baf")[,-1]
    colnames(stat.baf) <- paste0(colnames(stat.baf), ".baf")
    
    seg <- cbind(seg, nb.mark=nb.mark, ppm.mark=ppm.mark, stat.rrc, stat.baf)
    
    seg
  }, use.threads=use.threads) ## thread bug 
  
  segs.rcaf <- do.call(rbind, segs.rcaf)
  
  #
  # add delta.cn for gains and losses
  # ref.cn is the majoritary cn level observed in segments
  # it is used to compute gains and losses
  #
  cnt <- aggregate(segs.rcaf$to - segs.rcaf$from+1,
                   by=list(cn=segs.rcaf$lev.cn),
                   FUN=sum, simplify=F)
  ref.cn <- cnt$cn[which.max(cnt$x)]
  segs.rcaf$delta.cn <- segs.rcaf$lev.cn - ref.cn

  #
  # end
  #
  
  lx.out("found ", nrow(segs.rcaf), " segments", with.mem=T)
  
  list(theo=theo, hmm=hmm.rcaf, segs=segs.rcaf, ref.cn=ref.cn)
}

# -------------------------------------------------
# RRC only segmentation
#

asdog.segment.rc <- function(gccorrect, regions, params,
                             sd.winsize=1000L,
                             use.threads=lx.use.threads()) {
  #
  # get parameters
  #
  alpha  <- params$segment.alpha
  ploidy <- params$segment.ploidy

  lx.out("ploidy model : alpha=", alpha, " ploidy=", ploidy)
  
  theo <- asdog.theo.RCAF(alpha, ploidy, 0:params$segment.cnmax)
  max.theo.rc <- max(theo$rc)
  
  lx.out("ploidy model : max.theo.rc = ", max.theo.rc)

  #
  # estimate rrc standard deviation
  #
  rrc <- unlist(sapply(gccorrect$bychr, function(x) x$rrc.cor), use.names=F)
  lx.stopif(length(rrc) == 0, "no rrc.cor data points found in gccorrect")
  
  sd.winsize <- min(sd.winsize, length(rrc))
  sd0 <- median(sapply(seq(1, length(rrc), sd.winsize), 
                       function(i) sd(rrc[i:(i+sd.winsize)], na.rm=T)),
                na.rm=T)
  lx.out("sd rrc estimate=", sd0)
  
  #
  # make HMM
  #
  lx.out("make HMM RC only")  
  
  means <- unique(theo$rc)
  ploidy.levels <- theo$ploidy[match(means, theo$rc)]
  hmm.rc <- thmm.init(dnorm, params$segment.tau, mean=means, sd=sd0)

  #
  # compute segments
  #
  lx.out("computing segments by chromosome", with.mem=T)
  
  segs.rc <- lx.lapply(names(gccorrect$bychr), function(chr) {
    
    dat  <- gccorrect$bychr[[chr]]
    ichr <- as.integer(chr)
    rrc  <- dat$rrc.cor
    
    rrc[rrc >= max.theo.rc] <- max.theo.rc
    
    v <- thmm.viterbi(hmm.rc, rrc)
    
    seg <- .collect.segments(v$states, dat$pos, ichr, 
                             lev.rrc=means, 
                             lev.cn=ploidy.levels,
                             delta.cn=rep(0, length(ploidy.levels)))
    
    seg <- .filter.segments(seg, 
                            minsize=params$segment.minsize,
                            joinsize=params$segment.joinsize,
                            regions=regions[regions[,1]==chr,,drop=F])
    
    # add rrc statistics
    #
    stat.rrc <- .stat.segments(seg, dat$pos, dat$rrc.cor, "lev.rrc")
    nb.mark  <- stat.rrc[,1]
    ppm.mark <- nb.mark / (seg$to - seg$from + 1) * 1e6
    stat.rrc <- stat.rrc[,-1]
    colnames(stat.rrc) <- paste0(colnames(stat.rrc), ".rrc")
    stat.rrc$med.cn <- asdog.absCN(stat.rrc$med.rrc, alpha, ploidy)

    seg <- cbind(seg, nb.mark=nb.mark, ppm.mark=ppm.mark, stat.rrc)
    
    seg
  }, use.threads=use.threads)
  
  segs.rc <- do.call(rbind, segs.rc)

  #
  # add delta.cn for gains and losses
  # ref.cn is the majoritary cn level observed in segments
  # it is used to compute gains and losses
  #
  cnt <- aggregate(segs.rc$to - segs.rc$from+1,
                   by=list(cn=segs.rc$lev.cn),
                   FUN=sum, simplify=F)
  ref.cn <- cnt$cn[which.max(cnt$x)]
  segs.rc$delta.cn <- segs.rc$lev.cn - ref.cn
  
  #
  # end
  #

  lx.out("found ", nrow(segs.rc), " segments", with.mem=T)
  
  list(theo=theo, hmm=hmm.rc, segs=segs.rc, ref.cn=ref.cn)
}

# -------------------------------------------------
# All segmentation - API
#
# note: plmodel maybe NULL
#
asdog.segment <- function(gccorrect, plmodel=NULL, params=gccorrect$params,
                          use.threads=lx.use.threads()) {

  #-------------------------------
  # check parameter compatibility
  #-------------------------------
  
  lx.stopif(is.null(plmodel) && (   is.na(params$segment.alpha) 
                                 || is.na(params$segment.ploidy)),
            "plmodel not provided. ",
            "you should provide segment.alpha and segment.ploidy parameters")
  
  if (is.na(params$segment.alpha))
    params$segment.alpha <- plmodel$best$alpha

  if (is.na(params$segment.ploidy))
    params$segment.ploidy <- plmodel$best$q0
  
  #
  # the correct binsize is the one actually used in gccorrect
  #
  binsize <- gccorrect$params$gccorrect.binsize
  params$segment.minsize <- max(params$segment.minsize, binsize)

  #-------------------------------
  # init results
  #-------------------------------
  
  segment <- asdog.object('Segment', params=params)
  
  asdog.print.params(asdog.filter.params(params, in.filter="segment:"))
  
  # keep sequence header
  #
  segment$header <- gccorrect$header

  #-------------------------------
  # Get user's regions
  #-------------------------------
  
  lx.out("---------------------------------------")
  lx.out("Get User's regions")
  lx.out("---------------------------------------")
  
  .rsize <- function(x) x[,3] - x[,2] + 1
  .tsize <- function(x) sum(.rsize(x))
  
  regions <- basta2clocs(segment)
  
  chrs <- regions[,1]

  lx.out("basta regions: ", length(chrs), " chromosomes")
  lx.out("yielding ", nrow(regions), 
         " regions covering ", .tsize(regions), " bp")
  
  if (length(params$chrs) == 0)
    params$chrs = chrs
  
  chrs <- intersect(chrs, params$chrs)
  
  lx.out("filtered chromosomes indexes [", chrs, "]")

  if (! is.null(plmodel)) {
    chrs <- intersect(chrs, as.integer(unique(plmodel$rcaf$chr)))
    lx.out("filtered rcaf chromosomes indexes [", chrs, "]")
  }

  lx.stopif(length(chrs) == 0, "empty list of chromosomes")

  regions <- regions[chrs,,drop=F]

  lx.out("yielding ", nrow(regions), 
         " regions covering ", .tsize(regions), " bp")

  #-------------------------------
  # intersect with gccorrect regions
  #-------------------------------
  
  lx.out("intersecting with gccorrect regions")
  
  regions <- clocs.inter(regions, gccorrect$regions, minsize=params$segment.minsize)
  
  lx.out("yielding ", nrow(regions), 
         " regions covering ", .tsize(regions), " bp")

  # keep chrs in params
  
  chrs <- unique(regions[,1])
  params$chrs <- chrs
  
  lx.stopif(length(chrs) == 0, "empty list of chromosomes")

  #-------------------------------
  # join regions
  #-------------------------------

  lx.out("joining regions with joinsize= ", params$segment.joinsize, 
         " and minsize=", params$segment.minsize)
  
  regions <- clocs.join(regions, delta=params$segment.joinsize,
                        minsize=params$segment.minsize)

  lx.out("yielding ", nrow(regions), 
         " regions covering ", .tsize(regions), " bp")

  #-------------------------------
  # Segmentation on RCAF (using plmodel normal heterozygous sites)
  #-------------------------------

  if ((! is.null(plmodel)) && params$segment.use.rcaf) {
    lx.out("---------------------------------------")
    lx.out("Segmentation on RC-AF")
    lx.out("---------------------------------------")
    res.rcaf <- asdog.segment.rcaf(plmodel, regions, params,
                                   use.threads=use.threads)
    segs.rcaf <- res.rcaf$segs
  } else {
    res.rcaf  <- NULL
    segs.rcaf <- NULL
  }
  
  #-------------------------------
  # Segmentation on RC Only (using gccorrect data)
  #-------------------------------
  
  lx.out("---------------------------------------")
  lx.out("Segmentation on RC only")
  lx.out("---------------------------------------")
  
  res.rrc <- asdog.segment.rc(gccorrect, regions, params,
                             use.threads=use.threads)
  segs.rrc <- res.rrc$segs
  

  #-------------------------------
  # assign putative genotype(s) to rc segments
  # using overlap with rcaf segments
  #-------------------------------
  
  if (! is.null(segs.rcaf)) {
    clocs.rcaf <- clocations(segs.rcaf[,1:3])
    clocs.rc   <- clocations(segs.rrc[,1:3])
    over <- clocs.overlap(clocs.rc, clocs.rcaf)
    segs.rrc$geno <- sapply(over, function(lst)
                            paste(unique(segs.rcaf[lst, "geno"]), collapse="/"))
  }
  
  #-------------------------------
  # keep some information in Segment object
  #-------------------------------
  #
  chrs.char <- as.character(params$chrs)
  segment$regions <- regions
  segment$rcaf.data <- if (is.null(plmodel)) NULL 
                       else split(plmodel$rcaf, plmodel$rcaf$chr)[chrs.char]
  segment$rrc.data  <- sapply(chrs.char, function(chr) {
                            x <- gccorrect$bychr[[chr]]
                            data.frame(chr=chr, 
                                       pos=x$pos,
                                       rrc=x$rrc.cor,
                                       stringsAsFactors=F)
                        }, simplify=F)
  segment$rcaf.theo  <- res.rcaf$theo
  segment$rrc.theo   <- res.rrc$theo
  segment$rcaf.hmm   <- thmm.forget.env(res.rcaf$hmm)
  segment$rrc.hmm    <- thmm.forget.env(res.rrc$hmm)
  segment$rcaf.segs  <- segs.rcaf
  segment$rrc.segs   <- segs.rrc
  segment$rcaf.refcn <- res.rcaf$ref.cn
  segment$rrc.refcn  <- res.rrc$ref.cn
  
  #
  # end
  #
  
  lx.out("---------------------------------------", with.mem=T)
  lx.out("End")
  lx.out("---------------------------------------")
  
  lx.info(segment)            <- 'Segmentation results'
  lx.info(segment$params)     <- 'computation parameters'
  lx.info(segment$header)     <- 'chromosomes information'
  lx.info(segment$regions)    <- 'regions for selecting SNPs'
  lx.info(segment$rcaf.data)  <- 'RRC-AF data per chromosome'
  lx.info(segment$rrc.data)   <- 'RRC only data per chromosome'
  lx.info(segment$rcaf.theo)  <- 'RRC-AF ploidy model theo points'
  lx.info(segment$rrc.theo)   <- 'RRC only ploidy model theo points'
  lx.info(segment$rcaf.hmm)   <- 'RRC-AF hmm'
  lx.info(segment$rrc.hmm)    <- 'RRC only hmm'
  lx.info(segment$rcaf.segs)  <- 'RRC-AF segments'
  lx.info(segment$rrc.segs)   <- 'RRC only segments'
  lx.info(segment$rcaf.refcn) <- 'RRC-AF reference CN'
  lx.info(segment$rrc.refcn)  <- 'RRC only reference CN'
  
  segment
}

# -------------------------------------------------
# some S3 helper
#

# print overload

print.Segment <- function(segment) lx.doc(segment, .name='Segment')

##
## [AV] dev test
##

if (FALSE) {
  
  require(asdog)
  
  # !
  # dans rcaf la position est celle des SNP heterozygotes du normal
  # dans gccor c'est celle du centre de la fenetre de taille binsize
  # !
  
  gccorrect  <- readRDS("/TmpStorage/GYNCS.PA16014.PR16020.WGS.gccorrect.rds")  
  plmodel <- readRDS("/TmpStorage/GYNCS.PA16014.PR16020.WGS.plmodel.rds")  
  
  gccorrect  <- readRDS("/TmpStorage/HER2.P01952.TU110C1.WGS.gccorrect.rds")  
  plmodel <- readRDS("/TmpStorage/HER2.P01952.TU110C1.WGS.plmodel.rds")  

  gccorrect  <- readRDS("/TmpStorage/GYNCS.PA140A7.PR140A7.WGS.gccorrect.rds")  
  plmodel <- readRDS("/TmpStorage/GYNCS.PA140A7.PR140A7.WGS.plmodel.rds")  
  
  params = asdog.new.params()
  segment <- asdog.segment(gccorrect, plmodel, params, use.threads=T)

  segment <- readRDS("/TmpStorage/HER2.P01952.TU110C1.WGS.segment.rds")
  
  plot(segment)
}
