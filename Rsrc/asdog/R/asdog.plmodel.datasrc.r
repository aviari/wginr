# -------------------------------------------------
# $Id: asdog.plmodel.datasrc.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# Ploidy model : get RC & AF data from BAF files
#

# -------------------------------------------------
# collect all necessary data from:
# nor.hdle : handle to normal BAF
# tum.hdle : handle to tumour BAF
# gccorrect    : result of asdog.gccorrect (GCcorrected RC)
# chrs: chromosomes
# lowread: lower read threshold
# mincov: min cover
# deltafreq: min delta AF around 0.5 for heterozygous sites
#
asdog.rcaf.datasrc <- function(nor.hdle, tum.hdle, gccorrect, chrs,
                               lowread=2, mincov=10, deltafreq=0.1,
                               # min.match=0,
                               .chunk.size=1e6L,
                               use.threads=lx.use.threads()) {
  
  .tsize <- function(x, fun=length) fun(unlist(x))
  
  #
  # get heterozygous sites in normal
  #
  lx.out("get heterozygous sites in normal", with.mem=T)
  nor.baf.cnt <- baf.heterozygous(nor.hdle, chrs=chrs,
                                  lowread=lowread, mincov=mincov,
                                  deltafreq=deltafreq,
                                  flatten=F, .chunk.size=.chunk.size,
                                  use.threads=use.threads)
  
  hetero.rpos <- lx.lapply(nor.baf.cnt, function(h) as.integer(rownames(h)),
                           use.threads=use.threads)
  lx.out("found ", .tsize(hetero.rpos), " heterozygous sites")
  
  #
  # get tumoral BAF at heterozygous sites
  #
  lx.out("get tumoral BAF at heterozygous sites", with.mem=T)
  
  tum.baf.cnt <- lx.happly(chrs, function(chr, handle) {
    relpts <- hetero.rpos[[as.character(chr)]]
    baf.fetch.points.chr(handle, chr, relpts)
  }, handle=tum.hdle, use.threads=use.threads)
  
  names(tum.baf.cnt) <- chrs
  
  #
  # check matching genotypes
  # note: this is only a check. we do not remove sites
  #       that do not match. otherwise this will remove LOH
  #       in pure tumours.
  #
  lx.out("checking matching genotypes", with.mem=T)
  
  ok <- lx.lapply(as.character(chrs), function(chr) {
    nor.geno <- baf.count.genotype(nor.baf.cnt[[chr]], lowread=lowread)
    tum.geno <- baf.count.genotype(tum.baf.cnt[[chr]], lowread=lowread)
    mapply(function(x, y) all((length(y)==0) || (y %in% x)), nor.geno, tum.geno)
  }, use.threads=use.threads)
  
  names(ok) <- chrs
  
  frac.match <- .tsize(ok, sum)/.tsize(ok)
  
  lx.out("found ", .tsize(ok, sum), " matched genotypes (",
         format(frac.match*100, digits=3), "%)")

  # no: this will remove LOH in pure tumours
  #
  # .take <- function(ok, x) if (length(ok) > 0) x[ok,] else x
  # nor.baf.cnt <- mapply(.take, ok, nor.baf.cnt, SIMPLIFY=F)
  # tum.baf.cnt <- mapply(.take, ok, tum.baf.cnt, SIMPLIFY=F)
  # 
  # .take <- function(ok, x) if (length(ok) > 0) x[ok] else x
  # hetero.rpos <- mapply(.take,  ok, hetero.rpos, SIMPLIFY=F)
  #

  # test to be added in future
  #
  # lx.stopif(frac.match < min.match,
  #           "Too few matching genotypes. Are you sure this is a matched pair ?",
  #           "If yes please decrease the 'plmodel.min.match' parameter to 0")
  
  #
  # collect allele counts
  #
  lx.out("collect allele counts", with.mem=T)
  res <- lx.lapply(as.character(chrs), function(chr) {
    x   <- nor.baf.cnt[[chr]]
    y   <- tum.baf.cnt[[chr]]
    # first lexicographic allele
    i   <- sapply(baf.count.genotype(x, lowread=lowread, what="index"), function(x) x[1])
    
    if (length(i) == 0)
      return(list(pos=integer(0), all=character(0), sym=character(0), baf=numeric(0)))
    
    all.cnt <- y[cbind(seq_along(i), i)]
    names(all.cnt) <- colnames(x)[i]
    all.tot <- rowSums(y)
    all.sym <- baf.count.genotype(x, lowread=lowread, what="string", sorted=T)
    
    list(pos=as.integer(rownames(x)), all.sym=all.sym, all.cnt=all.cnt,
         all.tot=all.tot)
  }, use.threads=use.threads)
  
  names(res) <- chrs
  
  #
  # add tumoral RCC from RCPREDICT
  #
  lx.out("get tumoral rrc from gccorrect", with.mem=T)
  binsize <- gccorrect$params$gccorrect.binsize
binsize=1e6 # debug
  res <- lx.lapply(as.character(chrs), function(chr) {
    rc <- gccorrect$bychr[[chr]]
    af <- res[[chr]]
    i <- lx.findInterval(af$pos, rc$pos)
    ok <- abs(af$pos - rc$pos[i]) <= binsize
    res <- lapply(af, function(x) x[ok])
    res$rrc <- rc$rrc.cor[i[ok]]
    res
  }, use.threads=use.threads)
  
  names(res) <- chrs
  
  #
  # reformat everything as a dataframe
  #
  lx.out("reformating baf data", with.mem=T)
  res <- lx.lapply(as.character(chrs), function(chr) {
    x <- res[[chr]]
    data.frame(chr=chr, pos=x$pos, all=x$all.sym, cnt=x$all.cnt,
               tot=x$all.tot, rrc=x$rrc,
               stringsAsFactors=F)
  }, use.threads=use.threads)
  
  res <- do.call(rbind, res)
  
  #
  # regularized baf
  #
  lx.out("regularize baf", with.mem=T)
  res$baf <- res$cnt / pmax(res$tot, 1)
  res$raf <- baf.count.regularize.gaussian(res$cnt, res$tot, sd=1, .seed=0)$raf
  
  lx.out("RC-AF data : ", nrow(res), " normal heterozygous sites")
  
  res
}

