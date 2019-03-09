# -------------------------------------------------
# $Id: asdog.snpbaf.util.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# SNPs and BAF utilities
#

# -------------------------------------------------
# load hapamp snip db (reformatted)
# format : CHROM POS ID REF ALT AF [POP_AF]+
#
asdog.snp.load <- function(pathname) {
  
  lx.out("loading SNPs frequencies from: ", pathname)

  if (lx.file.ext(pathname) == "rds") {  # rds file
    snp <- readRDS(pathname)
  } 
  else if (lx.require("data.table", install=F, load=T, silent=T)) { # text data with fread
    if (lx.file.ext(pathname) == "gz")
      pathname <- paste0("gunzip -c ", pathname)
    snp <- fread(pathname, colClasses=c(CHROM="character"))
    snp <- as.data.frame(snp)
  }
  else {                                 # text data with read.table (slower)
    snp <- read.table(pathname, header=T, colClasses=c(CHROM="character"))
  }
  
  lx.out("loaded ", nrow(snp), " SNPs")
  snp
}

# -------------------------------------------------
# filter and optionaly sample snp
#
asdog.snp.filter <- function(snp, chrs=as.character(1:22),
                             pop.low=0.01,
                             nsample=1e5,
                             use.threads=lx.use.threads()) {
  lx.out("filtering ", nrow(snp), " SNPs")
  
  # keep only required chromosomes
  if (! is.null(chrs))
    snp <- snp[snp$CHROM %in% chrs,]
  
  # filter low frequencies (that may bias probability)
  if (! is.na(pop.low)) {
    af <- as.matrix(snp[,7:ncol(snp)])
    af.max <- lx.rowMaxs(af, "first")
    af.min <- lx.rowMins(af, "first")
    ok <- (af.min >= pop.low) & (af.max <= (1-pop.low))
    snp <- snp[ok,]
  }

  lx.out("filtered ", nrow(snp), " SNPs")    
  
  # sample
  if ((nsample > 0) && (nsample < nrow(snp))) {
    indx <- sort(sample(nrow(snp), nsample, replace=FALSE))
    snp <- snp[indx,]
    lx.out("sampled ", nrow(snp), " SNPs")    
  }

  snp
}

# -------------------------------------------------
# collect baf of SNPs
# snp is a snp dataframe (returned by asdog.snp.load/asdog.snp.filter)
# handle = file handle to a BAF file
#
asdog.snp.baf.count <- function(snp, handle, use.threads=lx.use.threads()) {
  clocs <- mapply(function(chr, pos) c(baf.name2index(handle, chr), pos, pos),
                  snp$CHROM, snp$POS, SIMPLIFY=F)
  byloc <- lx.happly(clocs, function(cloc, handle) baf.fetch.cloc(handle, cloc),
                     handle=handle, use.threads=use.threads)
  do.call(rbind, byloc)
}

# -------------------------------------------------
# compute probas of genotypes
# for mono or biallelic sites
# snp is a snp dataframe
# geno is an vector of genotypes (e.g from baf.count.genotype) at same positions
# freq is a population alt. allele frequency vector at same postions
# (i.e. typically snp[,"AF"], snp[,"EUR"], ...)
#
asdog.genotype.proba <- function(snp, geno, freq, use.threads=lx.use.threads()) {

  prob <- lx.mapply(function(gen, ref, alt, frq) {
    # if ((length(gen)==1) && (nchar(gen)>1)) gen <- lx.strsplit(gen, "")
    p <- sapply(gen, function(x) if (x==alt) frq else if (x==ref) (1-frq) else NA)
    lp <- length(p)
    if (lp == 1) p*p else if (lp == 2) 2*prod(p) else NA
  }, geno, snp$REF, snp$ALT, freq, USE.NAMES=F, use.threads=use.threads)
  
  names(prob) <- sapply(geno, paste, collapse="")
  
  prob
}

# -------------------------------------------------
# compute bayes probabilities of populations
# snp is a snp dataframe (returned by asdog.snp.load/asdog.snp.filter)
# geno is an vector of genotypes (e.g from baf.count.genotype) at same positions
# pop is a dataframe of population alt. allele frequencies at same positions
# (i.e. typically snp[7:ncol(snp)])
#
asdog.population.proba <- function(snp, geno, pop=snp[7:ncol(snp)],
                                   .eps=1e-10, use.threads=lx.use.threads()) {

  
  cols <- colnames(pop)
  llk <- sapply(cols, function(col) {
    p <- asdog.genotype.proba(snp, geno, pop[,col])
    sum(log(p+.eps), na.rm=T)
  })

  # bayes formula with logs  
  # pi/sum(pj) = 1/(sum(pj)/pi) = 1/sum(pj/pi) = 1/sum(exp(lj)/exp(li)) = 1/sum(exp(lj-li))
  
  bayes <- 1 / sapply(llk, function(li) sum(sapply(llk, function(lj) exp(lj-li))))

  list(llk=llk, bayes=bayes)  
}

# -------------------------------------------------
# match normal/tumor pair
# snp is a snp dataframe (returned by asdog.snp.load/asdog.snp.filter)
# nor.handle is file handle to the normal BAF file
# tum.handle is file handle to the tumor BAF file
#
asdog.match.samples <- function(snp, nor.handle, tum.handle,
                                mincov=10, lowread=2,
                                nbest=1000, refpop="AF",
                                use.threads=lx.use.threads()) {
  
  lx.out("retrieving baf at SNPs sites in normal")
  nor.cnt <- asdog.snp.baf.count(snp, nor.handle, use.threads=use.threads)

  # get homozygous sites

  lx.out("filtering homozygous sites")
  nor.homo <- baf.count.filter(nor.cnt, lowread=lowread, mincov=mincov,
                               minall=1, maxall=1, deltafreq=NA,
                               what="index")

  nor.cnt <- nor.cnt[nor.homo,]
  snp <- snp[nor.homo,]
  
  # select nbest homozygous sites most likely heterozygous in ref population
  # i.e. most segregating sites in tumour

  lx.out("filtering ", nbest, " most segregating sites")
  nor.gen <- baf.count.genotype(nor.cnt, lowread=lowread, use.threads=use.threads)
  nor.pro <- asdog.genotype.proba(snp, nor.gen, snp[,refpop], use.threads=use.threads)
  nor.ord <- head(order(nor.pro, decreasing=F, na.last=NA), nbest)

  nor.cnt <- nor.cnt[nor.ord,]
  snp <- snp[nor.ord,]

  # get corresponding sites in tumor
  
  lx.out("retrieving baf at SNPs sites in tumor")
  tum.cnt <- asdog.snp.baf.count(snp, tum.handle, use.threads=use.threads)

  # compute genotype overlap
  
  lx.out("computing genotype overlap at SNPs sites")
  nor.gen <- baf.count.genotype(nor.cnt, lowread=lowread, what="string", 
                                use.threads=use.threads)
  tum.gen <- baf.count.genotype(tum.cnt, lowread=lowread, what="string",
                                use.threads=use.threads)
  tum.val <- baf.count.filter(tum.cnt, lowread=lowread, mincov=mincov,
                              minall=1, maxall=1, deltafreq=NA,
                              what="logical")

  com <- mapply(function(ok, nor, tum) ok && (identical(nor, tum)),
                tum.val, nor.gen, tum.gen)
  
  list(nor.baf=nor.cnt, tum.baf=tum.cnt, nor.gen=nor.gen, tum.gen=tum.gen,
       snp=snp, same.gen=com, simil=sum(com)/length(com))
}

