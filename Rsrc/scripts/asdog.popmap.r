#!/usr/bin/env Rscript
# $Id: asdog.popmap.r 396 2019-01-02 22:53:10Z viari $
#
# Compute population origin probability from HapMap SNPs
#

suppressMessages(library(asdog))

prog.usage <- function() {
  .cat <- function(...) cat(..., '\n', file=stderr())
  .cat("usage: asdog.popmap.r --base=<base> [--ref.snp=<snp>] [--<option>=<value>]")
  .cat("       <base> := path to baf file (without extension)")
  .cat("       <snp>  := path to reference Hamap SNP database (without extension)")
  .cat("                 can be either a .rds or .txt file")
}

#
# get arguments
#

args <- asdog.main.args("popmap:")

if (! is.character(args$base)) {
  asdog.main.exit(1, usage=TRUE)
}

#
# get params
#

params <- asdog.main.params(args)

#
# check baf file
#

baf.path <- asdog.check.file(params$base, "baf")

#
# check SNP freq database
#

# note : params$ref.snp has its own default to : sys:asdog:data/HapMap.hg19.snp.100K.rds

ref.path <- asdog.check.file(params$ref.snp, c("txt", "rds"), .bgz=NULL)

#
# print parameters for posterity
#

asdog.print.params(asdog.filter.params(params, "popmap:"))

#
# load baf
#

lx.out("---------------------------------------", with.mem=T)
lx.out(" loading baf")
lx.out("---------------------------------------")

baf.hdl <- baf.open(baf.path)

#
# load SNP 
#

lx.out("---------------------------------------", with.mem=T)
lx.out(" loading SNPs")
lx.out("---------------------------------------")

snp <- asdog.snp.load(ref.path)

#
# filter SNP
#

lx.out("---------------------------------------", with.mem=T)
lx.out(" filtering SNPs")
lx.out("---------------------------------------")

chrs <- baf.index2name(baf.hdl, params$chrs)
snp <- asdog.snp.filter(snp, chrs=chrs,
                        pop.low=params$snp.pop.low, 
                        nsample=params$snp.sample.size)

#
# get baf of snps
#

lx.out("---------------------------------------", with.mem=T)
lx.out(" retrieving baf of SNPs")
lx.out("---------------------------------------")

baf.cnt <- asdog.snp.baf.count(snp, baf.hdl)

#
# filter baf
#
lx.out("---------------------------------------", with.mem=T)
lx.out(" filtering baf")
lx.out("---------------------------------------")

baf.ok <- baf.count.filter(baf.cnt, lowread=params$snp.lowread,
                           mincov=params$snp.mincov,
                           minall=1, maxall=2,
                           deltafreq=params$snp.deltafreq,
                           what="index")

baf.cnt <- baf.cnt[baf.ok,]
snp <- snp[baf.ok,]

#
# compute population probas
#

lx.out("---------------------------------------", with.mem=T)
lx.out(" computing population probas")
lx.out("---------------------------------------")

baf.geno <- baf.count.genotype(baf.cnt, lowread=params$snp.lowread)
pop.prob <- asdog.population.proba(snp, baf.geno, pop=snp[7:ncol(snp)])

lx.out("")

cat("Loglikelihoods:", lx.list2str(pop.prob$llk), "\n")
cat("Bayes_probas:", lx.list2str(pop.prob$bayes), "\n")

imax <- which.max(pop.prob$bayes)
cat("Decision:", names(imax), pop.prob$bayes[imax], "\n")

lx.out("")

#
# end
#
lx.out("---------------------------------------", with.mem=T)
lx.out(" End asdog.popmap")
lx.out("---------------------------------------")


asdog.main.exit(0)
