#!/usr/bin/env Rscript
# $Id: asdog.matchpair.r 396 2019-01-02 22:53:10Z viari $
#
# Match normal/tumour pair 
# based on best homozygous sites most likely heterozygous
# in HapMap ref population
#

suppressMessages(library(asdog))

prog.usage <- function() {
  .cat <- function(...) cat(..., '\n', file=stderr())
  .cat("usage: asdog.matchpair.r --base=<base> --match=<match> [--ref.snp=<snp>] [--<option>=<value>]")
  .cat("       <base>  := path to normal baf file (without extension)")
  .cat("       <match> := path to tumor baf file (without extension)")
  .cat("       <snp>   := path to reference Hamap SNP database (without extension)")
  .cat("                  can be either a .rds or .txt file")
}

#
# get arguments
#

args <- asdog.main.args("matchpair:")

if ((! is.character(args$base)) || (! is.character(args$match))) {
  asdog.main.exit(1, usage=TRUE)
}

#
# get params
#

params <- asdog.main.params(args)

#
# check baf files
#

nor.path <- asdog.check.file(params$base, "baf")
tum.path <- asdog.check.file(params$match, "baf")

#
# check SNP freq database
#

# note : params$ref.snp has its own default to : sys:asdog:data/HapMap.hg19.snp.100K.rds

ref.path <- asdog.check.file(params$ref.snp, c("txt", "rds"), .bgz=NULL)

#
# print parameters for posterity
#

asdog.print.params(asdog.filter.params(params, "matchpair:"))

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

# @fixme : chrindex to chrname
nor.hdl <- baf.open(nor.path)
chrs <- baf.index2name(nor.hdl, params$chrs)
baf.close(nor.hdl)

snp <- asdog.snp.filter(snp, chrs=chrs,
                        pop.low=params$snp.pop.low, 
                        nsample=params$snp.sample.size)

#
# load baf files
#

lx.out("---------------------------------------", with.mem=T)
lx.out(" loading baf")
lx.out("---------------------------------------")

nor.hdl <- baf.open(nor.path)
tum.hdl <- baf.open(tum.path)

#
# match pairs
#

lx.out("---------------------------------------", with.mem=T)
lx.out(" matching samples")
lx.out("---------------------------------------")

res <- asdog.match.samples(snp, nor.hdl, tum.hdl,
                           mincov=params$snp.mincov, 
                           lowread=params$snp.lowread,
                           nbest=params$snp.match.nbest,
                           refpop=params$snp.match.refpop)

lx.out("")

cat("NbSites:", length(res$nor.gen), length(res$tum.gen), "\n")
cat("Overlap:", sum(res$same.gen), length(res$same.gen), "\n")
cat("Similarity:", res$simil, "\n")

lx.out("")

#
# end
#
lx.out("---------------------------------------", with.mem=T)
lx.out(" End asdog.popmap")
lx.out("---------------------------------------")


asdog.main.exit(0)
