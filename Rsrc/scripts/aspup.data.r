#!/usr/bin/env Rscript
#
# convert BAF to aspup.data
#

library(asdog)

lx.options(use.threads=TRUE)

prog.usage <- function() {
  .cat <- function(...) cat(..., '\n', file=stderr())
  .cat("usage: aspup.data.r --baf=<baf> --ref=<reference> --bed=<bed> [--outdir=<dir>]")
  .cat("       <base>   := path to baf file")
  .cat("       <ref>    := path to ref genome basta file")
  .cat("       <bed>    := path to probes bed file")
  .cat("       <dir>    := output directory (dft='.')")
}

args <- asdog.main.args(NULL)

# --- dev tests
#args$baf = "/TmpStorage/P_01-2162_refined_alignement.recalibrated"
#args$ref = "/TmpStorage/references/hg19/HS_GRCh37_72"
#args$bed = "/TmpStorage/backbone_Profiler02.bed"
#args$outdir = "."
# ---

if (   (! is.character(args$baf)) 
    || (! is.character(args$ref))
    || (! is.character(args$bed))) {
  prog.usage()
  asdog.main.exit(1)
}

base <- basename(args$baf)

baf <- baf.open(asdog.check.file(args$baf, "baf"))

bst <- basta.open(asdog.check.file(args$ref, "bst"))

bed <- asdog.check.file(args$bed, "bed", .bgz=NULL)

outdir <- if (is.character(args$outdir)) args$outdir else "."

#

MARGIN = 100

data <- list(params=c(args, margin=MARGIN), header=baf$header)

lx.out("reading bed file")

data$beds <- clocs.sort(bed2clocs(bst, bed.read(bed)))

lx.out("reading baf")

data$baf <- apply.cloc(data$beds, function(cloc, h)
              baf.fetch.cloc(h, cloc), handle=baf,
              use.threads=lx.use.threads())

data$cov <- sapply(data$baf, function(x) mean(rowSums(x)))

lx.out("reading basta")

data$seq <- apply.cloc(data$beds, function(cloc, h)
              basta.fetch.cloc(h, cloc), handle=bst,
              use.threads=lx.use.threads())

data$gc <- sapply(data$seq, function(x) length(lx.strchr(x, "G|C"))/nchar(x))

seqbeds5 <- clocations(cbind(data$beds[,1], pmax(1, data$beds[,2]-MARGIN), pmax(1, data$beds[,2]-1)))
seqbeds3 <- clocations(cbind(data$beds[,1], data$beds[,3]+1, data$beds[,3]+MARGIN))

data$seq5 <- apply.cloc(seqbeds5, function(cloc, h)
              basta.fetch.cloc(h, cloc), handle=bst,
              use.threads=lx.use.threads())

data$seq3 <- apply.cloc(seqbeds3, function(cloc, h)
              basta.fetch.cloc(h, cloc), handle=bst,
              use.threads=lx.use.threads())

data$gc <- sapply(data$seq, function(x) length(lx.strchr(x, "G|C"))/nchar(x))

#

out <- paste0(outdir, "/", base, ".data.rds")
lx.out("saving data as ", out)

saveRDS(data, out)

invisible(baf.close(baf))
invisible(basta.close(bst))

asdog.main.exit(0)







