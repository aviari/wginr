#!/usr/bin/env Rscript
# $Id: asdog.gcmodel.r 343 2017-11-02 21:44:22Z viari $
#
# GC correction - make GC model - new version with gloess model
#

suppressMessages(library(asdog))

prog.usage <- function() {
  .cat <- function(...) cat(..., '\n', file=stderr())
  .cat("usage: asdog.gcmodel.r --base=<base> --ref=<ref> [--outdir=<dir>] [--<option>=<value>]")
  .cat("       <base>   := path to baf file (without extension)")
  .cat("       <ref>    := path to ref genome basta file (without extension)")
  .cat("       <dir>    := output directory (dft='.')")
}

#
# get arguments
#

args <- asdog.main.args("gccorrect:")

#args$base = "/Users/viari/Desktop/GCCorrect/HER2.P02987.TU1107C.WGS"
#args$base = "/Users/viari/Desktop/GCCorrect/chr16/chr16_lg"
#args$ref = "/Users/viari/Desktop/LyonCancer/data/HS_GRCh37_72"

if ((! is.character(args$base)) || (! is.character(args$ref))) {
  asdog.main.exit(1, usage=TRUE)
}

#
# get parameters
#

params <- asdog.main.params(args)

#
# create output directory if needed
#

dir.create(params$outdir, showWarnings=FALSE)

#
# run GC model
#

gcmodel <- asdog.gcmodel(params)

#
# outputs names
#

args.base <- asdog.clean.path.suffix(args$base, c("baf", "bgz"))

out.full <- paste0(params$outdir, "/", basename(args.base), ".gcmodel")
out.base <- basename(out.full)

#
# save model
#

fname <- paste0(out.full, ".rds")

lx.out("---------------------------------------", with.mem=T)
lx.out(" saving model to: ", fname)
lx.out("---------------------------------------")

saveRDS(gcmodel, file=fname)

#
# print pdf report
#

setwd(params$outdir)
fname <- out.base

lx.out("---------------------------------------", with.mem=T)
lx.out(" making pdf report: ", paste0(fname, ".pdf"))
lx.out("---------------------------------------")

asdog.pdf.report(gcmodel, fname)

#
# end
#
lx.out("---------------------------------------", with.mem=T)
lx.out(" End asdog.gcmodel")
lx.out("---------------------------------------")


asdog.main.exit(0)
