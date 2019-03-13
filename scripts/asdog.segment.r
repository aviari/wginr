#!/usr/bin/env Rscript
# $Id: asdog.segment.r 396 2019-01-02 22:53:10Z viari $
#
# segmentation 
#

suppressMessages(library(asdog))

prog.usage <- function() {
  .cat <- function(...) cat(..., '\n', file=stderr())
  .cat("usage: asdog.segment.r --gccorrect=<gccorrect> [--plmodel=<plmodel>] [--outdir=<dir>] [--<option>=<value>]")
  .cat("       <gccorrect> := path to gccorrect (.rds) file")
  .cat("       <plmodel>   := path to ploidy model (.rds) file")
  .cat("       <dir>       := output directory (dft=.)")
}

#
# get arguments
#

args <- asdog.main.args("segment:")

# --- dev tests
#args$gccorrect = "tumor.gccorrect.rds"
#args$plmodel = "tumor.plmodel.rds"
# ---

if (! is.character(args$gccorrect)) {
    asdog.main.exit(1, usage=TRUE)
}

#
# load GCCorrect
#

corfile <- asdog.check.file(args$gccorrect, "rds", .bgz=NULL)

args$gccorrect <- NULL

lx.out("---------------------------------------", with.mem=T)
lx.out(" loading GCCorrect: ", corfile)
lx.out("---------------------------------------")

gccorrect <- readRDS(file=corfile)

#
# load PLModel
#

if (is.null(args$plmodel)) {
  lx.out("---------------------------------------", with.mem=T)
  lx.out(" No PLModel used => no RCAF segmentation (RRC only)")
  lx.out("---------------------------------------")
  plmodel <- NULL
} else {
  modfile <- asdog.check.file(args$plmodel, "rds", .bgz=NULL)
  
  args$plmodel <- NULL
  
  lx.out("---------------------------------------", with.mem=T)
  lx.out(" loading PLModel: ", modfile)
  lx.out("---------------------------------------")
  
  plmodel <- readRDS(file=modfile)
}

#
# get parameters
#

params <- asdog.main.params(args, gccorrect$params)

# next line is for development (when adding new parameters in asdog.params.r)
# you may comment it in production phase
params <- lx.mixin(asdog.new.params(), params)

#
# create output directory if needed
#

dir.create(params$outdir, showWarnings=FALSE)

#
# run segmentation
#

segment <- asdog.segment(gccorrect, plmodel=plmodel, params=params)

#
# outputs names
#

out.short <- asdog.clean.path.suffix(corfile, c("rds", "gccorrect"))
out.full  <- paste0(params$outdir, "/", basename(out.short), ".segment")
out.base  <- basename(out.full)

#
# save segmentation
#

fname <- paste0(out.full, '.rds')

lx.out("---------------------------------------", with.mem=T)
lx.out(" saving segments to: ", fname)
lx.out("---------------------------------------")

saveRDS(segment, file=fname)

#
# making table exports
#

fname <- out.full

if (! is.null(segment$rcaf.segs)) {
  ffname <- paste0(fname, ".rcaf.txt.gz")
  lx.out("---------------------------------------", with.mem=T)
  lx.out(" making table export : ", ffname)
  lx.out("---------------------------------------")
  asdog.write.table(segment, file=gzfile(ffname), what="rcaf")
}

if (! is.null(segment$rrc.segs)) {
  ffname <- paste0(fname, ".rrc.txt.gz")
  lx.out("---------------------------------------", with.mem=T)
  lx.out(" making table export : ", ffname)
  lx.out("---------------------------------------")
  asdog.write.table(segment, file=gzfile(ffname), what="rrc")
}

#
# print pdf report
#

setwd(params$outdir)
fname <- out.base

lx.out("---------------------------------------", with.mem=T)
lx.out(" making pdf report: ", paste0(fname, ".pdf"))
lx.out("---------------------------------------")

tmp <- asdog.pdf.report(segment, fname)

#
# end
#
lx.out("---------------------------------------", with.mem=T)
lx.out(" End asdog.segment")
lx.out("---------------------------------------")

asdog.main.exit(0)
