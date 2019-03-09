#!/usr/bin/env Rscript
# $Id: asdog.gccorrect.r 396 2019-01-02 22:53:10Z viari $
#
# gc correction of read counts
#

suppressMessages(library(asdog))

prog.usage <- function() {
  .cat <- function(...) cat(..., '\n', file=stderr())
  .cat("usage: asdog.gccorrect.r --gcmodel=<gcmodel> [--outdir=<dir>] [--<option>=<value>]")
  .cat("       <gcmodel> := path to GC model (.rds) file")
  .cat("       <dir>     := output directory (dft=.)")
}

#
# get arguments
#

args <- asdog.main.args("gccorrect:")

#args$gcmodel = "HER2.P02987.TU1107C.WGS.gcmodel.rds"

if (! is.character(args$gcmodel)) {
    asdog.main.exit(1, usage=TRUE)
}

#
# load GC model
#

modfile <- asdog.check.file(args$gcmodel, "rds", .bgz=NULL)

args$gcmodel <- NULL

lx.out("---------------------------------------", with.mem=T)
lx.out(" loading model: ", modfile)
lx.out("---------------------------------------")

gcmodel <- readRDS(file=modfile)

#
# get parameters
#

params <- asdog.main.params(args, gcmodel$params)

# next line is for development (when adding new parameters in asdog.params.r)
# you may comment it in production phase
params <- lx.mixin(asdog.new.params(), params)

#
# create output directory if needed
#

dir.create(params$outdir, showWarnings=FALSE)

#
# run RC prediction
#

gccorrect <- asdog.gccorrect(gcmodel, params)

#
# outputs names
#

out.short <- asdog.clean.path.suffix(modfile, c("rds", "gcmodel"))
out.full  <- paste0(params$outdir, "/", basename(out.short), ".gccorrect")
out.base  <- basename(out.full)

#
# save prediction
#

fname <- paste0(out.full, '.rds')

lx.out("---------------------------------------", with.mem=T)
lx.out(" saving prediction to: ", fname)
lx.out("---------------------------------------")

saveRDS(gccorrect, file=fname)

#
# making table exports
#

fname <- out.full
ffname <- paste0(fname, ".txt.gz")

lx.out("---------------------------------------", with.mem=T)
lx.out(" making table export : ", ffname)
lx.out("---------------------------------------")

asdog.write.table(gccorrect, file=gzfile(ffname))

#
# print pdf report
#

setwd(params$outdir)
fname <- out.base

lx.out("---------------------------------------", with.mem=T)
lx.out(" making pdf report: ", paste0(fname, ".pdf"))
lx.out("---------------------------------------")

asdog.pdf.report(gccorrect, fname)

#
# end
#
lx.out("---------------------------------------", with.mem=T)
lx.out(" End asdog.gccorrect")
lx.out("---------------------------------------")

asdog.main.exit(0)
