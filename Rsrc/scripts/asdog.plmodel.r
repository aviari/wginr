#!/usr/bin/env Rscript
# $Id: asdog.plmodel.r 396 2019-01-02 22:53:10Z viari $
#
# GC correction - make Ploidy model
#

suppressMessages(library(asdog))

prog.usage <- function() {
  .cat <- function(...) cat(..., '\n', file=stderr())
  .cat("usage: asdog.plmodel.r --normal=<normal> --gccorrect=<gccorrect> [--outdir=<dir>] [--<option>=<value>]")
  .cat(" or    asdog.plmodel.r --plmodel=<plmodel> [--outdir=<dir>] [--<option>=<value>]")
  .cat("       <normal>    := path to normal baf file (without extension)")
  .cat("       <gccorrect> := path to gccorrect (.rds) file")
  .cat("       <plmodel>   := path to previous plmodel (.rds) file")
  .cat("       <dir>       := output directory (dft='.')")
}

#
# get arguments
#

args <- asdog.main.args("cnpredict:")

# nice diplo
#args$normal = "/TmpStorage/HER2.P01952.BL110D1.WGS"
#args$gccorrect = "HER2.P01952.TU110C1.WGS.gccorrect"

# tetra conta
#args$normal = "/TmpStorage/HER2.P00661.BL11096.WGS"
#args$gccorrect = "HER2.P00661.TU1103C.WGS.gccorrect"

# high conta
#args$normal = "/TmpStorage/HER2.P02987.BL11078.WGS"
#args$gccorrect = "HER2.P02987.TU1107C.WGS.gccorrect"

# ewing test
#args$normal = "/TmpStorage/EWING.PA13012.BD1300B.WGS"
#args$gccorrect = "/TmpStorage/EWING.PA13012.PR1300E.WGS.gccorrect"

# carcinosarcome
#args$normal = "/TmpStorage/GYNCS.PA16005.BD16005.WGS"
#args$gccorrect = "/TmpStorage/GYNCS.PA16005.PR16007.WGS.gccorrect"

#args$plmodel.shmm.segsize=10

mode <- if (is.character(args$normal) && is.character(args$gccorrect)) "newmodel" else
        if (is.character(args$plmodel)) "recompute" else
        NULL

if (is.null(mode)) {
  asdog.main.exit(1, usage=TRUE)
}

#
# check files
#

if (mode == "newmodel") {
  srcfile <- asdog.check.file(args$gccorrect, "rds", .bgz=NULL)
  args$gccorrect <- NULL
} else {
  srcfile <- asdog.check.file(args$plmodel, "rds", .bgz=NULL)
  args$plmodel <- NULL
}
  
#
# load GC correct or PL model
#

lx.out("---------------------------------------", with.mem=T)
lx.out(" loading : ", srcfile)
lx.out("---------------------------------------")

rcsource <- readRDS(file=srcfile)

#
# get parameters
#

params <- asdog.main.params(args, rcsource$params)

# next line is for development (when adding new parameters in asdog.params.r)
# you may comment it in production phase
params <- lx.mixin(asdog.new.params(), params)

#
# create output directory if needed
#

dir.create(params$outdir, showWarnings=FALSE)

#
# run PL model
#

plmodel <- asdog.plmodel(rcsource, params)

#
# outputs names
#

out.short <- asdog.clean.path.suffix(srcfile, c("rds", "gccorrect", "plmodel"))
out.full  <- paste0(params$outdir, "/", basename(out.short), ".plmodel")
out.base  <- basename(out.full)

#
# save model
#

fname <- paste0(out.full, ".rds")

lx.out("---------------------------------------", with.mem=T)
lx.out(" saving model to: ", fname)
lx.out("---------------------------------------")

saveRDS(plmodel, file=fname)

#
# print pdf report
#

setwd(params$outdir)
fname <- out.base

lx.out("---------------------------------------", with.mem=T)
lx.out(" making pdf report: ", paste0(fname, ".pdf"))
lx.out("---------------------------------------")

asdog.pdf.report(plmodel, fname)

#
# end
#
lx.out("---------------------------------------", with.mem=T)
lx.out(" End asdog.plmodel")
lx.out("---------------------------------------")


asdog.main.exit(0)
