#!/usr/bin/env Rscript
# $Id: asdog.report.r 396 2019-01-02 22:53:10Z viari $
#
# reporting
#

suppressMessages(library(asdog))

prog.usage <- function() {
  .cat <- function(...) cat(..., '\n', file=stderr())
  .cat("usage: asdog.report.r --base=<base> [--outdir=<dir>] [--<option>=<value>]")
  .cat("       <base> := path to .rds file")
  .cat("       <dir>  := output directory (dft='.')")
}

#
# get arguments
#

args <- asdog.main.args("report:")

if (! is.character(args$base)) {
  asdog.main.exit(1, usage=TRUE)
}

args.base <- args$base
args$base <- NULL

#
# create output directory if needed
#

args.outdir <- paste0(ifelse(is.null(args$outdir), '.', args$outdir), '/')
dir.create(args.outdir, showWarnings=FALSE)
args$outdir <- NULL

#
# load data
#

fname <- asdog.check.file(args.base, ".rds", .bgz=NULL)

lx.out("---------------------------------------", with.mem=T)
lx.out(" loading : ", fname)
lx.out("---------------------------------------")

object <- readRDS(file=fname)

#
# get data type
#

if (! ("Asdog" %in% class(object))) {
  lx.warn('RDS file: ', fname, ' not an Asdog object')
  asdog.main.exit(3)
}

object.class <- NA

for (clas in c("GCModel", "GCCorrect", "PLModel", "Segment")) {
  if (clas %in% class(object)) object.class <- clas
}

if (is.na(object.class)) {
  lx.warn('RDS file: ', fname, ' unknown R object class')
  asdog.main.exit(4)
}

#
# outputs names
#

out.short <- asdog.clean.path.suffix(args.base, "rds")
out.full <- paste0(args.outdir, basename(out.short))
out.base <- basename(out.full)

#
# get parameters
#

object$params <- asdog.main.params(args, object$params)

# next line is for development (when adding new parameters in asdog.params.r)
# you may comment it in production phase
object$params <- lx.mixin(asdog.new.params(), object$params)

#
# table exports
#

.writer.GCCorrect <- function(x, fname) {
    asdog.write.table.GCCorrect(x, gzfile(paste0(fname, ".txt.gz")))
}

.writer.Segment <- function(x, fname) {
  asdog.write.table.Segment(x, gzfile(paste0(fname, ".rcaf.txt.gz")), what="rcaf")
  asdog.write.table.Segment(x, gzfile(paste0(fname, ".rrc.txt.gz")), what="rrc")
}

table.writer <- paste0('.writer.', object.class)

if (exists(table.writer)) {

  table.writer <- lx.get0(table.writer, mode="function")

  fname <- out.full

  lx.out("---------------------------------------", with.mem=T)
  lx.out(" making table export ", fname)
  
  table.writer(object, fname)

  lx.out("---------------------------------------")
}

#
# summary export
#
# TBD

#
# print pdf report
#

setwd(args.outdir)  # need to change directory for tmp/

fname <- out.base

pdf.reporter <- paste0('asdog.pdf.report.', object.class)

if (exists(pdf.reporter)) {

  lx.out("---------------------------------------", with.mem=T)
  lx.out(" making pdf report: ", paste0(fname, ".pdf"))
  lx.out("---------------------------------------")

  pdf.reporter <- lx.get0(pdf.reporter, mode="function")
  
  pdf.reporter(object, fname)
  
}

#
# end
#
lx.out("---------------------------------------", with.mem=T)
lx.out(" End asdog.report")
lx.out("---------------------------------------")


asdog.main.exit(0)
