#!/usr/bin/env Rscript
# $Id: gccorrect.mappable.r 120 2016-10-29 07:45:45Z viari $
#
# compute binnd mappable regions from map file
#

require(gccor)


prog.usage <- function() {
  .cat <- function(...) cat(..., '\n', file=stderr())
  .cat("usage: gccorrect.mappable.r --map=<map> --ref=<reference> [--outdir=<dir>] [--<option>=<value>]")
  .cat("       <map>    := base path of mappable file (without extension)")
  .cat("       <ref>    := base path of basta file (without extension)")
  .cat("       <dir>    := output directory (dft='.')")
  .cat("       options  :")
  .cat("             --binsize=<binsize>     bin size in bp (dft=1000)")
  .cat("             --threshold=<threshold> mappability threshold (dft=0.9)")
}

#
# get arguments
#

args <- gccor.main.args("none:")

if (is.null(args$map) || is.null(args$ref)) {
  gccor.main.exit(1, usage=TRUE)
}

map.file   <- paste0(args$map, '.bed')
basta.file <- paste0(args$ref, '.bst')

binsize <- if (is.null(args$binsize)) 1000 else as.integer(args$binsize)
threshold <- if (is.null(args$threshold)) 0.9 else as.double(args$threshold)

args.outdir <- paste0(ifelse(is.null(args$outdir), '.', args$outdir), '/')
dir.create(args.outdir, showWarnings=FALSE)

#
# go ahead
#

lx.out("---------------------------------------", with.mem=T)
lx.out('reading map bed file: ', map.file)
lx.out("---------------------------------------")

bed <- bed.read(map.file)

basta <- basta.open(basta.file)

clocs <- bed2clocs(basta, bed)

tot.size <- sum(sapply(basta$header$seq, function(x) x$size))
int.size <- clocs[,3]-clocs[,2]+1
cover    <- sum(int.size)

lx.out("initial cover: ", cover, '/', tot.size, ' bp. (', round(cover*100/tot.size),'%)')

int.size <- int.size[int.size >= binsize]
cover    <- sum(int.size)

lx.out("initial bin ", binsize, " cover: ", cover, '/', tot.size, ' bp. (', round(cover*100/tot.size),'%)')

lx.out("---------------------------------------", with.mem=T)
lx.out('computing bits from clocs')
lx.out("---------------------------------------")

bits <- clocs2bits(basta, clocs)

lx.out("---------------------------------------", with.mem=T)
lx.out('binsum bits binsize=', binsize, ' threshold=', threshold)
lx.out("---------------------------------------")

clocs <- lx.napply(bits, function(chr, bit) {
  chr <- as.integer(chr)
  size <- basta$header$seq[[chr]]$size
  lx.out("binsum on chrindex: ", chr, " size=", size)
  bins <- lx.binsum(bit, binsize) / binsize
  bins <- seq.int(1, size, by=binsize)[bins >= threshold]
  if (length(bins) > 0)
    clocs.matrix(cbind(chr, bins, bins+binsize-1))
  else
    clocs.matrix(NULL)
}, SIMPLIFY=F, use.threads = lx.use.threads())

lx.out("---------------------------------------", with.mem=T)
lx.out('reducing clocs')
lx.out("---------------------------------------")

clocs <- do.call(rbind, clocs)

clocs <- clocs.reduce(clocs)

cover <- sum(clocs[,3]-clocs[,2]+1)
lx.out("final bin ", binsize, " cover: ", cover, '/', tot.size, ' bp. (', round(cover*100/tot.size),'%)')

lx.out("---------------------------------------", with.mem=T)
lx.out('formatting clocs')
lx.out("---------------------------------------")

clocs[,2] <- clocs[,2]-1    # 0-based
tlocs <- format(clocs, trim=T, scientific=F)

out.file <- paste0(args.outdir, basename(args$map), format(binsize/1000), 'k', format(threshold*100), '.bed')

lx.out("---------------------------------------", with.mem=T)
lx.out('writing bed file: ', out.file)
lx.out("---------------------------------------")

write.table(tlocs, file=out.file, sep='\t', quote=F, row.names=F, col.names=F)

lx.out("---------------------------------------", with.mem=T)
lx.out(" End gccorrect.mappable")
lx.out("---------------------------------------")

gccor.main.exit(0)

