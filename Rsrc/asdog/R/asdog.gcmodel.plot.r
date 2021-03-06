# -------------------------------------------------
# $Id: asdog.gcmodel.plot.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# GCModel model : plotting
#

# -------------------------------------------------
# plot GC model optimization results
#
asdog.plot.gcmodel.optim <- function(optim, col=heat.colors(20), ...) {

  scores <- optim$scores
  xgrid <- seq(0,1,length.out=nrow(scores))
  ygrid <- seq(0,1,length.out=ncol(scores))
  
  image(log10(scores+1e-6), axes=F,
        xlab="Small window (b)", ylab="Large window (Kb)",
        col=col, ...)
  
  axis(1, at=xgrid, labels=rownames(scores))
  axis(2, at=ygrid, labels=as.numeric(colnames(scores))/1000)

  if (nrow(scores) > 1) abline(v=xgrid+(0.5/(nrow(scores)-1)))
  if (ncol(scores) > 1) abline(h=ygrid+(0.5/(ncol(scores)-1)))

  ix <- which(rownames(scores) == optim$model$winsize.gc[1])
  iy <- which(colnames(scores) == optim$model$winsize.gc[2])
  
  if (length(ix*iy) > 0) 
    points(xgrid[ix], ygrid[iy], pch=3, cex=2)
  
  title(paste0("fit = ", optim$info$fit,
               " / ", optim$info$objective),
        cex.main=0.9, font.main=1)
  invisible()
}

# -------------------------------------------------
# plot GC model best model from optim
# this will actually call plot.GCGloessModel
#
asdog.plot.gcmodel.model <- function(optim, ...) plot(optim$model, ...)

# -------------------------------------------------
# S3 method to plot GCModel model
# see also : plot.GCGloessModel in asdog.gcmodel.gloess.r
#
plot.GCModel <- function(gcmodel, what=c("model", "optim"), ...) {

  what <- match.arg(what)
  
  fun <- lx.get0(paste0("asdog.plot.gcmodel.", what), mode="function")
  
  fun(gcmodel$optim)
}
