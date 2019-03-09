# -------------------------------------------------
# $Id: asdog.gccorrect.plot.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# GCCorrect plotting
#

# -------------------------------------------------
# S3 method to plot GCCorrect
# note: this function is not used in report it is provided
#       for interactive usage only
# note: we should write a chrs version (as in plot.Segment)
#
plot.GCCorrect <- function(correct, chr='1', what=c("cor", "raw"), 
                           xlim=NULL, ylim=NULL, cex=0.1, ...) {
  
  what <- match.arg(what)
  
  res <- correct$bychr[[as.character(chr)]]
  
  x <- res$pos
  y <- res[[paste0('rrc.', what)]]
  
  
  if (is.null(xlim)) {
    xmax <- max(x) + 1
    xlim <- c(1, xmax)
  }
  
  ymargin <- 1.05
  ytop    <- 1.10
  
  if (is.null(ylim)) {
    indx <- lx.findInterval(xlim, x)
    ymax <- max(y[indx[1]:indx[2]])
    ylim <- c(0, ymax*ytop)
  }
  
  ymax <- ylim[2]
  
  ylim[2] <- ymax * ymargin
  
  y[y > ymax] <- ymax * ymargin
  
  # all points in black
  #
  lx.plot(x, y, xlim=xlim, ylim=ylim, type='p', cex=cex, xaxs='i',
          main="", xlab="position", ylab="relative read counts", ...)
  title(main=paste0('chr', chr, ' ', what)) 
  
  # overflow points in magenta
  #
  imap <- which(y >= ymax)
  ymap <- rep(ymax, length(imap)) * ymargin
  lines(x[imap], ymap, type='p', col=6, cex=cex, ...)
  
  # horizontal line at rrc=1
  #
  abline(h=1, col=4)
  
  invisible()
}

