# -------------------------------------------------
# $Id: asdog.gccorrect.report.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# gccorrect latex reporting
#

# -------------------------------------------------
# RRC plot
# note: should use plot.GCCorrect S3 method in asdog.gccorrect.plot.r
#
.rrcplot <- function(x, y, nbp=5000, ymax=4, cex=0.1, main="", axis=T, ...) {
  
  xaxt <- ifelse(axis, "s", "n")
  xlab <- ifelse(axis, "position", "")
  ylab <- ifelse(axis, "relative read counts", "")
  ymargin <- 1.05
  ytop    <- 1.10
  
  # all points in black
  #
  ymax <- ifelse(ymax > 0, ymax, max(y))
  xmax <- max(x) + 1
  y[y > ymax] <- ymax * ymargin
  
  lx.plot(x, y, xlim=c(0, xmax), ylim=c(0, ymax*ytop), type='p', cex=cex, 
       main="", xpd=NA, xlab=xlab, ylab=ylab, xaxt=xaxt, xaxs='i',
       .samp=nbp, .method="spaced", ...)
  title(main=main, adj=1, line=-1, font.main=1, family="Times")

  # overflow points in magenta
  #
  imap <- which(y >= ymax)
  ymap <- rep(ymax, length(imap)) * ymargin
  lines(x[imap], ymap, type='p', col=6, cex=cex, ...)
  
  # horizontal line at cn=1
  #
  abline(h=1, col=3)
  
  invisible(NULL)
}

# -------------------------------------------------
#' produce gccorrect tex report 
#
asdog.tex.report.gccorrect <- function(tex, correct) {
  
  params <- correct$params
  
  smooth <- ! (is.null(params$gccorrect.plot.smooth) ||
               all(params$gccorrect.plot.smooth == 0))
                                                          
  # -----------------------------------------------
  # parameters section
  #
  
  p <- asdog.filter.params(params, in.filter="gccorrect:")
  p <- as.matrix(p, ncol=2)
  p <- as.data.frame(apply(p, 1, paste))
  names(p) <- 'value'
  tex.section(tex, "Parameters")
  tex.print(tex, p, size='small')
  
  #
  # collect data by chr (and smooth if required)
  #
  
  bychr <- lapply(correct$bychr, function(res) {
    rrc.smo <- if (smooth) lx.smooth.median(res$rrc.cor, 
                                            k=params$gccorrect.plot.smooth)
               else NULL
    list(pos=res$pos, rrc.cor=res$rrc.cor, rrc.raw=res$rrc.raw, rrc.smo=rrc.smo)
  })
  
  #
  # collect data for all chr
  #
  
  df.all <- data.frame(raw=unlist(lapply(bychr, function(x) x$rrc.raw), use.names=F),
                       cor=unlist(lapply(bychr, function(x) x$rrc.cor), use.names=F))
  
  if (smooth)
    df.all$smo <- unlist(lapply(bychr, function(x) x$rrc.smo), use.names=F)
  
  # -----------------------------------------------
  # CN total distributions (raw, cor and smo)
  #
  
  tex.tag(tex, "newpage")
  tex.section(tex, "Relative read counts total distribution")
  
  tex.subsection(tex, "statistics")
  df.stat <- apply(df.all, 2, summary)
  colnames(df.stat) <- c("raw", "corrected", if(smooth) "smooth")
  tex.print(tex, df.stat)
  
  tex.subsection(tex, "distributions")
  
  bins.max  <- params$gccorrect.plot.rcmax
  if (bins.max <= 0) bins.max <- max(df.all$raw, df.all$cor) # @fixme: [AV] may use quantile instead
  bins.rgn  <- c(0, bins.max)
  
  .filter <- function(x) x[x <= bins.max]
  .hist   <- function(x) hist(.filter(x), breaks=100, plot=F)
  .norm   <- function(h, hh=h) h$density*max(hh$density)/(max(h$density)+1e-6)
  
  hist.raw <- .hist(df.all$raw)
  hist.cor <- .hist(df.all$cor)
  hist.smo <- if (smooth) .hist(df.all$smo) else NULL

  # fig on
  #
  tex <- tex.fig.on(tex, width=4, height=4, family="Times")

  # histograms
  
  plot(hist.raw$mids, .norm(hist.raw, hist.cor), xlim=bins.rgn,
       type='l', col=1,
       font.main=1, family="Times", main="", 
       xlab="relative read counts", ylab="density")
  
  lines(hist.cor$mids, .norm(hist.cor), xlim=bins.rgn, col=2)
  
  if (smooth)
    lines(hist.smo$mids, .norm(hist.smo, hist.cor), xlim=bins.rgn, col=3)
  
  abline(v=1, col=1, lty=2)
  
  # legend
  
  leg.txt <- c('raw', 'corr', if (smooth) 'smooth' else NULL)
  leg.col <- c(1, 2, if (smooth) 3 else NULL)
  legend(x='topright', legend=leg.txt, col=leg.col, lty=1)
  
  tex <- tex.fig.off(tex)
  #
  # fig off

  # -----------------------------------------------
  # global RRC plot
  #
  tex.tag(tex, "newpage")
  tex.section(tex, "Relative read counts profile")

  fig.nrows  <- if (smooth) 3 else 2
  fig.height <- if (smooth) 9 else 6
  fig.cex    <- params$gccorrect.plot.cex
  fig.nbpts  <- params$gccorrect.plot.nbpts

  #
  
  glob.len <- as.numeric(sapply(bychr, function(x) max(x$pos)))
  glob.lim <- head(cumsum(c(0, glob.len)), -1)
  glob.pos <- unlist(mapply(function(x, l) l+x$pos, bychr, glob.lim), use.names=F)

  .plot.delim <- function() {
    abline(v=cumsum(glob.len))
    mtext(names(bychr), side=3, line=-1,
          at=cumsum(glob.len)-glob.len/2,
          adj=0.5, cex=0.5, las=3)
  }
  
  # fig on
  #
  tex <- tex.fig.on(tex, width=7, height=fig.height, family="Times")

  opar <- par(no.readonly=TRUE)
  par(mfrow=c(fig.nrows, 1),
      mgp=c(2,1,0),
      mar=c(0, 4, 0, 1) + 0.1,
      oma=c(4, 0, 0, 0) + 0.1)
  
  .rrcplot(glob.pos, df.all$raw, nbp=fig.nbpts,
           ymax=bins.max, cex=fig.cex, axis=F,
           main="")
  .plot.delim()
  
  .rrcplot(glob.pos, df.all$cor, nbp=fig.nbpts,
           ymax=bins.max, cex=fig.cex, axis=!smooth,
           main="")
  .plot.delim()
  
  if (smooth) {
    .rrcplot(glob.pos, df.all$smo, nbp=fig.nbpts,
             ymax=bins.max, cex=fig.cex, axis=T,
             main="")
    .plot.delim()
  }
  par(opar)
  
  tex <- tex.fig.off(tex)
  #
  # fig off
  
  # -----------------------------------------------
  # RRC plots by chromosome
  #
  
  tex.tag(tex, "newpage")
  tex.section(tex, "Relative read counts profile by chromosome")

  lx.napply(bychr, function(chr, res) {
    
    lx.out("plotting chr : ", chr, level="debug")
    
    # fig on
    #
    tex <- tex.fig.on(tex, width=7, height=fig.height, family="Times")
    opar <- par(no.readonly=TRUE)
    par(mfrow=c(fig.nrows, 1),
        mgp=c(2,1,0),
        mar=c(0, 4, 0, 1) + 0.1,
        oma=c(4, 0, 0, 0) + 0.1)
    .rrcplot(res$pos, res$rrc.raw, nbp=fig.nbpts,
             ymax=bins.max, cex=fig.cex, axis=F,
             main=paste0('chr', chr, ' raw '))
    .rrcplot(res$pos, res$rrc.cor, nbp=fig.nbpts,
             ymax=bins.max, cex=fig.cex, axis=!smooth,
             main=paste0('chr', chr, ' corr '))
    if (smooth)
      .rrcplot(res$pos, res$rrc.smo, nbp=fig.nbpts,
               ymax=bins.max, cex=fig.cex, axis=T,
               main=paste0('chr', chr, ' smooth'))
    par(opar)
    tex <- tex.fig.off(tex)
    #
    # fig off
    
  })
  
  invisible(tex)
}

