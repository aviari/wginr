# -------------------------------------------------
# $Id: asdog.segment.report.r 396 2019-01-02 22:53:10Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# Segmentation latex reporting
#

# -------------------------------------------------
# histogram of segments number and density
#
.histo.segnum <- function(segment, what=c("rcaf", "rrc")) {
  
  .mean <- function(x, w) {
    ok <- ! (is.na(x) | is.infinite(x))
    lx.wt.mean(x[ok], w[ok], na.rm=T)
  }
  
  what <- match.arg(what)
  segs <- segment[[paste0(what, ".segs")]]
  segs.chr <- split(segs, segs$chr)
  
  opar <- par(no.readonly=T)
  par(mfrow=c(3,1), xpd=NA)
  tab <- table(segs$chr)
  colr <- asdog.chromos.colors(names(tab))
  lx.barplot(tab, cex.text=0.7, col=colr, cex.names=0.7,
             main=paste0("number of ",  what, " segments"))
  dens <- tab / sapply(segment$header$seq, function(x) x$size)[as.integer(names(tab))]
  dens <- round(dens * 1e8) / 100
  lx.barplot(dens, cex.text=0.7, col=colr, cex.names=0.7,
             srt.text=90, pos.text=3, off.text=0.8, col.text=1,
             main=paste0("number of ",  what, " segments per Mb"))
  zsco <- sapply(segs.chr, function(seg) .mean(seg$zscor.rrc, seg$to-seg$from+1))
  pval <- sapply(segs.chr, function(seg) .mean(seg$pval.rrc, seg$to-seg$from+1))
  colr <- asdog.gradient.colors(abs(zsco), xrange=c(0, 10), nlevels=11, alpha=0.5, rev=T)
  lx.barplot(round(pval*100)/100, col=colr, cex.names=0.7, cex.text=0.7, srt.text=90,
             pos.text=3, off.text=0.8, col.text=1,
             main=paste0("mean ", what, " segments quality"))
  par(opar)
  invisible()
}

# -------------------------------------------------
# pies of Gain-Loss
#
.pies.gainloss <- function(segment, what=c("rcaf", "rrc")) {
  
  what <- match.arg(what)
  segs <- segment[[paste0(what, ".segs")]]
  colr <- lx.COLORS[c(3, 1, 4)] # order: L N G
  names(colr) <- c("L", "N", "G")
  
  segs$size <- segs$to - segs$from + 1
  segs$stat <- factor(names(colr)[sign(segs$delta.cn)+2], levels=names(colr))
  segs.chr <- split(segs, segs$chr)
  cnt <- lapply(segs.chr, function(x)
              aggregate(x$size, list(stat=x$stat), sum, drop=F))
  
  opar <- par(no.readonly=TRUE)
  par(mfrow=lx.mfrow(length(cnt)+1),
      mai = c(0, 0, 0, 0))
  
  lapply(names(cnt), function(chr) {
    pie(cnt[[chr]]$x, col=colr[cnt[[chr]]$stat], labels="")
    text(0, 0, chr, cex=2, font.main=1, family="mono", col="white")
  })
  
  pie(1, col=0, labels="", radius=0)
  legend('center', legend=c('Gain', 'Normal', 'Loss'),
         fill=colr[c(3,2,1)])

  par(opar)
  invisible()
}

# -------------------------------------------------
# lines of Gain-Loss
#
.lines.gainloss <- function(segment, what=c("rcaf", "rrc")) {
  
  what <- match.arg(what)
  segs <- segment[[paste0(what, ".segs")]]
  
  segs.chr <- split(segs, segs$chr)
  colr <- lx.COLORS[c(3, 1, 4)] # order: L N G
  nchr <- length(segs.chr)
  xmax <- max(segs$to)
  ymax <- 10 * nchr + 1
  plot(c(-xmax/10,xmax), c(-1, ymax), cex=0, xlab="", ylab="",
       xaxt='n', yaxt='n', frame.plot=F)
  off <- ymax - 10
  lapply(segs.chr, function(seg) {
    y <- sign(seg$delta.cn)
    col <- colr[y+2]
    wid <- ifelse(y == 0, 2, 5)
    segments(seg$from, off+y, seg$to, off+y, col=col, lwd=wid, lend=1)
    off <<- off - 10
  })
  
  text(rep(-xmax/10, nchr), seq(ymax-10, 1, by=-10), names(segs.chr), pos=4, offset=1,
       cex=0.7, font.main=1, family="mono")
  
  legend('bottomright', legend=c('Gain', 'Normal', 'Loss'),
         fill=colr[c(3,2,1)], cex=0.7)
  
  title(paste0(what, " gain-loss"))
  
  invisible()
}

# =================================================
# API
# =================================================

# -------------------------------------------------
#' produce segmentation tex report 
#
asdog.tex.report.segment <- function(tex, segment) {
  
  params <- segment$params
  
  # -----------------------------------------------
  # @fixme: there is problem with pdf driver and par()
  #         the main and labels family is not inherited
  #         so we do have to set it each time
  # -----------------------------------------------
  
  # -----------------------------------------------
  # parameters section
  #
  
  p <- asdog.filter.params(params, in.filter="segment:")
  p <- as.matrix(p, ncol=2)
  p <- as.data.frame(apply(p, 1, paste))
  names(p) <- 'value'
  tex.section(tex, "Parameters")
  tex.print(tex, p, size='small')
  
  # -------------------------------
  # segments summary table
  #
  
  if (! is.null(segment$rcaf.segs)) {
    tex.tag(tex, "newpage")
    tex.section(tex, "RCAF segments summary")
    
    # fig on
    #
    tex <- tex.fig.on(tex, width=7, height=7, family="Times")
    
    .histo.segnum(segment, "rcaf")
    
    tex <- tex.fig.off(tex)
    #
    # fig off
  }
  
  if (! is.null(segment$rrc.segs)) {
    tex.tag(tex, "newpage")
    tex.section(tex, "RRC segments summary")
    
    # fig on
    #
    tex <- tex.fig.on(tex, width=7, height=7, family="Times")
    
    .histo.segnum(segment, "rrc")
    
    tex <- tex.fig.off(tex)
    #
    # fig off
  }
  
  # -------------------------------
  # Gain-Loss pies summary
  #
  
  if (! is.null(segment$rcaf.segs)) {
    tex.tag(tex, "newpage")
    tex.section(tex, "RCAF Gain-Loss summary")
    
    # fig on
    #
    tex <- tex.fig.on(tex, width=7, height=7, family="Times")
    
    .pies.gainloss(segment, "rcaf")
    
    tex <- tex.fig.off(tex)
    #
    # fig off
  }
  
  if (! is.null(segment$rrc.segs)) {
    tex.tag(tex, "newpage")
    tex.section(tex, "RRC Gain-Loss summary")
    
    # fig on
    #
    tex <- tex.fig.on(tex, width=7, height=7, family="Times")
    
    .pies.gainloss(segment, "rrc")
    
    tex <- tex.fig.off(tex)
    #
    # fig off
  }

  # -------------------------------
  # Gain-Loss lines summary
  #
  
  if (! is.null(segment$rcaf.segs)) {
    tex.tag(tex, "newpage")
    tex.section(tex, "RCAF Gain-Loss lines")
    
    # fig on
    #
    tex <- tex.fig.on(tex, width=7, height=7, family="Times")
    
    .lines.gainloss(segment, "rcaf")
    
    tex <- tex.fig.off(tex)
    #
    # fig off
  }
  
  if (! is.null(segment$rrc.segs)) {
    tex.tag(tex, "newpage")
    tex.section(tex, "RRC Gain-Loss lines")
    
    # fig on
    #
    tex <- tex.fig.on(tex, width=7, height=7, family="Times")
    
    .lines.gainloss(segment, "rrc")
    
    tex <- tex.fig.off(tex)
    #
    # fig off
  }
  
  # -----------------------------------------------
  # Profiles section - all chromosomes
  #  
  
  if (! is.null(segment$rcaf.segs)) {
    
    lx.out("reporting profiles RCAF - all chromosomes")
    
    tex.tag(tex, "newpage")
    tex.section(tex, "RCAF Profiles - all chromosomes")

    # fig on
    #
    tex <- tex.fig.on(tex, width=7, height=7, family="Times")
    
    plot(segment, what="rcaf")  
    
    tex <- tex.fig.off(tex)
    #
    # fig off
  }

  if (! is.null(segment$rrc.segs)) {
    
    lx.out("reporting profiles RRC - all chromosomes")
    
    tex.tag(tex, "newpage")
    tex.section(tex, "RRC Profiles - all chromosomes")
    
    # fig on
    #
    tex <- tex.fig.on(tex, width=7, height=7, family="Times")
    
    plot(segment, what="rrc")  
    
    tex <- tex.fig.off(tex)
    #
    # fig off
  }
  
  # -----------------------------------------------
  # Profiles section - by chromosomes
  #
  
  if (! is.null(segment$rcaf.segs)) {
    
    lx.out("reporting profiles RCAF - by chromosome")
    
    tex.tag(tex, "newpage")
    tex.section(tex, "RCAF Profiles - by chromosomes")

    chrs <- as.character(unique(segment$rcaf.segs$chr))
    
    lapply(chrs, function(chr) {
      
      lx.out("  chr: ", chr)
      
      # fig on
      #
      tex <- tex.fig.on(tex, width=7, height=4, family="Times")
      
      plot(segment, what="rcaf", chrs=chr, main=paste0("chr", chr))
      
      tex <- tex.fig.off(tex)
      #
      # fig off
    })
  }
  
  if (! is.null(segment$rrc.segs)) {
    
    lx.out("reporting profiles RRC - by chromosome")
    
    tex.tag(tex, "newpage")
    tex.section(tex, "RRC Profiles - by chromosomes")
    
    chrs <- as.character(unique(segment$rrc.segs$chr))
    
    lapply(chrs, function(chr) {
      
      lx.out("  chr: ", chr)
      
      # fig on
      #
      tex <- tex.fig.on(tex, width=7, height=4, family="Times")
      
      plot(segment, what="rrc", chrs=chr, main=paste0("chr", chr))
      
      tex <- tex.fig.off(tex)
      #
      # fig off
    })
  }
  
  # -----------------------------------------------
  # end
  
  lx.out("end of report", with.mem=T)

  invisible(tex)
}
