# -------------------------------------------------
# $Id: asdog.gcmodel.report.r 298 2017-08-14 17:56:27Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# GC correction model latex reporting
#

# -------------------------------------------------
# <internal> <no-export>
# produce empty plot
# -------------------------------------------------

.noplot <- function(label="no data", boxed=TRUE) {
  plot.new()
  if (boxed) box()
  text(0.5, 0.5, label)
  invisible()
}

# -------------------------------------------------
# <internal> <no-export>
# plot coverage histogram with densities
#
# cnt  : counts
# rng  : plot range
# pos  : vertical line at cov=pos
# dsmo : smoothed density or smooth factor
# bnd  : selected band
# lgn  : legend on/off
# -------------------------------------------------

.cover.histo <- function(cnt, rng=range(cnt), pos, dsmo, bnd=NULL, 
                         lgn=FALSE, .mincnt=100) {
  
  cnt <- cnt[(cnt != 0) & (cnt >= rng[1]) & (cnt <= rng[2])]
  
  if (length(cnt) < .mincnt)
    return(.noplot(paste0("count < ", .mincnt)))

  h <- hist(cnt, breaks="FD", plot=FALSE)
  d <- lx.density(cnt)
  
  if (! ("density" %in% class(dsmo)))
    dsmo <- lx.density(lx.smooth.median(cnt, k=dsmo))

  # histogram  
  plot(h$mids, h$density, type="h",
     xlim=rng, ylim=c(0, max(h$density)*1.1),
     xlab="Coverage", ylab="Density",
     main="", font.main=1, family="Times")
  points(h$mids, h$density, type="s")
  abline(h=0)
  abline(v=pos, lty=2, lwd=1, col=2)
  
  # densities
  lines(d$x, d$y*max(h$density)/max(d$y), col=1, lwd=2)
  lines(dsmo$x, dsmo$y*max(h$density)/max(dsmo$y), col=2, lwd=2)
  
  # select band and area under band
  if (! is.null(bnd)) {
    abline(v=bnd, lty=2, lwd=1, col=3)
    z <- d$x>=bnd[1] & d$x<=bnd[2]
    if (any(z)) {
      x <- d$x[z]
      y <- d$y[z]
      polygon(c(min(x), x, max(x)), c(0, y, 0), 
              col=lx.color.alpha("blue", 0.2))
    }
  }
  
  # legend
  if (lgn)
    legend("topright", legend=c("obs", "smooth"), 
           cex=0.7, col=c(1, 2), lwd=1)

  invisible()
}

# -------------------------------------------------
# <internal> <no-export>
# plot coverage profile with chr boundaries
#
.cover.profile <- function(pos, rcov, model=NULL, ylab="", main="") {
  lx.plot(pos, rcov,
          ylim=c(0, 2),
          xlab="pos", ylab=ylab,
          main=main, font.main=1, family="Times")
  abline(h=1, col=2)
  if (! is.null(model)) {
    rle <- rle(coords2clocs(model, pos)[,1])
    csm <- cumsum(rle$lengths)
    abline(v=pos[csm], col=1)
    text(x=pos[csm], y=2, rle$values, pos=2, 
         cex=0.5, family="Times")
  }
  invisible()
}

# -------------------------------------------------
# <internal> <no-export>
# fit log(y) ~ x with error handling
#
.logfit <- function(x, y, .ratio=10) {
  tryCatch(lm(log10(y + min(y[y>0])/.ratio) ~ x),
           error=function(e) NULL)
}

# -------------------------------------------------
# <internal> <no-export>
# put text label into plot
#
.text <- function(at=c("bl", "tl", "tr", "br"), label, 
                  delta=c(0.05, 0.1), log="", cex=0.8, ...) {
  at  <- match.arg(at)
  usr <- par("usr")
  rng <- diff(usr)[c(1,3)]
  pos <- switch(at,
                bl=c(usr[1], usr[3],  1,  1, 4),
                tl=c(usr[1], usr[4],  1, -1, 4),
                tr=c(usr[2], usr[4], -1, -1, 2),
                br=c(usr[2], usr[3], -1,  1, 2))
  x <- pos[1]+pos[3]*rng[1]*delta[1]
  y <- pos[2]+pos[4]*rng[2]*delta[2]
  if (grepl("x", log)) x <- 10^x
  if (grepl("y", log)) y <- 10^y
  text(x, y, label, pos=pos[5], cex=cex, 
       family="Times", ...)
}  

# =================================================
# API
# =================================================

# -------------------------------------------------
#' produce gccor.model tex report 
#
asdog.tex.report.gcmodel <- function(tex, model) {
  
  params <- model$params
  
  # -----------------------------------------------
  # @fixme: there is problem with pdf driver and par()
  #         the main and labels family is not inherited
  #         so we do have to set it each time
  # -----------------------------------------------
  
  # -----------------------------------------------
  # parameters section
  #
  
  p <- asdog.filter.params(params, in.filter="gccorrect:")
  p <- as.matrix(p, ncol=2)
  p <- as.data.frame(apply(p, 1, paste))
  names(p) <- 'value'
  tex.section(tex, "Parameters")
  tex.print(tex, p, size='small')
  
  # -----------------------------------------------
  # regions section
  #  
  if (! is.null(model$regions)) {
    tex.tag(tex, "newpage")
    tex.section(tex, "Regions selection")    
    tex.subsection(tex, "Coverage statistics")
    
    bins.pos <- model$regions$binpos
    bins.bnd <- model$regions$binrange
    bins.wid <- diff(bins.bnd) * params$gcmodel.regions.plot.alpha
    bins.rng <- pmax(0, bins.bnd + bins.wid * c(-1, 1))
    bins.all <- unlist(model$regions$bins, use.names=F)
    bins.all <- bins.all[bins.all != 0]
    bins.dsm <- model$regions$smooth.dens
    bins.smo <- model$regions$smooth.k
    
    p <- as.matrix(summary(bins.all))
    colnames(p) <- "Coverage"
    tex.print(tex, p, size='small')
    
    tex.subsection(tex, paste("Histogram of coverage in",
                              params$gcmodel.regions.binsize/1000,
                              "kb regions"))
    # fig on
    #
    tex <- tex.fig.on(tex, width=4, height=4, family="Times")
    
    .cover.histo(bins.all, bins.rng, bins.pos, dsmo=bins.dsm,
                 bnd=bins.bnd, lgn=T)
    title("all chromosomes", font.main=1, family="Times")
    
    tex <- tex.fig.off(tex)
    #
    # fig off

    # -------------------------------
    # coverage by chromosomes subsection
    #  
    tex.subsection(tex, paste("Histogram of coverage in",
                              params$gcmodel.regions.binsize/1000,
                              "kb regions per chromosome"))
    bins.splt <- split(model$regions$bins, names(model$regions$bins))
    bins.splt <- bins.splt[order(as.numeric(names(bins.splt)))]
    
    # fig on
    #
    tex <- tex.fig.on(tex, width=7, height=7, family="Times")
    
    opar <- par(no.readonly=TRUE)
    par(mfrow=lx.mfrow(length(bins.splt)),
        mgp = c(3, 0, 0) + 0.2,
        oma = c(1,1,0,0) + 0.1,
        mar = c(1,0,2,1) + 0.1)
    lx.napply(bins.splt, function(chr, bins) {
      bins <- unlist(bins, use.names=F)
      .cover.histo(bins, bins.rng, bins.pos, dsmo=bins.smo,
                   bnd=bins.bnd, lgn=F)
      title(paste0("chr", chr), font.main=1, family="Times")
    }, use.threads=F)
    par(opar)
    
    tex <- tex.fig.off(tex)
    #
    # fig off
  }
  
  # -----------------------------------------------
  # sample section
  #
  if (! is.null(model$sample)) {
    
    # tex.tag(tex, "newpage")
    
    # -------------------------------
    # # samples per chromosome
    #
    tex.section(tex, "Sampling")    
    
    tex.subsection(tex, "Sampled positions per chromosome")
    clocs <- coords2clocs(model, model$sample$coords)
    plot.cnts <- lx.table(clocs[,1], levels=params$chr)
    plot.pchr <- sapply(names(plot.cnts), function(chr) {
      seq <- model$header$seq[[chr]]
      ifelse(is.null(seq), 0, seq$siz)
    })
    plot.pchr <- plot.pchr * params$gcmodel.sample.size / sum(plot.pchr)
    
    # fig on
    #
    tex <- tex.fig.on(tex, width=7, height=3, family="Times")
    
    barplot(rbind(plot.cnts, plot.pchr), beside=TRUE,
            xlab="chromosomes", ylab="Frequency",
            cex.names=0.7, cex.axis=0.7, 
            main="", font.main=1, family="Times")
    box()
    legend(x='topright', legend=c('actual', 'expect.'),
           fill=grey(c(0, 0.8)), cex=0.8)
    
    tex <- tex.fig.off(tex)
    #
    # fig off
    
    # -------------------------------
    # correlogram
    #
    tex.subsection(tex, "Sample correlogram")
    delta <- diff(sort(model$sample$coords))
    q90   <- quantile(delta, 0.90)
    delta <- delta[delta <= q90]

    # fig on
    #
    tex <- tex.fig.on(tex, width=4, height=4, family="Times")
    
    hd  <- hist(delta, breaks="FD", plot=F)
    plot(hd$mids, hd$density, log="y",
         xlab="delta (bp)", ylab="log10 density",
         main=paste0("delta [0, Q90=", q90, "] bp"),
         cex.main=0.9, font.main=1, family="Times")
    fit <- .logfit(hd$mids, hd$density)
    abline(fit, col=2)
    .text("tr", paste0("lambda=", sprintf("%.2f", -1/fit$coefficients[2])), 
          log="y")
    
    tex <- tex.fig.off(tex)
    #
    # fig off

        
    # -------------------------------
    # sample coverage
    #
    tex.subsection(tex, "Sample coverage")
    
    if (is.null(model$regions)) {
      bins.pos <- NA
      bins.bnd <- NULL
      bins.rng <- range(model$sample$pts$cov)
      bins.smo <- NULL
    } else {
      bins.pos <- model$regions$binpos
      bins.bnd <- model$regions$binrange
      bins.wid <- diff(bins.bnd) * params$gcmodel.regions.plot.alpha
      bins.rng <- pmax(0, bins.bnd + bins.wid * c(-1, 1))
      bins.smo <- model$regions$smooth.k
    }
    
    # fig on
    #
    tex <- tex.fig.on(tex, width=7, height=7, family="Times")
    
    opar <- par(no.readonly=TRUE)
    layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
    par(cex.main=0.9, cex.axis=0.7, cex.lab=0.7,
        mgp=c(1.5,0.5,0), mar=c(4,4,1,1))
    
    .cover.histo(model$sample$pts$cov, bins.rng, bins.pos, dsmo=bins.smo,
                 bnd=bins.bnd, lgn=F)
    .text("tr", paste0("median=", sprintf("%.2f", median(model$sample$pts$cov))))
    title("Coverage", font.main=1, family="Times")
    
    lx.hist(model$sample$pts$gc, br=20, bw=0.01, xlim=c(0, 1), xlab="%GC",
            col.density=1, freq=F,
            main="%GC", font.main=1, family="Times")
    box()
    .text("tr", paste0("median=", sprintf("%.2f", median(model$sample$pts$gc))))
    
    lx.plot(model$sample$pts$gc, model$sample$pts$cov,
            xlim=c(0,1), ylim=c(0, 2*median(model$sample$pts$cov)),
            xlab="%GC", ylab="Coverage",
            main="%GC dependency", font.main=1, family="Times")
    cor <- cor(model$sample$pts$gc, model$sample$pts$cov)
    .text("tr", paste0("corr=", sprintf("%.2f", cor)))
    
    par(opar)
    
    tex <- tex.fig.off(tex)
    #
    # fig off
    
  }

  # -----------------------------------------------
  # model section
  #
  tex.tag(tex, "newpage")
  tex.section(tex, "GC Model")
  
  # -------------------------------
  # optimize GC window
  #
  tex.subsection(tex, "Optimized GC windows")
  # fig on
  #
  tex <- tex.fig.on(tex, width=4, height=4, family="Times")
  
  image(as.numeric(rownames(model$optim$scores)), 
        as.numeric(colnames(model$optim$scores)), 
        log10(model$optim$scores+1e-6),
        xlab="Small window", ylab="Large window",
        family="Times",
        col=heat.colors(20))
  
  .grid <- function(x) as.numeric(x)[-1]-diff(as.numeric(x))/2
  abline(h=.grid(colnames(model$optim$scores)))
  abline(v=.grid(rownames(model$optim$scores)))
  
  points(max(0, model$optim$model$winsize.gc[1], na.rm=T),
         max(0, model$optim$model$winsize.gc[2], na.rm=T),
         pch=3, cex=2)
  
  title(paste0("fit = ", model$optim$info$fit,
               " / ", model$optim$info$objective),
        cex.main=0.9, font.main=1, family="Times")
  
  tex <- tex.fig.off(tex)
  #
  # fig off
  
  # -------------------------------
  # plot model
  #
  tex.subsection(tex, "GC model")
  
  # fig on
  #
  width <- if (model$optim$model$nmodel<=1) 4 else 7
  tex <- tex.fig.on(tex, width=width, height=4, family="Times")
  plot(model$optim$model, font.main=1, family="Times")
  tex <- tex.fig.off(tex)
  #
  # fig off
  
  # -------------------------------
  # model prediction on test
  #
  tex.subsection(tex, "Predictions on test set")
  
  # fig on
  #
  tex <- tex.fig.on(tex, width=7, height=7, family="Times")
  
  opar <- par(no.readonly=TRUE)
  layout(matrix(c(1,2,3,3,4,4), 3, 2, byrow = TRUE))
  par(cex.main=1.2, cex.axis=0.7, cex.lab=0.7,
      mgp=c(1.5,0.5,0), mar=c(4,4,1,1))
  
  lx.plot(model$optim$test$gc, 
          model$optim$test$cov/median(model$optim$test$cov, na.rm=T),
          xlim=c(0.2, 0.8),
          ylim=c(0, 2),
          xlab="%GC", ylab="rel. cover",
          main="raw", font.main=1, family="Times")
  abline(h=1, col=2)
  
  lx.plot(model$optim$test$gc, 
          model$optim$test$cov/model$optim$test$pred,
          xlim=c(0.2, 0.8),
          ylim=c(0, 2),
          xlab="%GC", ylab="rel. cover",
          main="corrected", font.main=1, family="Times")
  abline(h=1, col=2)
  
  .cover.profile(model$optim$test$pos,
                 model$optim$test$cov/median(model$optim$test$cov, na.rm=T),
                 model=model, ylab="rel. cover", main="raw")
  
  .cover.profile(model$optim$test$pos,
                 model$optim$test$cov/model$optim$test$pred,
                 model=model, ylab="rel. cover", main="corrected")
  par(opar)
  
  tex <- tex.fig.off(tex)
  #
  # fig off
  
  invisible(tex)
}
