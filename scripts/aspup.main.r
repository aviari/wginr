#!/usr/bin/env Rscript
#
# aspup main script
#
#

# -------------------------------------------------
# compute % GC of string
#
pergc <- function(s, offset=0, winsize=nchar(s)) {
  s <- substr(s, start=offset, stop=offset+winsize)
  length(lx.strchr(s, "GC"))/nchar(s)
}

# -------------------------------------------------
# gam GC model
# (simplified version of asdog GC model)
#
gam.model <- function(gc, cov, N=100,
                      base.quant=0.01,
                      trim.quant=c(0.02, 0.95)) {

  mod <- list(gc=gc, cov=cov, mcov=median(cov), N=N)

  # aggregate
  mod$mean <- aggregate(cov, list(gc=round(gc*N)), FUN=median)
  colnames(mod$mean) <- c("gc", "cov")

  # baseline adjustment
  #
  baseline <- if (is.na(base.quant)) 1 else quantile(cov, base.quant, na.rm=T)
  cov <- c(baseline, baseline, mod$mean$cov)
  gc  <- c(0, N, mod$mean$gc)
  
  # make gam model
  #
  mod$gam <- gam(cov ~ s(gc))

  # prediction vector 
  #
  mod$pvec <- predict(mod$gam, list(gc=seq.int(0, N)))
  mod$pvec[mod$pvec <= baseline] <- baseline
  
  # trim low and high gc
  #
  if (! is.null(trim.quant)) {
    trim <- quantile(mod$gc, trim.quant, na.rm=T)
    bnds <- round(trim*N)
    mod$pvec[1:bnds[1]] <- mod$pvec[bnds[1]]
    mod$pvec[bnds[2]:length(mod$pvec)] <- mod$pvec[bnds[2]]
  }
  
  # remove outliers (may occur if span is too small)
  #
  #   outl <- 5*median(mod$pvec, na.rm=T)
  #   mod$pvec[mod$pvec >= outl] <- outl
  
  mod
}

# -------------------------------------------------
# gam GC model predictor
#
gam.predict <- function(mod, gc) as.vector(mod$pvec[round(gc*mod$N)+1])

# -------------------------------------------------
# log2 ratio
#
log.ratio <- function(x, y, EPS=1e-6) {
  x[abs(x)<=EPS] <- EPS
  y[abs(y)<=EPS] <- EPS
  log(x/y, base=2)
}

# -------------------------------------------------
# local plot : plot GC gam model
#
plot.gcmodel <- function(mod, xlim=c(0.2,0.8), ylim=c(0,2), ...) {
  
  lx.plot(mod$gc, mod$cov/mod$mcov,
          xlim=xlim,
          ylim=ylim,
          xlab="%GC", ylab="rel. cov.",
          ...)
  
  points(mod$mean$gc/mod$N, mod$mean$cov/mod$mcov, pch=19, cex=0.8, col=4)
  points(mod$mean$gc/mod$N, mod$mean$cov/mod$mcov, pch=1, cex=0.8, col=7)
  lines(seq.int(0, 1, 1/mod$N), mod$pvec/mod$mcov, col=2, lwd=3)
  abline(h=1, col=3)
  abline(v=mean(mod$gc), col=3)

  invisible()
}

# -------------------------------------------------
# local plot : cn profile
#
plot.profile <- function(x, y, fun=plot, chrs=NULL, hline=1,
                         ylim=NULL, cex=0.05, alpha=0.2,
                         xlab="pos", ylab="Y", ...) {
  
  if (missing(y)) {
    y <- x
    x <- seq_along(y)
  }
  
  if (is.null(ylim)) {
    ylim <- quantile(y, c(0.01, 0.99), na.rm=T)
  }
  
  below <- y<=ylim[1]
  above <- y>=ylim[2]
  y[below] <- ylim[1]
  y[above] <- ylim[2]
  
  col <- lx.color.alpha(ifelse(above|below, 6, 1), alpha)
  
  fun(x, y, ylim=ylim, cex=cex, 
       xlab=xlab, ylab=ylab,
       col=col, ...)

  abline(h=hline, col=4)
  
  if (! is.null(chrs)) {
    rle <- rle(chrs)
    gx <- x[cumsum(rle$lengths)]
    abline(v=gx, col=4)
    text(gx, ylim[2], labels=rle$values, cex=0.5, pos=2, off=0.1, col=4)
  }
  
  invisible()
}

# -------------------------------------------------
# get segment statistics
#
stat.segments <- function(seg, pos, data, what=c("rc", "af")) {
  
  what <- match.arg(what)
  
  if (what == "af") data <- abs(data-0.5)+0.5 # fold af
  
  res <- lapply(seq_len(nrow(seg)), function(i) {
    row   <- seg[i,] # 'apply' sucks
    ifrom <- lx.findInterval(row$from, pos)
    ito   <- lx.findInterval(row$to, pos)
    ilen  <- ito - ifrom + 1
    seg.data <- as.vector(data[seq.int(ifrom, ito)])
    med  <- median(seg.data)
    sd   <- if (ilen > 1) sd(seg.data) else 0
    mad  <- mad(seg.data)
    if (mad == 0) mad <- sd
    if (what == "af") {
      cor <- asdog.unfold(med, sd, type="median")$solution
      med <- cor$mu
      mad <- cor$sd
    }
    ref  <- row[, what]
    zscor <- if (med == ref) 0 else (med - ref)/mad
    pval  <- 2*pnorm(-abs(zscor)) # two-sided
    list(nb.mark=ilen, med=med, mad=mad, zscor=zscor, pval=pval)
  })
  
  as.data.frame(apply(do.call(rbind, res), 2, unlist))
}


# =========================
# main
# =========================

if (FALSE) {

library(asdog)

#
# parameters (should be handled by asdog.parameters)
#
  
PCNV.VERSION = "1.0"        # version number
COV.FUN = "median"          # cover collect function
COV.MIN = 5                 # minimum reads for normal cover
COV.QUANT = c(0.02, 0.98)   # extreme cover quantiles
COF.QUANT = 0.95            # var. coeff quantile
#GC.QUANT = c(0.005, 0.995)  # trim quantiles for GC model
GC.QUANT = NULL             # trim quantiles for GC model
RATIO.EPS  = 1e-6           # epsilon value for ratio
SNP.SOURCE = "hapmap"       # source of SNPs (paired or hapmap)
BAF.THRESH = 0.8            # lower threshold for BAF filtering
BAF.SD = 4                  # sd factor for BAF filtering
RRC.MAX = 4                 # rrc max
MIN.SEGSIZE = 1e6           # minimum segment size (in b)
BCP.P0 = 1e-10              # BCP proba
WEIGHT.QUANT = 0.25         # lower quantile for segment weights
RANGE.A = c(0, 1, 0.05)     # contamination range and step for fit
RANGE.Q = c(1, 8, 0.05)     # ploidy range and step for fit
FIT.GAMMA0 = 1              # fit gamma parameter
FIT.RHO = 0.5               # fit rho parameter
CN.MAX = 20                 # copy number max
HMM.TAU = 1e-50             # HMM transition proba


lx.options(use.threads=T)

#
# backbone Hapmap SNPs
#

SFIL = "data/backbone.hapmap.snp.rds"

#
# test data
#

# ok
tfil = "/TmpStorage/prof2data/P_01-2129_refined_alignement.recalibrated.data.rds"

# traines
# tfil = "/TmpStorage/prof2data/P_01-2162_refined_alignement.recalibrated.data.rds"

# multiple normal - ok
#
nfil = c("/TmpStorage/prof2data/P_01-2129_constit_refined_alignement.recalibrated.data.rds",
         "/TmpStorage/prof2data/P_01-2162_constit_refined_alignement.recalibrated.data.rds")

# run2
nfil = c("/TmpStorage/prof2data/Run2_Normal_6950_recalibrated_alignment_human_g1k_v37.data.rds",
         "/TmpStorage/prof2data/Run2_Normal_7471_recalibrated_alignment_human_g1k_v37.data.rds")
tfil = "/TmpStorage/prof2data/Run2_TumProf1_D170965_recalibrated_alignment_human_g1k_v37.data.rds"
tfil = "/TmpStorage/prof2data/Run2_TumProf2_D171171_recalibrated_alignment_human_g1k_v37.data.rds"


# ---------------------
# banner
#

lx.out("--------------------------------------------", with.mem=T)
lx.out("AsPup version ", PCNV.VERSION)
lx.out("--------------------------------------------")

# ---------------------
# read data
#

lx.out("reading ", length(nfil), " control data")

ndat <- lapply(nfil, function(f) {
          lx.out("reading control data: ", f)
          readRDS(f)
        })

lx.out("reading tumour data", tfil)
tdat <- readRDS(tfil)

# @todo: check sizes and NA

# FIXME
tdat$header$sizes <- sapply(tdat$header$seq, function(x) x$size)

# collect pos, len and GC

lx.out("collecting GC")

df <- data.frame(tdat$beds)

df$indx <- seq.int(nrow(df))

df$len <- df$to - df$from + 1

df$gc <- sapply(tdat$seq, pergc)

# collect cover

lx.out("collecting cover with: ", COV.FUN)

ncov <- lx.lapply(ndat, function(d) sapply(d$baf, function(x, fun) 
  fun(rowSums(x)), fun=get(COV.FUN)))

tcov <- sapply(tdat$baf, function(x, fun) fun(rowSums(x)), fun=get(COV.FUN))

# ---------------------
# filter low and high counts
#

refmean <- mean(sapply(ncov, mean))

mcov  <- lapply(ncov, function(x) x*refmean/mean(x))
mcov  <- rowMeans(do.call(cbind, mcov))
quant <- pmax(quantile(mcov, COV.QUANT), c(COV.MIN, 0))

nok  <- (mcov > quant[1]) & (mcov < quant[2])

lx.out("lowcount filter: ", sum(nok), " data points kepts on ", length(nok), 
       " (", round(sum(nok)*100/length(nok), 1), "%)")

ncov <- lapply(ncov, function(x) x[nok])
tcov <- tcov[nok]
df <- df[nok,]

# ---------------------
# rescale by median => normalize by library depth
#

ncov <- lapply(ncov, function(x) x / median(x))
tcov <- tcov / median(tcov)

# ---------------------
# remove variable probes
#

if (length(ncov) > 1) {
  
  mcov <- do.call(cbind, ncov)
  mcof <- apply(mcov, 1, function(x) sd(x)/mean(x))
  nok <- mcof <= quantile(mcof, COF.QUANT)
  
  lx.out("ref var. coef. filter: ", sum(nok), " data points kepts on ", length(nok), 
         " (", round(sum(nok)*100/length(nok), 1), "%)")

  ncov <- lapply(ncov, function(x) x[nok])
  tcov <- tcov[nok]
  df <- df[nok,]
}

# ---------------------
# mean of reference
#

mcov <- rowMeans(do.call(cbind, ncov))

# ---------------------
# probe length correction
#

mod.len <- lm(mcov ~ df$len)
cor.len <- predict(mod.len)

ncov <- lapply(ncov, function(x) x / cor.len)
mcov <- mcov / cor.len
tcov <- tcov / cor.len

# ---------------------
# GC correction
#

nmod.gc <- gam.model(df$gc, mcov, trim.quant=GC.QUANT)

ncor.gc <- gam.predict(nmod.gc, df$gc)

tmod.gc <- gam.model(df$gc, tcov, trim.quant=GC.QUANT)

tcor.gc <- gam.predict(tmod.gc, df$gc)

ncor.gc[ncor.gc < RATIO.EPS] <- RATIO.EPS
tcor.gc[tcor.gc < RATIO.EPS] <- RATIO.EPS

ncov <- lapply(ncov, function(x) x / ncor.gc)
mcov <- mcov / ncor.gc
tcov <- tcov / tcor.gc

# ---------------------
# final normalisation - center on median
#

ncov <- lapply(ncov, function(x) x / median(x))
mcov <- mcov / median(mcov)
tcov <- tcov / median(tcov)

# ---------------------
# get BAF
#

if (SNP.SOURCE == "hapmap") {
  
  sfil <- SFIL
  
  if (! file.exists(sfil)) {
    
    lx.out("recovering hapmap SNPs")
    
    # get Hapmap SNP in probes
    
    snp <- readRDS(lx.system.file("data/HapMap.hg19.snp.full.rds", package="asdog"))
    chr <- basta.name2index(tdat, as.character(snp$CHROM))
    clocs.snp <- clocations(cbind(chr, snp$POS, snp$POS))
    coords.snp <- clocs2coords(tdat, clocs.snp)
    int.snp <- intervals::Intervals(coords.snp)
    
    coords.bed <- clocs2coords(tdat, tdat$beds)
    int.bed <- intervals::Intervals(coords.bed)
    
    int.inter <- intervals::interval_intersection(int.snp, int.bed)
    indx.inter <- findInterval(int.inter[,1], int.snp[,1])
    jndx.inter <- (findInterval(int.inter[,1], as.vector(t(int.bed)))+1)/2
    
    snp.probes <- snp[indx.inter,]
    snp.probes$PROBE <- jndx.inter
    snp.probes$COORD <- coords.snp[indx.inter,1]
    
    lx.out("saving hapmap SNPs in ", sfil)
    
    saveRDS(snp.probes, sfil)
  }
  
  lx.out("recovering SNPs")
  
  snp <- readRDS(sfil)
  
} else {
  
  # get heterozygous positions in paired normal
  # paired normal is assumed to be at ndat[1]
  
  pndat <- ndat[[1]]

  coord.all <- clocs2coords(pndat, pndat$beds)
  coord <- unlist(apply(coord.all, 1, function(x) x[1]:x[2]), use.names=F)

  pnbaf <- do.call(rbind, pndat$baf)
  pnref <- strsplit(do.call(paste0, pndat$seq), "")[[1]]
  pnrow <- rowSums(pnbaf)
  pnraf <- lx.rowMaxs(pnbaf)/pnrow
  pnbok <- (! is.na(pnrow)) & (pnrow > 10) & (abs(pnraf-0.5) < 0.1)
  
  coord <- coord[pnbok]
  pnbaf <- pnbaf[pnbok,]
  pnref <- pnref[pnbok]
  
  pnalt <- pnbaf
  pnalt[cbind(seq_along(pnref), match(pnref, colnames(pnbaf)))] <- 0
  pnalt <- colnames(pnalt)[lx.rowMaxs(pnalt, what="index")]
  
  clocs <- coords2clocs(pndat, coord)
  iprob <- findInterval(coord, coord.all[,1])
  
  snp <- data.frame(CHROM=clocs[,1], POS=clocs[,2], ID="none",
                    REF=pnref, ALT=pnalt, AF=1.0, 
                    PROBE=iprob, COORD=coord)
}
  
# recover baf at SNP positions

coord <- clocs2coords(tdat, tdat$beds)
coord <- unlist(apply(coord, 1, function(x) x[1]:x[2]), use.names=F)
spos  <- findInterval(snp$COORD, coord)
sbaf  <- do.call(rbind, tdat$baf)[spos,]

indx <- cbind(seq_along(spos), match(snp$ALT, colnames(sbaf)))
tbaf <- sbaf[indx]/(rowSums(sbaf)+RATIO.EPS)

# remove homozygous positions

if (SNP.SOURCE == "hapmap") {
  thresh <- max(BAF.THRESH, 1 - BAF.SD * sd(tbaf[tbaf > BAF.THRESH]))
  bok <- abs(tbaf-0.5) < thresh/2
} else {
  bok <- rep(T, length(tbaf))
}

tbaf <- tbaf[bok]

# check :  lx.plot(tbaf)

# get probe copy number for each SNP

cpos <- snp$COORD[bok]
ipos <- match(snp$PROBE[bok], df$indx)
fok  <- ! is.na(ipos)
ipos <- ipos[fok]
cpos <- cpos[fok]

trrc <- tcov[ipos]/(mcov[ipos] + RATIO.EPS)

chrs <- coords2clocs(tdat, cpos)[,1]

dff <- data.frame(x=cpos, rrc=trrc, baf=tbaf[fok],  chrs=chrs)

# ---------------------
# make ploidy model
# 

segsize <- nrow(dff) * MIN.SEGSIZE / 3e9

# 
# pre-segmentation using BCP
#
ssegs <- asdog.bcp.segment(dff$rrc, dff$baf, 
                           rcmax=RRC.MAX,
                           smooth.rc=NULL, smooth.af=NULL,
                           p0=BCP.P0,
                           sample.size=nrow(dff),
                           minseg.size=segsize)$seg

#
# fit ploidy model
# - threshold segment size
# - fit model
#

obs <- ssegs[,1,drop=F]
obs$rc <- ssegs$mean.rc
obs$af <- ssegs$mean.af
obs$weight <- log10(ssegs$weight)
obs <- obs[,-1]

# threshold weights

quant <- quantile(obs$weight, WEIGHT.QUANT)
obs <- obs[obs$weight >= quant,,drop=F]

# fit ploidy model

lx.out("fitting ploidy model", with.mem=T)

plfit <- plmodel.fit(obs,
                   min.A=RANGE.A[1],
                   max.A=RANGE.A[2],
                   step.A=RANGE.A[3],
                   min.Q=RANGE.Q[1],
                   max.Q=RANGE.Q[2],
                   step.Q=RANGE.Q[3],
                   theo.weight=F,
                   gamma0=FIT.GAMMA0,
                   rho=FIT.RHO)

lx.out("best model alpha=", plfit$a0, " Q=", plfit$q0)

pltheo <- asdog.theo.RCAF(plfit$a0, plfit$q0, qrange=0:CN.MAX)

# ---------------------
# final HMM segmentation
#
# note: segment all chromosomes at once
#       not chr by chr as in asdog
#

max.theo.rc <- max(pltheo$rc)

.c <- function(x,y=x) complex(real=x, imaginary=y)

means <- .c(pltheo$rc, pltheo$af-0.5)
sd.rc <- lx.wt.mean(ssegs$sd.rc, ssegs$weight)
sd.af <- lx.wt.mean(ssegs$sd.af, ssegs$weight)
sds <- .c(sd.rc, sd.af)

hmm <- thmm.init(drcaf.asdog, HMM.TAU,
                 mean=means,
                 sd=sds)

rrc <- dff$rrc
rrc <- lx.smooth.median(dff$rrc, c(3,5,15)) #####
baf <- dff$baf
pos <- dff$x

rrc[rrc >= max.theo.rc] <- max.theo.rc

vit <- thmm.viterbi(hmm, .c(rrc, baf-0.5))

rle <- rle(vit$states)
ifrom <- head(c(0, cumsum(rle$lengths)) + 1, -1)
coords <- cbind(pos[ifrom], pos[ifrom+rle$lengths-1])

# we should cut at chrs boundaries... this is a pain in the neck...
intb <- intervals::Intervals(basta2coords(tdat))
intc <- intervals::Intervals(coords)
intr <- intervals::interval_intersection(intb, intc)
clocs <- coords2clocs(tdat, intr)
ipos <- findInterval(intr[,1], intc[,1])
vals <- rle$values[ipos[seq_len(nrow(intr))]]

# make final segments
fsegs <- data.frame(cfrom=intr[,1], cto=intr[,2], clocs)

fsegs$state <- vals
fsegs$rc <- pltheo$rc[vals]
fsegs$af <- pltheo$af[vals]
fsegs$ploidy <- pltheo$ploidy[vals]
fsegs$geno <- pltheo$label[vals]

# and add some statistics
stat.rc <- stat.segments(fsegs, dff$x, dff$rrc, "rc")
nb.mark <- stat.rc[,1]
stat.rc <- stat.rc[,-1]
colnames(stat.rc) <- paste(colnames(stat.rc), "rrc", sep=".")
stat.af <- stat.segments(fsegs, dff$x, dff$baf, "af")[,-1]
colnames(stat.af) <- paste(colnames(stat.af), "baf", sep=".")
ppm.mark <- nb.mark / (fsegs$to - fsegs$from + 1) * 1e6
fsegs <- cbind(fsegs, nb.mark=nb.mark, ppm.mark=ppm.mark, stat.rc, stat.af)


# ---------------------
# reporting
#

base <- lx.strsplit(basename(tfil), "\\.")[1]

#
# plots
#

asdog.tex.driver(list(report.latex.driver="jpeg", report.driver.res=300))

tex <- tex.open(base)

tex.section(tex, "GC Model")

# fig on
#
tex <- tex.fig.on(tex, width=7, height=4, family="Times")
par(mfrow=c(1,2))
plot.gcmodel(nmod.gc)
title("GC model Normal")
plot.gcmodel(tmod.gc)
title("GC model Tumor")
par(mfrow=c(1,1))
tex <- tex.fig.off(tex)
#
# fig off

tex.tag(tex, "newpage")
tex.section(tex, "Ploidy Fit")
tex.subsection(tex, "Alpha-Q fit")
# fig on
#
tex <- tex.fig.on(tex, width=4, height=4, family="Times")
asdog.plot.plmodel.fit(plfit)
tex <- tex.fig.off(tex)
#
# fig off

tex.subsection(tex, "CN-AF fit")
# fig on
#
tex <- tex.fig.on(tex, width=4, height=4, family="Times")
asdog.plot.plmodel.rcaf(obs, pltheo)
title(paste0("a=", round(plfit$a0, 2), 
             " Q=", round(plfit$q0, 2),
             " qual=", round(plfit$qual*100, 1), "%"))
tex <- tex.fig.off(tex)
#
# fig off


tex.tag(tex, "newpage")
tex.section(tex, "CN-AF Profiles")    
# fig on
#
tex <- tex.fig.on(tex, width=7, height=7, family="Times")
opar <- par(no.readonly=T)
par(mfrow=c(2, 1),
    mgp=c(2,1,0),
    mar=c(0, 4, 0, 2) + 0.1,
    oma=c(4, 0, 2, 0) + 0.1)

plot.profile(dff$x, dff$rrc, ylim=c(0, RRC.MAX), ylab="rrc", alpha=0, xaxt='n')
pal.qual <- viridis::viridis(11, alpha=0.5)
col.qual <- rev(pal.qual)[round(pmin(10, pmax(0, abs(fsegs$zscor.rrc)))) + 1]
segments(fsegs$cfrom, 0, fsegs$cto, 0, col=col.qual, lwd=1000, lend=1)
plot.profile(dff$x, dff$rrc, fun=points, ylim=c(0, RRC.MAX),
             chrs=dff$chrs, alpha=0.8, xaxt='n')
abline(h=unique(Re(thmm.parameters(hmm, "mean"))), lty=2, lwd=0.5, col=lx.GREY)
segments(dff$x[ssegs$pfrom], ssegs$mean.rc,
         dff$x[ssegs$pto], ssegs$mean.rc,
         col=lx.color.alpha(7, 0.5), lwd=5)
segments(fsegs$cfrom, fsegs$rc, fsegs$cto, fsegs$rc, col=2, lwd=3)
axis(4, pltheo$rc, pltheo$ploidy, las=2, cex.axis=0.5)


plot.profile(dff$x, dff$baf, ylim=c(0,1), ylab="af", alpha=0, hline=0.5)
col.qual <- rev(pal.qual)[round(pmin(10, pmax(0, abs(fsegs$zscor.baf)))) + 1]
segments(fsegs$cfrom, 0.5, fsegs$cto, 0.5, col=col.qual, lwd=1000, lend=1)
plot.profile(dff$x, dff$baf, fun=points, ylim=c(0,1), chrs=dff$chrs, hline=0.5, alpha=0.8)
#abline(h=unique(Im(thmm.parameters(hmm, "mean")))+0.5, lty=2, lwd=0.5)
segments(dff$x[ssegs$pfrom], ssegs$mean.af, 
         dff$x[ssegs$pto], ssegs$mean.af,
         col=lx.color.alpha(7, 0.5), lwd=5)
segments(fsegs$cfrom, fsegs$af, fsegs$cto, fsegs$af, col=2, lwd=3)
par(opar)
tex <- tex.fig.off(tex)
#
# fig off

tex <- tex.close(tex)

#
# segments
#

out <- fsegs[,-(1:2)]

typs <- sapply(out, typeof)
for (coln in setdiff(names(typs[typs=="double"]), c("nb.mark", "pval.rc", "pval.af"))) {
  out[,coln] <- round(out[,coln], 3)
}
for (coln in c("pval.rc", "pval.af")) {
  out[,coln] <- sprintf("%.2e", out[,coln])
}

write.table(out, paste0(base, ".segments.txt"), quote=F)

} 





