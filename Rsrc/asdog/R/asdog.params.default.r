# -------------------------------------------------
# $Id: asdog.params.default.r 498 2019-03-11 19:40:36Z viari $
# Asdog: Copy Number Analysis for WGS data
#
# asdog default parameters
#

# -------------------------------------------------
# default parameters
#
# note: seed, verbose, use.threads and mc.cores
#       are handled directly from args (in function 'asdog.main.args')
#

asdog.default.params <- function() {

  .def <- function(category, info, type, value, required=FALSE) {
    list(category=category, info=info, type=type,
         value=value, required=required)
  }
  
  def <- list()
  class(def) <- c('ParamDefs', class(def))

  # -------------------------------------------------
  # general parameters
  # -------------------------------------------------

  def$base        <- .def("gccorrect:cnpredict:popmap:matchpair: general",
                          "pathname of BAF file (without extension)",
                          "character",
                          "none", TRUE)

  def$ref         <- .def("gccorrect: general",
                          "pathname of reference (BASTA) file (without extension)",
                          "character",
                          "none", TRUE)

  def$normal      <- .def("cnpredict: general",
                          "pathname of normal BAF file (without extension)",
                          "character",
                          "none", TRUE)
  
  def$ref.snp     <- .def("popmap:matchpair: general",
                          "pathname of reference SNPs file (default to internal)",
                          "character",
                          "sys:asdog:data/HapMap.hg19.snp.100K.rds")

  def$match      <- .def("matchpair: general",
                         "pathname of matched BAF file (without extension)",
                         "character",
                         "none", TRUE)

  def$outdir     <- .def("gccorrect:cnpredict: general",
                         "output directory",
                         "character",
                         ".")
  
  def$chrs       <- .def("gccorrect:cnpredict:segment:popmap:matchpair: general",
                         "list of chromosomes indexes to consider (empty means all)",
                         "vector-of-integer",
                         1L:22L)

  def$seed        <- .def("gccorrect:cnpredict:popmap:matchpair: general",
                          "random seed (use < 0 for no seed)",
                          "integer",
                          0L)

  def$verbose     <- .def("*: general",
                          "verbosity level (one of 'debug', 'info', 'warning', 'error')",
                          "character",
                          "info")

  def$use.threads <- .def("*: general",
                          "use threads",
                          "logical",
                          TRUE)

  def$mc.cores    <- .def("*: general",
                          "nb cores to use (use 0 for maximum cores available)",
                          "integer",
                          0L)
  
  # -------------------------------------------------
  # parameters for gcmodel regions selection
  # -------------------------------------------------
    
  def$gcmodel.regions.name    <- .def("gccorrect: regions_selection",
                                      "name of user's regions bed file",
                                      "character",
                                      "@REF@.mappable.bed")

  def$gcmodel.regions.minreg  <- .def("gccorrect: regions_selection",
                                      "minimum size of regions in bp",
                                      "integer",
                                      10000L)

  def$gcmodel.regions.binsize <- .def("gccorrect: regions_selection",
                                      "size for binning regions coverage",
                                      "integer",
                                      10000L)
  
  def$gcmodel.regions.joinsize <- .def("gccorrect: regions_selection",
                                      "min and max join size for large windows",
                                      "vector-of-integer",
                                      c(1000L, 1e6L))
  
  def$gcmodel.regions.smooth <- .def("gccorrect: regions_selection",
                                      "median smoothing parameters",
                                      "vector-of-integer",
                                      c(3L, 5L, 15L, 35L, 55L))

  def$gcmodel.regions.win0 <- .def("gccorrect: regions_selection",
                                   "GC model0 window (set to 0 to disable model0)",
                                   "integer",
                                   1000L)

  def$gcmodel.regions.band0 <- .def("gccorrect: regions_selection",
                                   "GC model0 band selection",
                                   "character",
                                   "2 * poisson")

  def$gcmodel.regions.band <- .def("gccorrect: regions_selection",
                                    "coverage band selection model",
                                    "character",
                                    "1 * poisson")
  
  def$gcmodel.regions.plot.alpha <- .def("gccorrect:report: regions_selection",
                                         "region display extension alpha factor",
                                         "double",
                                         5)
  
  # -------------------------------------------------
  # parameters for gcmodel sampling
  # -------------------------------------------------
  
  def$gcmodel.sample.size    <- .def("gccorrect: sampling",
                                       "sample size",
                                       "integer",
                                       1000000L)

  def$gcmodel.sample.spacin  <- .def("gccorrect: sampling",
                                       "minimum distance between samples",
                                       "integer",
                                       0L)

  def$gcmodel.sample.gcbins  <- .def("gccorrect: sampling",
                                       "number of gc bins (use 0 to disable gc sampling, typical value 4)",
                                       "integer",
                                       5L)
  
  def$gcmodel.sample.gcwindow  <- .def("gccorrect: sampling",
                                         "gc sampling window size in bp (only if gcbins != 0)",
                                         "integer",
                                         1000L)

  # -------------------------------------------------
  # parameters for gam-loess gcmodel
  # -------------------------------------------------

  def$gcmodel.datasrc.grain      <- .def("gccorrect: data source",
                                         "datasource grain (in bp)",
                                         "integer",
                                         100L)
  
  def$gcmodel.datasrc.smooth     <- .def("gccorrect: data source",
                                         "datasource median smoothing (use 0 to disable smoothing)",
                                         "vector-of-integer",
                                         c(3L, 5L, 15L))

  def$gcmodel.small.winsize     <- .def("gccorrect: gc_window_optimization",
                                         "gc small windows size in bp (use 0 to disable small window)",
                                         "vector-of-integer",
                                         c(500L, 1000L, 2000L, 5000L))

  def$gcmodel.large.winsize     <- .def("gccorrect: gc_window_optimization",
                                         "gc large windows size in bp (use 0 to disable large window)",
                                         "vector-of-integer",
                                         c(1e5L, 2e5L, 5e5L, 1e6L, 2e6L, 5e6L))

  def$gcmodel.learn.ratio       <- .def("gccorrect: gc_window_optimization",
                                         "learn/test size ratio",
                                         "double",
                                         0.5)

  def$gcmodel.gloess.fit        <- .def("gccorrect: model definition",
                                        "fit function: gam or loess",
                                        "character",
                                        "gam")
  
  def$gcmodel.loess.span        <- .def("gccorrect: model definition",
                                         "loess span parameters",
                                         "vector-of-double",
                                         c(0.75, 0.75))
  
  def$gcmodel.base.quant         <- .def("gccorrect: model definition",
                                         "baseline quantile (use 0 to disable)",
                                         "double",
                                         0)
  
  def$gcmodel.base.trim          <- .def("gccorrect: model definition",
                                         "baseline trimming (use NULL to disable)",
                                         "vector-of-double",
                                         NULL)
  
  def$gcmodel.nband              <- .def("gccorrect: model definition",
                                         "min and max number of gc bands",
                                         "vector-of-integer",
                                         c(4L, 10L))
  
  def$gcmodel.optim.objective    <- .def("gccorrect: model optimization",
                                         "objective function (one of gam.rsq, pearson)",
                                         "character",
                                         "gam.rsq")
  
  # -------------------------------------------------
  # parameters for gc correction
  # -------------------------------------------------
  
  def$gccorrect.regions    <- .def("gccorrect: gc correction",
                                   "regions to predict",
                                   "character",
                                   "@REF@.mappable.bed")

  def$gccorrect.binsize    <- .def("gccorrect: gc correction",
                                   "binsize in bp",
                                   "integer",
                                   1000L)

  def$gccorrect.collect    <- .def("gccorrect: gc correction",
                                   "collect function (mean or median)",
                                   "character",
                                   "median")
  
  def$gccorrect.plot.rcmax  <- .def("gccorrect:report: gc correction",
                                    "rrc plot max value (use 0 to disable)",
                                    "double",
                                    4)

  def$gccorrect.plot.smooth <- .def("gccorrect:report: gc correction",
                                    "rrc plot median smoothing parameters",
                                    "vector-of-integer",
                                    c(3L, 5L, 15L, 35L, 55L))
  
  def$gccorrect.plot.nbpts  <- .def("gccorrect:report: gc correction",
                                    "number of plot points",
                                    "integer",
                                    5000L)
  
  def$gccorrect.plot.cex   <- .def("gccorrect:report: gc correction",
                                   "rrc plot point size",
                                   "double",
                                   0.1)
  
  # -------------------------------------------------
  # parameters for popmap, matchpair
  # -------------------------------------------------
  
  def$snp.pop.low     <- .def("popmap:matchpair: snp selection",
                              "population alt/ref allele frequency lower threshold",
                              "double",
                              0.01)

  def$snp.lowread   <- .def("popmap:matchpair: snp selection",
                            "maximum number of reads to be considered as 0",
                            "integer",
                            2L)
  
  def$snp.mincov     <- .def("popmap:matchpair: snp selection",
                             "minimum coverage (use negative value for quantile/1000)",
                             "integer",
                             10L)

  def$snp.deltafreq  <- .def("popmap: snp selection",
                             "maximum delta allelic frequency around 0.5",
                             "double",
                             0.3)

  def$snp.sample.size <- .def("popmap:matchpair: snp selection",
                              "initial SNPs sample size (use 0 to keep all SNPs)",
                              "integer",
                              10000L)

  def$snp.match.nbest <- .def("matchpair: snp selection",
                              "number of most segregating SNPs",
                              "integer",
                              200L)

  def$snp.match.refpop <- .def("matchpair: snp selection",
                              "reference population ('AF', 'EUR', 'AMR', 'AFR', 'EAS', 'SAS')",
                              "character",
                              "AF")
  
  # -------------------------------------------------
  # parameters for ploidy model
  # -------------------------------------------------

  def$plmodel.regions <- .def("cnpredict: ploidy model",
                              "user's regions (set to NULL to ignore)",
                              "character",
                              "@REF@.mappable.bed")
  
  def$plmodel.snp.lowread <- .def("cnpredict: ploidy model",
                                  "maximum number of reads to be considered as 0",
                                  "integer",
                                  2L)

  def$plmodel.snp.mincov  <- .def("cnpredict: ploidy model",
                                  "minimum coverage (use negative value for quantile/1000)",
                                  "integer",
                                  10L)
  
  def$plmodel.snp.dfreq   <- .def("cnpredict: ploidy model",
                                  "maximum delta allelic frequency around 0.5",
                                  "double",
                                  0.1)

  # pre-segmentation : general
  
  def$plmodel.preseg.mode <- .def("cnpredict: ploidy model",
                                  "pre-segmentation mode (shmm or bcp)",
                                  "character",
                                  "bcp")
  
  def$plmodel.preseg.rcmax <- .def("cnpredict:shmm: ploidy model",
                                   "pre-segmentation maximum rrc",
                                   "double",
                                   4)
  
  def$plmodel.preseg.sampsize <- .def("cnpredict:shmm: ploidy model",
                                      "pre-segmentation rc and af downsampling (0 to disable)",
                                      "integer",
                                      1e5L)
  
  def$plmodel.preseg.segsize  <- .def("cnpredict:shmm: ploidy model",
                                      "rc and af minimum seg size in Mb for smooth.hmm",
                                      "integer",
                                      10L)
  
  # pre-segmentation : smooth hmm
  
  def$plmodel.shmm.rc.raw  <- .def("cnpredict:shmm: ploidy model",
                                   "use raw (TRUE) or smooth (FALSE) rrc for smooth.hmm",
                                   "logical",
                                   TRUE)

  def$plmodel.shmm.rc.smooth <- .def("cnpredict:shmm: ploidy model",
                                     "rrc smoothing parameters for smooth.hmm",
                                     "vector-of-integer",
                                     c(3L, 5L, 15L, 35L, 55L, 105L))

  def$plmodel.shmm.rc.eps    <- .def("cnpredict:shmm: ploidy model",
                                     "rrc maxima eps value for smooth.hmm",
                                     "double",
                                     0.1)

  def$plmodel.shmm.rc.maxlev <- .def("cnpredict:shmm: ploidy model",
                                    "maximum number of rrc levels for smooth.hmm",
                                    "integer",
                                    10L)
  
  def$plmodel.shmm.rc.tau    <- .def("cnpredict:shmm: ploidy model",
                                     "rrc smooth.hmm transition probability",
                                     "double",
                                     1e-50)

  def$plmodel.shmm.af.raw  <- .def("cnpredict:shmm: ploidy model",
                                   "use raw (TRUE) or smooth (FALSE) af for smooth.hmm",
                                   "logical",
                                   FALSE)
  
  def$plmodel.shmm.af.smooth <- .def("cnpredict:shmm: ploidy model",
                                     "af smoothing parameters for smooth.hmm",
                                     "vector-of-integer",
                                     c(3L, 5L, 15L, 35L, 55L, 105L))
  
  def$plmodel.shmm.af.eps    <- .def("cnpredict:shmm: ploidy model",
                                     "af maxima eps value for smooth.hmm",
                                     "double",
                                     0.1)
  
  def$plmodel.shmm.af.maxlev <- .def("cnpredict:shmm: ploidy model",
                                     "maximum number of af levels for smooth.hmm",
                                     "integer",
                                     10L)
  
  def$plmodel.shmm.af.tau    <- .def("cnpredict:shmm: ploidy model",
                                     "af smooth.hmm transition probability",
                                     "double",
                                     1e-50)
  
  def$plmodel.shmm.sampsize <- .def("cnpredict:shmm: ploidy model",
                                     "rc and af smooth.hmm downsampling (0 to disable)",
                                     "integer",
                                     1e5L)
  
  def$plmodel.shmm.segsize  <- .def("cnpredict:shmm: ploidy model",
                                    "rc and af minimum seg size in Mb for smooth.hmm",
                                    "integer",
                                    10L)
  
  # pre-segmentation : bcp
  
  def$plmodel.bcp.rc.smooth <- .def("cnpredict:bcp: ploidy model",
                                     "rrc smoothing parameters for bcp",
                                     "vector-of-integer",
                                     NULL)
  
  def$plmodel.bcp.af.smooth <- .def("cnpredict:bcp: ploidy model",
                                    "baf smoothing parameters for bcp",
                                    "vector-of-integer",
                                    NULL)

  def$plmodel.bcp.p0 <- .def("cnpredict:bcp: ploidy model",
                             "bcp p0 parameter",
                             "double",
                             1e-50)
  
  # ploidy fit

  def$plmodel.fit.weightquant  <- .def("cnpredict: ploidy model",
                                    "minimum weight quantile for fit (0 to disable)",
                                    "double",
                                    0.25)
  
  def$plmodel.fit.segsrc    <- .def("cnpredict: ploidy model",
                              "segment source for fit (mean|hmm.rc|af)",
                              "vector-of-character",
                              c("mean.rc", "mean.af"))

  def$plmodel.fit.arange  <- .def("cnpredict: ploidy model",
                                    "plmodel fit alpha range and step",
                                    "vector-of-double",
                                    c(0, 1, 0.05))
  
  def$plmodel.fit.qrange  <- .def("cnpredict: ploidy model",
                                  "plmodel fit Q range and step",
                                  "vector-of-double",
                                  c(1, 8, 0.05))

  def$plmodel.fit.thweight  <- .def("cnpredict: ploidy model",
                                  "plmodel fit use theo weights",
                                  "logical",
                                  FALSE)
  
  def$plmodel.fit.gamma     <- .def("cnpredict: ploidy model",
                                    "plmodel fit gamma penalty",
                                    "double",
                                    1)

  def$plmodel.fit.rho      <- .def("cnpredict: ploidy model",
                                    "plmodel fit score ratio power",
                                    "double",
                                    0.5)

  def$plmodel.alpha.normal <- .def("cnpredict: ploidy model",
                                   "max alpha tolerated for a tumour",
                                   "double",
                                   0.95)

  def$plmodel.plot.rcmax   <- .def("cnpredict:report: ploidy model",
                                    "rrc plot max value (use 0 to disable)",
                                    "double",
                                    4)

  # -------------------------------------------------
  # parameters for segmentation
  # -------------------------------------------------
  
  
  def$segment.alpha  <- .def("segment: HMM_segmentation",
                             "contamination value (set to NA to use ploidy model)",
                             "double",
                             NA)
  
  def$segment.ploidy  <- .def("segment: HMM_segmentation",
                              "ref. ploidy value (set to NA to use ploidy model)",
                              "double",
                              NA)
  
  def$segment.tau      <- .def("segment: HMM_segmentation",
                               "HMM transition probability",
                               "double",
                               1e-50)
  
  def$segment.cnmax    <- .def("segment: HMM_segmentation",
                               "max CN level",
                               "double",
                               30)
  
  def$segment.joinsize    <- .def("segment: HMM_segmentation",
                                  "max interval distance in bp for segments fusion",
                                  "integer",
                                  1e5L)
  
  def$segment.minsize     <- .def("segment: HMM_segmentation",
                                  "min segment size in bp. (should be >= gccorrect.binsize)",
                                  "integer",
                                  1000L)
  
  def$segment.use.rcaf    <- .def("segment: HMM_segmentation",
                                   "use rc and af (TRUE) or rc only (FALSE) for segmentation",
                                   "logical",
                                   TRUE)

  def$segment.plot.cnmax   <- .def("segment:report: ploidy model",
                                   "cn plot max value (use 0 to disable)",
                                   "double",
                                   10)
  
  def$segment.plot.rcmax   <- .def("segment:report: ploidy model",
                                   "rrc plot max value (use 0 to disable)",
                                   "double",
                                   4)

  # -------------------------------------------------
  # parameters for reporting
  # -------------------------------------------------
 
  def$report.latex.driver <- .def("gcmodel:rcpredict:segment:report:",
                                 "latex graphics driver ('pdf' | 'jpeg')",
                                 "character",
                                 "jpeg")
  
  def$report.driver.res   <- .def("gcmodel:rcpredict:segment:report:",
                                  "resolution for jpeg | tiff drivers",
                                  "integer",
                                  300L)

  def
}
