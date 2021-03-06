% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xlx.bbaf.r
\name{baf.heterozygous}
\alias{baf.heterozygous}
\title{get heterozygous positions on all chromosomes}
\usage{
baf.heterozygous(handle, chrs = NULL, lowread = 2L, mincov = 10L,
  deltafreq = 0.1, flatten = TRUE, .chunk.size = NA,
  .sample.size = 1000000L, use.threads = lx.use.threads())
}
\arguments{
\item{handle}{file handle (as returned by \link{baf.open})}

\item{chrs}{integer (chromosome indexes) or character (chromosome names) vector
specifying which chromosomes to use. Use NULL to specify all
chromosome declared in baf header.}

\item{lowread}{low read threshold (see \link{baf.count.filter})}

\item{mincov}{minimal coverage (see \link{baf.count.filter})}

\item{deltafreq}{max allele frequency (see \link{baf.count.filter})}

\item{flatten}{if TRUE flatten all chromosomes within a single matrix
else return a list of such matrices, one per chromosome}

\item{.chunk.size}{chunk size for loading chromosomes. see \link{baf.heterozygous.chr}}

\item{.sample.size}{sample size to determine mincov for the case where mincov < 0 
(see note below).}

\item{use.threads}{(see \link{lx.use.threads})}
}
\value{
(list of) integer matrix of size n x 4 containing allele counts at heterozygous sites.
if flatten is TRUE matrix rownames are of the form: chr.position (1-based)
else position (1 based) only.
}
\description{
get heterozygous positions on all chromosomes
this function gather all heterozygous positions
defined as valid by \link{baf.count.filter}:\cr
\itemize{
\item coverage >= \code{covmin}
\item nb_alleles == 2
\item abs(0.5 - count_max_allele / coverage) <= \code{deltafreq}
}
}
\note{
if (mincov < 0) then mincov will be estimated 
(as \code{quantile(coverage, -mincov/1000)}) as in \link{baf.count.filter}.
However the region for computing coverage will depends upon the \code{.chunk.size}
parameter: if \code{.chunk.size == NA} then this will be performed on each chromosome
separately (therefore leading to potential different values of mincov per chromosome).
if \code{.chunk.size != NA} then mincov will be first evaluated
on a sample of \code{.sample.size} data points. Therefore for exact results, you
better use a positive value for mincov.

see \link{HELP.COORD} for help on coordinates systems
}
\examples{
baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
baf.heterozygous(baf, NULL, deltafreq=0.5)
baf.heterozygous(baf, NULL, deltafreq=0.5, flatten=FALSE)
baf.close(baf)

}
\seealso{
\link{baf.heterozygous.chr}
}
