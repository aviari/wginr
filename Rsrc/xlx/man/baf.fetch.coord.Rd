% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xlx.bbaf.r
\name{baf.fetch.coord}
\alias{baf.fetch.coord}
\title{fetch allele counts}
\usage{
baf.fetch.coord(handle, coord)
}
\arguments{
\item{handle}{file handle (as returned by \link{baf.open})}

\item{coord}{absolute sequence coordinates (c(absfrom, absto)) (1-based)
or a single position absfrom (this implies absto=absfrom)}
}
\value{
integer matrix of size n x 4 containing allele counts.
}
\description{
fetch allele counts using absolute coordinates.\cr
\code{coord} defines a region on a chromosome. this function
returns a count matrix (with 4 columns) of
the number of symbols at each position within the region.
}
\note{
see \link{HELP.COORD} for help on coordinates systems
}
\examples{
baf <- baf.open(lx.system.file("samples/test.baf", "xlx"))
baf.fetch.coord(baf, c(33725, 33732))
i <- baf.name2index(baf, "machaon")
baf.fetch.cloc(baf, c(i, 560, 567))
baf.close(baf)

}
\seealso{
\link{baf.fetch.cloc}, \link{baf.bin.coord},
\link{baf.fetch.points.chr}, \link{baf.heterozygous.coord}
}
