% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xlx.bed.r
\name{bed.read}
\alias{bed.read}
\title{read bed regions from file}
\usage{
bed.read(filename)
}
\arguments{
\item{filename}{bed file name}
}
\value{
nx3 dataframe with colnames: "chr" (character) "from" (integer), "to" (integer)
}
\description{
read file in bed (0-based) format and return a dataframe
}
\note{
\code{from, to} coordinates are 0-based
}
\examples{
bed <- bed.read(lx.system.file("samples/test.bed", "xlx"))
fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
clocs <- bed2clocs(fh, bed)
basta.close(fh)

}
\seealso{
\link{bed2clocs}
}
