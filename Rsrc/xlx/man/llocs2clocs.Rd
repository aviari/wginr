% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xlx.coords.r
\name{llocs2clocs}
\alias{llocs2clocs}
\title{convert llocations to clocations}
\usage{
llocs2clocs(llocations)
}
\arguments{
\item{llocations}{named list of mx2 relative locations per chromosome (see note)}
}
\value{
nx3 matrix of relative clocations (1-based)
}
\description{
convert a named list of mx2 locations to a nx3 matrix of clocations.
(see \link{HELP.COORD} for help on coordinates systems)
}
\note{
llocations must be named by chromosome indexes (as character)
}
\examples{
fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
clocs <- coords2clocs(fh, 1:25)
llocs <- clocs2llocs(clocs)
rclocs <- llocs2clocs(llocs)
identical(clocs, rclocs)
basta.close(fh)

}
