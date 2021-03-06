% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xlx.basta.r
\name{basta.name2index}
\alias{basta.name2index}
\title{convert seqname to seqindex}
\usage{
basta.name2index(handle, seqname, silent = FALSE)
}
\arguments{
\item{handle}{basta/baf file handle (as returned by \link{basta.open}
or \link{baf.open})}

\item{seqname}{character vector of sequence name(s)}

\item{silent}{logical keep silent if seqname(s) not found}
}
\value{
integer vector of 1-based sequence index or of 0 if seqname(s) not found
}
\description{
convert seqname to seqindex
}
\examples{
fh <- basta.open(lx.system.file("samples/test.bst", "xlx"))
basta.name2index(fh, "seq1")
basta.index2name(fh, 2)
basta.name2index(fh, c("seq1", "nothere"))
basta.index2name(fh, 1:3)
basta.close(fh)

}
\seealso{
\link{basta.index2name}
}
