% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xlx.dna.r
\name{as.Dna.character}
\alias{as.Dna.character}
\title{Coerce character string to Dna}
\usage{
\method{as.Dna}{character}(obj, ...)
}
\arguments{
\item{obj}{character string to coerce to Dna}

\item{...}{any argument to  \link{Dna}}
}
\description{
see \link{as.Dna}\cr
this is basically the same as the \link{Dna} constructor.
}
\examples{
as.Dna("acgtacgt", code="strict")

}
