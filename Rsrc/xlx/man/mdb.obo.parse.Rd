% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mdb.obo.r
\name{mdb.obo.parse}
\alias{mdb.obo.parse}
\title{obo main parsing driver (internal use)}
\usage{
mdb.obo.parse(key, rec)
}
\arguments{
\item{key}{key to parse (currently AC,OC,KW,DR,seq)}

\item{rec}{record to process}
}
\description{
obo main parsing driver (internal use)
}
\note{
call function \code{.mdb.obo.parse.<key> if it exists}
}
