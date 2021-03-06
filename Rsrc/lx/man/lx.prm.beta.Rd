% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.statistics.r
\name{lx.prm.beta}
\alias{lx.prm.beta}
\alias{lx.dbeta}
\alias{lx.pbeta}
\alias{lx.rbeta}
\title{Re-parameterized Beta distribution}
\usage{
lx.prm.beta(mean, sd)

lx.dbeta(x, mean, sd, ...)

lx.pbeta(x, mean, sd, ...)

lx.rbeta(n, mean, sd, ...)
}
\arguments{
\item{mean}{vector of means}

\item{sd}{vector of standard deviations}

\item{x}{vector of quantiles}

\item{...}{other parameters to \link{dbeta}, \link{pbeta} and \link{rbeta}}

\item{n}{number of observations}
}
\value{
\bold{lx.prm.beta} returns the standard parameters
of R library functions.
\bold{lx.dbeta} gives the density, \bold{lx.pbeta} gives the distribution function
and \bold{lx.rbeta} generates random deviates.
}
\description{
Beta distibution reparameterized with mean and variance
}
\section{Functions}{
\itemize{
\item \code{lx.dbeta}: density function

\item \code{lx.pbeta}: distribution function

\item \code{lx.rbeta}: random deviate
}}

\seealso{
\link{dbeta}, \link{pbeta} and \link{rbeta}
}
