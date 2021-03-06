% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.statistics.r
\name{lx.prm.binom}
\alias{lx.prm.binom}
\alias{lx.dbinom}
\alias{lx.pbinom}
\alias{lx.rbinom}
\title{Re-parameterized Binomial distribution}
\usage{
lx.prm.binom(mean, sd)

lx.dbinom(x, mean, sd, ...)

lx.pbinom(x, mean, sd, ...)

lx.rbinom(n, mean, sd, ...)
}
\arguments{
\item{mean}{vector of means}

\item{sd}{vector of standard deviations}

\item{x}{vector of quantiles}

\item{...}{other parameters to \link{dbinom}, \link{pbinom} and \link{rbinom}}

\item{n}{number of observations}
}
\value{
\bold{lx.prm.binom} returns the standard parameters
of R library functions.
\bold{lx.dbinom} gives the density, \bold{lx.pbinom} gives the distribution function
and \bold{lx.rbinom} generates random deviates.
}
\description{
Binomial  distibution reparameterized with mean and variance
}
\section{Functions}{
\itemize{
\item \code{lx.dbinom}: density function

\item \code{lx.pbinom}: distribution function

\item \code{lx.rbinom}: random deviate
}}

\seealso{
\link{dbinom}, \link{pbinom} and \link{rbinom}
}
