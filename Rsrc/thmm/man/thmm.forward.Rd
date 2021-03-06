% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/thmm.forback.r
\name{thmm.forward}
\alias{thmm.forward}
\title{(log) forward probabilities}
\usage{
thmm.forward(hmm, obs, .useC = TRUE)
}
\arguments{
\item{hmm}{DTHmm model (from \link{thmm.init})}

\item{obs}{numerical vector of observations}

\item{.useC}{logical if TRUE (default) use C code else use R code}
}
\value{
list with two components: \code{logalpha} and \code{loglike}
\itemize{
\item\code{logalpha} : the (log of) forward probabilities (densities) matrix:\cr
\code{alpha_{i,j} = Pr{ X_1 = obs_1, ..., X_i = obs_i, state_i = j | hmm}}
that is the probability (density) of seeing the partial sequence
\code{(obs_1, ..., obs_i)} and ending up in state j at time i for this
hmm.

\item\code{loglike} : the log-likelihood of this observation with this hmm:\cr
\code{log(Pr{ X_1 = obs_1, ..., X_n = obs_n | hmm})}=\code{max_j(logalpha_{n,j}}
}
}
\description{
compute forward probabilities of observations
according to a discrete time hidden markov model (DTHmm).
}
\note{
the code takes care of rescaling values during calculation
to avoid underflow problems and is quite robust in practice.

since the computation uses density probabilities it is perfectly
valid to get a positive log-likelihood.

for multivariate HMM, \code{obs} should be a list of (numerical)
tuples. (see \link{thmm} examples).
}
\examples{
hmm <- thmm.init(dnorm, 0.1, mean=c(-1,1), sd=0.1)
obs <- c(rnorm(100, -1, 0.1), rnorm(100, 1, 0.1))
fwd <- thmm.forward(hmm, obs)
#
# test numerical stability
#
dif1 <- fwd$logalpha[-100,1] - fwd$logalpha[-100,2]
obs[100] <- 1000000  # this may creates overflow
fwd <- thmm.forward(hmm, obs)
dif2 <- fwd$logalpha[-100,1] - fwd$logalpha[-100,2]
max(abs((dif1-dif2)/dif1))
\dontrun{
plot(dif1)
points(dif2, col=2, cex=0.1)}
}
\seealso{
\link{thmm.backward}, \link{thmm.forward.backward}
}
