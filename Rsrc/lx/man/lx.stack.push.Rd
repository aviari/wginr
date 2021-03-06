% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.stack.r
\name{lx.stack.push}
\alias{lx.stack.push}
\title{push value into stack}
\usage{
lx.stack.push(stk, value)
}
\arguments{
\item{stk}{Stack}

\item{value}{any R object (including NULL)}
}
\value{
modified stack
}
\description{
push value into stack
}
\note{
this implementation uses lists and is not efficient.
this is mostly intended for storing small number of elements.
if you need a better implementation, use the 'rstack' library
}
\examples{
stk <- lx.stack.new()
stk <- lx.stack.push(stk, 1)
stk <- lx.stack.push(stk, "two")
stk <- lx.stack.push(stk, NULL)
stk <- lx.stack.push(stk, 1:4)
while (! lx.stack.is.empty(stk)) {
 stk <- lx.stack.pop(stk)
 val <- lx.stack.value(stk)
 lx.out(typeof(val), ": ", val)
}
}
