% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.language.r
\name{lx.doc}
\alias{lx.doc}
\title{minimalist documentation about object}
\usage{
lx.doc(obj, depth = 2L, maxfields = 10L, .name = deparse(substitute(obj)),
  .prefix = ">", .indent = 0L)
}
\arguments{
\item{obj}{an R object}

\item{depth}{maximum depth of subfields recursion}

\item{maxfields}{maximum number of subfields}

\item{.name}{name to print in header (usually internal)}

\item{.prefix}{header prefix (usually internal)}

\item{.indent}{current indentation level (usually internal)}
}
\description{
return a minimalist description of an object
(including lx.info if available) and its
subfields (if any)
}
\examples{
x <- lx.new("people", name="alain", age=10, hobbies=c("scuba", "guitar"))
lx.info(x) <- "this is people class"
lx.info(x$age) <- "people age"
lx.doc(x)

}
\seealso{
\link{lx.info}
}
