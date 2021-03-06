% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.language.r
\name{lx.options}
\alias{lx.options}
\title{set or get lx options}
\usage{
lx.options(..., default = NULL)
}
\arguments{
\item{...}{arguments of the form \code{key} or \code{key=value} (see details)}

\item{default}{default value for retrieving a single key (see details)}
}
\value{
for set/get of \bold{single key}: returns the associated value
or (in get mode) \code{default} or \code{NULL} if option cannot be found
and has no default\cr
for set/get of \bold{multiple keys}: returns a named list of
key:value. (note: in get mode if the key is not found, the name is \code{<NA>} and
value \code{NULL})\cr
}
\description{
set or get lx options
}
\details{
\subsection{Set form}{
\code{lx.options(key=value [, key2=value, ...])}\cr
will set option key to value\cr
note: to remove a key then just set its value to NULL (\code{lx.options(key=NULL)}
}
\subsection{Get form}{
\code{lx.options(key, [key2, ... ] [, default=value])}\cr
return the value(s) associated to key(s)\cr
note that \code{key} is either a symbol or a string, see examples below.\cr
a special form for retrieving a single key with a default value is:\cr
\code{lx.options(key, default=value)}\cr
finally, if called without argument:\cr
\code{lx.options()}, then the function returns the list of all current
options.
}
\subsection{Returned value}{
for set/get of \bold{single key}: returns the associated value
or (in get mode) \code{default} or \code{NULL} if key cannot be found\cr
for set/get of \bold{multiple keys}: returns a named list of
key:value.\cr
in get mode, if the key is not found, then the corresponding
name is \code{<NA>} and value \code{NULL}\cr
}

note that set and get mode can be intermixed
(although this is a bit strange to do)\cr
}
\note{
as for \link{options}, key is a (uninterpreted) symbol or a character string.\cr
(for developpers: to use a variable value, use a \link{do.call})

see \link{HELP.LX.OPTIONS} for a list of current builtin options
}
\examples{
lx.options(foo=123, bar=234)
lx.options(foo)
lx.options("foo")
lx.options(foobar, default=456)
lx.options(foo=NULL)
lx.options(foo, default=pi)
lx.options()
}
