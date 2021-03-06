% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lx.tex.r
\name{tex.fig.on}
\alias{tex.fig.on}
\title{start new tex graphics}
\usage{
tex.fig.on(tex, ...)
}
\arguments{
\item{tex}{handle (as returned by \link{tex.open})}

\item{...}{any argument to graphics driver}
}
\value{
tex
}
\description{
start new tex graphics
}
\note{
it is important to reaffect tex handle upon return
since the internal components have been modified

this function opens the graphic driver defined in \code{lx.options("tex.graphics.driver")}
(default to "pdf") with options \code{lx.options("tex.driver.options")}.\cr
with "jpeg", "png" and "tiff" drivers, you may set the 'units' and 'res' options
in \code{lx.options("tex.driver.options")} (typical values are units="in" and res=300).
}
