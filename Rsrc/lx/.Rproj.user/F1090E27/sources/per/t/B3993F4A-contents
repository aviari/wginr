# -------------------------------------------------
# $Id: lx.globals.r 321 2017-08-18 11:10:19Z viari $
# lx global variables
#

# -------------------------------------------------
#' @name HELP.LX.OPTIONS
#' @docType data
#' @title lx options
#' @description
#' \subsection{Lx options}{
#' the following options can be accessed and modified by \link{lx.options}
#' \tabular{lll}{
#'   \bold{name} \tab \bold{descr.} \tab \bold{default}\cr
#'   verbose \tab be verbose \tab TRUE\cr
#'   use.threads \tab use multithreading \tab FALSE\cr
#'   mc.cores \tab number of cores to use \tab parallel::detectCores()\cr
#'   pg.verbose \tab \link{lx.lapply} use progress bar \tab TRUE\cr
#'   pg.options.style \tab progress bar style \tab 1\cr
#'   pg.options.time \tab add system.time in progress bar \tab TRUE\cr
#'   tex.dir \tab temporary dir for tex files \tab tmp\cr
#'   tex.docclass \tab class of tex document \tab report\cr
#'   tex.packages \tab latex packages \tab tmp\cr
#'   tex.graphics.driver \tab tex graphics driver \tab pdf\cr
#'   tex.driver.options \tab graphics driver options \tab NULL\cr
#'   tex.graphics.ext \tab tex graphics file extension \tab NULL\cr
#' }}
#' note: \code{tex.driver.options} depends upon \code{tex.graphics.driver} you
#' should usually change them together. e.g. with \code{tex.graphics.driver=="jpeg"}
#' you may set \code{tex.driver.options} to \code{list(units="in", res=600)}
NULL

# -------------------------------------------------
# <internal> environment for globals
#
.lx.env <- new.env()

# -------------------------------------------------
# <internal> <no_export>
# this function is called from [lx.r] .onAttach
# to setup package global variables
#
.lx.setup <- function() {

  # setup default options
  
  .lx.env$options <- list()

  lx.options(verbose="info")
  lx.options(use.threads=FALSE)
  lx.options(mc.cores=parallel::detectCores())
  lx.options(pg.verbose=FALSE)
  lx.options(pg.options.style=1)
  lx.options(pg.options.time=TRUE)
  lx.options(tex.dir="tmp")
  lx.options(tex.packages=c("graphicx", "float"))
  lx.options(tex.geometry="margin=0.5in")
  lx.options(tex.docclass="article")
  lx.options(tex.graphics.driver="pdf")
  #lx.options(tex.graphics.driver="jpeg")
  #lx.options(tex.driver.options=list(list(units="in", res=600)))
  
  # register default file handler
  lx.register.file.handler(lx.default.file.handler("default"))
  
  invisible()
}

# -------------------------------------------------
#' @name lx.COLORS
#' @aliases lx.RED lx.GREEN lx.BLUE lx.GREY lx.LGREY lx.TRANSP
#' @docType data
#' @title lx global colors
#' @description
#' \subsection{Lx global colors}{
#' \tabular{ll}{
#'   \bold{name} \tab \bold{descr.}\cr
#'   lx.COLORS \tab a nice looking color scale from d3.scale.category10\cr
#'   lx.RED  \tab semi-transparent red color for plots\cr
#'   lx.GREEN  \tab semi-transparent green color for plots\cr
#'   lx.BLUE  \tab semi-transparent blue color for plots\cr
#'   lx.GREY  \tab semi-transparent grey color for plots\cr
#'   lx.LGREY  \tab semi-transparent lightgrey color for plots\cr
#'   lx.TRANSP  \tab semi-transparent white color for plots\cr
#' }}
#' @seealso \link{lx.rainbow}
#'
NULL

lx.COLORS <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                  "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")

lx.RED    <- rgb(100,0,0,50,maxColorValue=100)

lx.GREEN  <- rgb(10,70,10,50,maxColorValue=100)

lx.BLUE   <- rgb(10,10,100,50,maxColorValue=100)

lx.GREY   <- rgb(0,0,0,30,maxColorValue=100)

lx.LGREY  <- rgb(0,0,0,10,maxColorValue=100)

lx.TRANSP <- rgb(0,0,0,0,maxColorValue=100)

# -------------------------------------------------
#' @name lx.iris
#' @docType data
#' @title nominal iris dataframe
#' @description
#' this is a version of \link{iris} with 3 nominal columns:\cr
#' species=c("setosa", "versicolor", "virginica")\cr
#' petal=c("small", "large")\cr
#' sepal=c("small", "large")\cr
#' used for testing and examples.
#'
NULL
