# -------------------------------------------------
# $Id: lx.tex.r 494 2019-03-08 19:30:51Z viari $
# lx latex reporting
#

# =================================================
# internal utlities
#

# -------------------------------------------------
# assert used packages
# currently xtable
#
.tex.assert.packages <- function() {
  lx.require("xtable", load=FALSE, tarball=lx.system.file("packages/xtable_1.7-1.tar.gz"),
             repos=NULL, type="source")
}

# -------------------------------------------------
# get temporary directory
#
.tex.dir <- function(...) {
  dir <- file.path(lx.options("tex.dir", default="tmp"), ...)
  dir.create(dir, showWarnings=FALSE)
  dir
}

# -------------------------------------------------
# get filname
#
.tex.filename <- function(pattern=NULL, fileext="") {
  if (is.null(pattern))
    tempfile(pattern="tmp", tmpdir=.tex.dir(), fileext=fileext)
  else
    file.path(.tex.dir(), paste0(pattern, fileext))
}

# -------------------------------------------------
# output line
#
.tex.out <- function(tex, ..., sep="", nl=TRUE, lf=FALSE) {
  cat(..., ifelse(lf, "\\\\", ""), ifelse(nl, "\n", ""), file=tex$file, sep=sep, fill=FALSE)
}

# -------------------------------------------------
# output tag
#
.tex.tag <- function(tex, tag=NULL, value=NULL, attr=NULL, prepend=FALSE) {
  .tex.out(tex, "\\", tag, nl=FALSE)
  if (prepend && (! is.null(attr)))
    .tex.out(tex, "[", attr, "]", nl=FALSE)
  if (! is.null(value))
    .tex.out(tex, "{", value, "}", nl=FALSE)
  if ((! prepend) && (! is.null(attr)))
    .tex.out(tex, "[", attr, "]", nl=FALSE)
  .tex.out(tex, "")
}

# -------------------------------------------------
# output tag conditionnaly
#
.tex.ctag <- function(tex, tag=NULL, value=NULL) {
  if (! is.null(value))
    .tex.tag(tex, tag, value)
}

# -------------------------------------------------
# init tex file
#
.tex.init <- function(tex, title=NULL, author=NULL, prolog=NULL) {
  .tex.tag(tex, "documentclass", lx.options("tex.docclass", default="article"))
  .tex.ctag(tex, "title", title)
  .tex.ctag(tex, "author", author)
  for (p in lx.options("tex.packages"))
    .tex.tag(tex, "usepackage", p)
  if (! is.null(lx.options("tex.geometry")))
    .tex.tag(tex, paste0("usepackage[", lx.options("tex.geometry") ,"]"), "geometry")
  if (! is.null(prolog))
    prolog(tex)
  .tex.tag(tex, "begin", "document")
  if (! is.null(title))
    .tex.tag(tex, "maketitle")
}

# -------------------------------------------------
# end tex file
#
.tex.end <- function(tex) {
  .tex.tag(tex, "end", "document")
}

# -------------------------------------------------
# shield underscore in string
# fix a bug with underscore in title with pdflatex 
# see: http://tex.stackexchange.com/questions/82840/pdflatex-tex-issue-with-section-which-contains-underscore
#
.tex.shield.title <- function(title) {
  gsub("_", "\\\\_", title)
}

# =================================================
# API
#

# -------------------------------------------------
#' check if latex is properly installed
#' @param cmd latext command to check
#' @param warn issue warning on error
#' @return logical
#' @note
#' this function uses \code{system('which command')} and therefore
#' only works on Unix
#'
tex.installed <- function(cmd="pdflatex", warn=TRUE) {
  absent <- system(paste0("which ", cmd), ignore.stdout=T) != 0
  lx.warnif(warn && absent, "please install ", cmd, " to compile tex file")
  ! absent
}

# -------------------------------------------------
#' open new tex report file
#' @param name file basename
#' @param title report title
#' @param author report author
#' @param prolog function to add user's specific latex commands
#' in document prolog. called as \code{prolog(texHandle)}
#' @return tex handle
#' @note
#' the actual tex file is \code{lx.options(tex.dir)/name.tex}
#' and the final pdf file is \code{name.pdf}
#'
tex.open <- function(name="report", title=name, author=lx.sysinfo("user"), prolog=NULL) {
#
# todo : copy lx.options(tex.*) as internal options
#
  tex <- list()
  tex$name <- name
  tex$filename <- .tex.filename(name, ".tex")
  tex$file <- file(tex$filename, open="wt")
  title <- .tex.shield.title(title) # fix bug with underscore in pdflatex
  .tex.init(tex, title=title, author=author, prolog=prolog)
  tex
}

# -------------------------------------------------
#' close tex report file
#' @param tex handle (as returned by \link{tex.open})
#' @param compile if TRUE, call \link{tex.compile} to produce pdf
#' @param ... any argument to \link{tex.compile} (see note)
#' @note a useful argument is \code{clean=TRUE} that remove
#'       all auxiliary files created during the conversion
#' @return invisible(tex)
#'
tex.close <- function(tex, compile=TRUE, ...) {
  .tex.end(tex)
  close(tex$file)
  if (compile)
    tex.compile(tex, ...)
  invisible(tex)
}

# -------------------------------------------------
#' compile tex report file
#' @param tex handle (as returned by \link{tex.open})
#' @param ... any argument passed to tools::\link{texi2pdf}
#' @return invisible(tex)
#'
tex.compile <- function(tex, ...) {
  if (tex.installed())
    tools::texi2pdf(file=tex$filename, ...) 
  invisible(tex)
}

# -------------------------------------------------
#' output line(s) to tex report file
#' @param tex handle (as returned by \link{tex.open})
#' @param ... any argument supported by \link{cat}
#' @param lf add a latex linefeed \code{\\\\} at end of line
#' @return invisible(tex)
#' @seealso \link{tex.print} for formatted output
#'
tex.out <- function(tex, ..., lf=FALSE) {
  .tex.out(tex, ..., lf=lf)
  invisible(tex)
}

# -------------------------------------------------
#' output tex tag to tex report file
#' @description
#' output tex tag as\cr
#' \code{\<tag>{<value>}[<attr>]}
#' or \code{\<tag>[<attr>]{<value>}} depending upon the \code{prepend} argument.
#' @param tex handle (as returned by \link{tex.open})
#' @param tag tag name
#' @param value tag value
#' @param attr tag attribute(s)
#' @param prepend if TRUE append attributes before value. default is FALSE,
#' append attributes after value.
#' @return invisible(tex)
#'
tex.tag <- function(tex, tag=NULL, value=NULL, attr=NULL, prepend=FALSE) {
  .tex.tag(tex, tag, value, attr, prepend)
  invisible(tex)
}

# -------------------------------------------------
#' print formatted argument to tex report file
#' @param tex handle (as returned by \link{tex.open})
#' @param x an object to print (see details)
#' @param ... any argument supported by \link{print} variant
#' @return invisible(tex)
#' @details
#' if \code{x} is of class \code{xtable} then just call the
#' S3 method \code{print} on \code{x}.\cr
#' else, first format x using \code{xtable} then call \link{print}
#' 
#' x should be of a type handled by \code{xtable}.
#' see \link{tex.out} for simple output.
#' @note
#' this requires the \code{xtable} package that will be internally
#' installed if not already present (using internal distribution)
#' @seealso \link{tex.out} for simple output
#'
tex.print <- function(tex, x, ...) {
  .tex.assert.packages()
  if (! ("xtable" %in% class(x)))
    x <- xtable::xtable(x)
  print(x, file=tex$file, append=TRUE, type="latex", ...)
  invisible(tex)
}

# -------------------------------------------------
#' start new tex section
#' @param tex handle (as returned by \link{tex.open})
#' @param name section name
#' @param numbered should section be numbered
#' @return invisible(tex)
#'
tex.section <- function(tex, name, numbered=TRUE) {
  tex.tag(tex, ifelse(numbered, "section", "section*"), name)
}

# -------------------------------------------------
#' start new tex subsection
#' @param tex handle (as returned by \link{tex.open})
#' @param name section name
#' @param numbered should section be numbered
#' @return invisible(tex)
#'
tex.subsection <- function(tex, name, numbered=TRUE) {
  tex.tag(tex, ifelse(numbered, "subsection", "subsection*"), name)
}

# -------------------------------------------------
#' start new tex graphics
#' @param tex handle (as returned by \link{tex.open})
#' @param ... any argument to graphics driver
#' @return tex
#' @note
#' it is important to reaffect tex handle upon return
#' since the internal components have been modified
#' @note
#' this function opens the graphic driver defined in \code{lx.options("tex.graphics.driver")}
#' (default to "pdf") with options \code{lx.options("tex.driver.options")}.\cr
#' with "jpeg", "png" and "tiff" drivers, you may set the 'units' and 'res' options
#' in \code{lx.options("tex.driver.options")} (typical values are units="in" and res=300).
#
tex.fig.on <- function(tex, ...) {
  args <- list(...)
  driver <- lx.options("tex.graphics.driver", default="pdf")
  fileext <- lx.options("tex.graphics.ext", default=paste0(".", driver))
  tex$graphics <- .tex.filename(fileext=fileext)
  driver.args <- lx.mixin(lx.options("tex.driver.options")[[1]], args, names(args))
  do.call(driver, as.list(c(file=tex$graphics, driver.args)))
  tex.tag(tex, "begin", "figure", "H")
  # keep figure size
  tex$figsiz <- lx.mixin(NULL, args, intersect(names(args), c("width", "height")))
  tex
}

# -------------------------------------------------
#' end tex graphics
#' @param tex handle (as returned by \link{tex.fig.on})
#' @param ... any attribute to latex:includegraphics
#' @return tex
#' @note
#' it is important to reaffect tex handle upon return
#' since the internal components have been modified
#'
tex.fig.off <- function(tex, ...) {
  args <- list(...)
  if (! is.null(tex$graphics)) {
    dev.off()
    siz <- lx.mixin(tex$figsiz, args, intersect(names(args), c("width", "height")))
    opt <- lx.options("tex.driver.options")[[1]]
    if ("units" %in% names(opt))
      siz  <- sapply(siz, function(x) if (is.numeric(x)) paste0(x, opt$units) else x)
    attr <- lx.list2str(lx.mixin(args, siz, names(siz)))
    tex.tag(tex, "includegraphics", tex$graphics, attr, prepend=T)
    tex.tag(tex, "end", "figure")
  }
  tex$graphics <- NULL
  tex$figsiz   <- NULL
  tex
}
