# -------------------------------------------------
# $Id: lx.plot.r 396 2019-01-02 22:53:10Z viari $
# lx plot utilities
#

# =================================================
# plot layout utilities
#

# -------------------------------------------------
#' determine best \code{\link{par}(mfrow=c(x,y))} layout to fit for n plots
#' @param n number of plots
#' @seealso \link{par}
#' @examples
#' opar <- par(no.readonly=TRUE)
#' par(mfrow=lx.mfrow(10))
#' # ... plots
#' par(opar)
#'
lx.mfrow <- function(n) {
  for (i in 1:n) {
    for (j in 1:i) {
      if (i * j >= n) 
        return(c(j,i))
    }
  }
  NULL # not reached
}

# -------------------------------------------------
#' change color(s) hue, saturation or value
#' @param color vector of any of the three kinds of R color specifications, 
#' i.e., either a color name (as listed by \link{colors}()),
#' a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see \link{rgb}),
#' or a positive integer i meaning \link{palette}()[i]
#' @param dh change in hue (-1 <= dh <= 1)
#' @param ds change in saturation (-1 <= ds <= 1)
#' @param dv change in value (-1 <= dv <= 1)
#' @return modified colors (as hexadecimal strings)
#' @seealso \link{lx.color.light}, \link{lx.color.alpha}
#' @examples
#' lx.color.change("#ff0000", ds=-0.5)  # => #ff8080
#' lx.color.change("red", ds=-0.5)      # => #ff8080
#
lx.color.change <- function(color, dh=0, ds=0, dv=0) {
  color <- rgb2hsv(col2rgb(color)) + c(h=dh, s=ds, v=dv)
  color <- pmin(pmax(color, 0), 1)
  apply(color, 2, function(c) do.call(hsv, as.list(c)))
}

# -------------------------------------------------
#' lighten or darken color(s)
#' @param color vector of any of the three kinds of R color specifications, 
#' i.e., either a color name (as listed by \link{colors}()),
#' a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see \link{rgb}),
#' or a positive integer i meaning \link{palette}()[i]
#' @param delta amount in percentage (-100 <= delta <= 100)\cr
#'        delta > 0 : lighten; delta < 0 : darken
#' @return modified colors (as hexadecimal strings)
#' @seealso \link{lx.color.change}, \link{lx.color.alpha}, \link{lx.color.lum}
#' @examples
#' lx.color.light("red", 50)  # => #ff8080
#' lx.color.light("red", -50) # => #800000
#
lx.color.light <- function(color, delta) {
  if (delta > 0) 
    lx.color.change(color, ds=-delta/100)
  else
    lx.color.change(color, dv=delta/100)
}

# -------------------------------------------------
#' add alpha channel to color(s)
#' @param color vector of any of the three kinds of R color specifications, 
#' i.e., either a color name (as listed by \link{colors}()),
#' a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see \link{rgb}),
#' or a positive integer i meaning \link{palette}()[i]
#' @param alpha alpha channel value (0 <= alpha <= 1)\cr
#'        alpha=0: completely transparent, alpha=1: completely opaque
#' @return modified colors (as hexadecimal strings)
#' @seealso \link{lx.color.change}, \link{lx.color.light}
#' @examples
#' lx.color.alpha("red", 0)  # => #ff000000
#' lx.color.alpha("red", 1)  # => #ff0000ff
#
lx.color.alpha <- function(color, alpha=1) {
  sapply(color, function(c) rgb(t(col2rgb(c)),
                                alpha=255*alpha,
                                maxColorValue=255))
}

# -------------------------------------------------
#' rainbow color scale
#' @description
#' similar to \link{rainbow} except that colors do not close
#' circularly (i.e. start and end colors are not the same).
#' @param n the number of required colors
#' @param skip end portion of color circle to remove (default=1/6)
#' @param ... any argument of \link{rainbow}
#' @return a character vector of colors
#' @examples
#' lx.rainbow(10)
#
lx.rainbow <- function(n, skip=1/6, ...) {
  nr <- floor(n * (1 + skip))
  rainbow(nr, ...)[1:n]
}

# -------------------------------------------------
#' get luminance (darkness) of color(s)
#' @description compute luminance of colors for human eye. luminance ranges
#' between 0 (dark) and 1 (light).
#' @param color vector of any of the three kinds of R color specifications, 
#' i.e., either a color name (as listed by \link{colors}()),
#' a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see \link{rgb}),
#' or a positive integer i meaning \link{palette}()[i]
#' @return numeric vector of luminance
#' @seealso \link{lx.color.fore}
#' @examples
#' lx.color.lum(c("black", "white", "red", "yellow"))
#
lx.color.lum <- function(color) {
  apply(col2rgb(color), 2, 
        function(rgb) (rgb[1]*0.299 + rgb[2]*0.587 + rgb[3]*0.114)/256)
}

# -------------------------------------------------
#' choose foreground color (black or white) from background color
#' @description compute the best foreground color for given
#' background colors according to background luminance
#' @param color vector of any of the three kinds of R color specifications, 
#' i.e., either a color name (as listed by \link{colors}()),
#' a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see \link{rgb}),
#' or a positive integer i meaning \link{palette}()[i]
#' @param threshold luminance threshold
#' @param white foreground color for dark background
#' @param black foreground color for light background
#' @return numeric vector of foreground colors
#' @seealso \link{lx.color.lum}
#' @examples
#' lx.color.fore(c("black", "white", "red", "yellow"))
#
lx.color.fore <- function(color, threshold=0.5, white="white", black="black") {
  ifelse(lx.color.lum(color) < threshold, white, black)
}

# -------------------------------------------------
#' DeltaE distance between two colors
#' @description compute the DeltaE distance between two colors
#' according to the CIE 1976 definition.
#' @param col1,col2 colors any of the three kinds of R color specifications, 
#' i.e., either a color name (as listed by \link{colors}()),
#' a hexadecimal string of the form "#rrggbb" or "#rrggbbaa" (see \link{rgb}),
#' or a positive integer i meaning \link{palette}()[i]
#' @return positive numeric DeltaE
#' @examples
#' lx.color.deltaE("white", "white")
#' lx.color.deltaE("white", "red")
#
lx.color.deltaE <- function(col1, col2) {
  .lab <- function(col) convertColor(t(col2rgb(col)), 
                                     "sRGB", "Lab", scale.in=255)
  sqrt(sum((.lab(col1)-.lab(col2))^2))
}

# =================================================
# plot utilities
#

# -------------------------------------------------
#' plotting large number of points
#' @description
#' this is a wrapper to \link{plot} (or \link{points}, \link{lines})
#' with pre-sampling of points. It is intended to plot very large
#' number of points (i.e. when plotting time becomes prohibited).
#' @param x,y data (same as in \link{plot})
#' @param fun plotting function (e.g. \link{plot}, \link{points}, \link{lines})
#' @param cex plot symbols magnifying factor. see \link{plot}
#' @param xlab a label for the x axis, defaults to a description of x
#' @param ylab a label for the y axis, defaults to a description of y
#' @param pch plotting character (see \link{par})
#' @param col plotting color (see \link{par})
#' @param ... arguments to \code{fun} (see \link{graphical parameters})
#' @param .samp number of points to sample
#' @param .method sampling method (may be abbreviated)\cr
#' \code{sampled}: random sampling\cr
#' \code{spaced}: equally spaced points\cr
#' @param .seed seed for sampling (for .method="sampled" only). use
#' NA to prevent seeding (i.e. to use current seed). use a non-NA
#' value to produce reproducible behavior.
#' @note
#' if \code{.seed != NA} the current seed will be saved and restored.
#' @return same as \link{barplot}
#' @examples
#' \dontrun{
#' lx.plot(rnorm(1e7))
#' lx.plot(rnorm(1e7), ylab="Normal")
#' lx.plot(rnorm(1e7, 0, 0.1), fun=points, col=2)
#' }
#
lx.plot <- function(x, y=NULL, fun=plot, cex=0.1, xlab=NULL, ylab=NULL,
                    col="black", pch=1, ..., 
                    .samp=1e4, .method=c("sampled", "spaced"), .seed=0) {
  
  .method <- match.arg(.method)
  
  # handle labels and actual coordinates
  #
  xlabel <- if (! missing(x)) deparse(substitute(x))
  ylabel <- if (! missing(y)) deparse(substitute(y))
  log <- if ("log" %in% names(list(...))) list(...)[["log"]] else "dft"
  xy <- xy.coords(x, y, xlabel, ylabel, log)
  xlab <- if (is.null(xlab)) xy$xlab else xlab
  ylab <- if (is.null(ylab)) xy$ylab else ylab
  
  # sample data
  #
  if ((.samp > 0) && (length(xy$y) > .samp)) {
    if (.method == "sampled") {
      if (! is.na(.seed)) lx.rnd.push(.seed)
      i <- tryCatch(sort(sample(length(xy$y), .samp, replace=F)),
                    finally=if (! is.na(.seed)) lx.rnd.pop())
    } else {
      i <- round(seq.int(1,length(xy$y), length.out=.samp))
    }
    xy$x <- xy$x[i]
    xy$y <- xy$y[i]
    if (length(cex) > 1) cex <- lx.recycle(cex, length(x))[i]
    if (length(pch) > 1) pch <- lx.recycle(pch, length(x))[i]
    if (length(col) > 1) col <- lx.recycle(col, length(x))[i]
  }
  
  # call plot function
  # note: xlab and ylab are accepted but ignored in points and lines
  #       so we don't need to worry about
  fun(xy, cex=cex, pch=pch, col=col, xlab=xlab, ylab=ylab, ...)
}

# -------------------------------------------------
#' barplot with values indicated on top of bars
#' @details
#' if \code{pos.text} is NULL then the label position will be
#' choosen to fit within the plot.\cr
#' if \code{col.text} is NULL then the label color will be
#' choosen to fit with the bar color.\cr
#' @param x vector or matrix of values
#' @param col bar colors (see \link{barplot})
#' @param col.text text color (or NULL to select contrast color)
#' @param cex.text text magnification
#' @param pos.text text position (see \link{text}, NULL means autopos)
#' @param off.text text offset (see \link{text})
#' @param srt.text text rotation (in degrees)
#' @param ... arguments to \link{barplot}
#' @return same as \link{barplot}
#' @note \code{pos.text} NULL does not work well if srt.text != 0
#' @examples
#' \dontrun{
#' lx.barplot(c(apple=10, orange=5, banana=15))
#' lx.barplot(c(apple=10, orange=5, banana=15), col=lx.COLORS)
#' lx.barplot(c(apple=10, orange=5, banana=15), col.text=0, pos.text=1, col=lx.COLORS)
#' }
#
lx.barplot <- function(x, col=NULL, col.text=NULL, cex.text=1,
                       pos.text=NULL, off.text=0.5, srt.text=0, ...) {

  # plot bars
  #
  mid <- barplot(x, col=col, ...)
  
  # add labels
  #
  if (is.null(pos.text)) {
    y0 <- mean(par("usr")[3:4])
    pos.text <- ifelse(x < y0, 3, 1)
  }
  if (is.null(col.text)) {
    if (is.null(col))
      col.text <- "black"
    else {
      col.text <- mapply(function(col, pos) {
        if (pos==3) "black" else lx.color.fore(col)
      }, col, lx.recycle(pos.text, length(col)))
    }
  }
  text(mid, x, as.character(x), pos=pos.text, offset=off.text,
       col=col.text, cex=cex.text, srt=srt.text)
  invisible(mid)
}

# -------------------------------------------------
#' histogram plot with density
#' @param x vector of values for which the histogram is desired
#' @param bw smoothing bandwidth (see \link{density})
#' @param freq y-axis is frequency or probability (see \link{hist})
#' @param main title (see \link{hist})
#' @param xlab x axis label (see \link{hist})
#' @param ... other arguments to \link{hist}
#' @param col.density density line color
#' @note \code{freq} defaults to FALSE
#' @return same as \link{hist} plus a 'dns' component containing
#' the density.
#' @examples
#' \dontrun{
#' lx.hist(rnorm(1000))
#' lx.hist(rnorm(1000), main="Normal", xlab="x")
#' }
#
lx.hist <- function(x, bw="nrd0",
                    freq=FALSE, 
                    main=paste("Histogram of", xname),
                    xlab=xname,
                    ...,
                    col.density="blue") {
  xname <- paste(deparse(substitute(x), 500), collapse = "\n")
  res <- hist(x, freq=freq, main=main, xlab=xlab, ...)
  dns <- lx.density(x, bw=bw)
  norm <- if (freq) 
            sum(res$counts) / length(res$counts) / sum(dns$y) * length(dns$y)
          else 1
  lines(dns$x, dns$y * norm, col=col.density)
  res$dns <- dns
  invisible(res)
}


# -------------------------------------------------
#' add inset plot into current plot
#' @description
#' add a small (inset) plot in current plot
#' @param ... same arguments as in generic \link{plot}
#' @param inset.pos inset position 'tl', 'tr', 'bl', 'br'
#' @param inset.alpha inset size factor
#' @param inset.margin inset margin
#' @details
#' inset position indicate the poistion of the inset as tl:top-left,
#' tr:top-right, bl=bottom-left, br:bottom-right
#' @examples
#' \dontrun{
#' plot(function(x) {sin(x*10)/x}, col="red", ylab="", main="main-plot")
#' lx.plot.inset(function(x) {cos(x*20)}, ylab="", xlab="inset", cex.axis=0.5,
#'               inset.pos="tr", inset.alpha=0.5)
#' }
#
lx.plot.inset <- function(..., inset.pos=c("tr", "tl", "br", "bl"), 
                          inset.alpha=0.5, inset.margin=0.01) {
  inset.pos <- match.arg(inset.pos)
  .fig <- function(pos="tr", alpha=0.5, margin=0.01) {
    switch(pos,
           tl = c(margin, alpha-margin, 1-alpha+margin, 1-margin),
           tr = c(1-alpha+margin, 1-margin, 1-alpha+margin, 1-margin),
           bl = c(margin, alpha-margin, margin, alpha-margin),
           br = c(1-alpha+margin, 1-margin, margin, alpha-margin),
           c(margin, alpha-margin, 1-alpha+margin, 1-margin))
  }
  ofig <- par("fig")
  par(fig=.fig(pos=inset.pos, alpha=inset.alpha, margin=inset.margin), new=TRUE)
  plot(...)
  par(fig=ofig)
  invisible(NULL)
}
