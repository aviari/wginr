#
# additional manual tests : dishonnest casino
#

if (FALSE) {

  require(thmm)

  #
  # dishonest casino: density function
  #
  dice <- function(x, p6=1/6, log=FALSE) {
    r <- unlist(lapply(x, function(x) {
            unlist(lapply(p6, function(p6) {
                if (is.na(x)) NA
                else if (x == 6) p6
                else if (x %in% 1:5) (1-p6)/5
                else 0
              })) }), recursive=F)
    if (log) r <- base::log(r)
    r
  }
  #
  # dishonest casino: random generation function
  #
  rice <- function(n, p6=1/6) {
    sample(1:6, n, prob=c(rep((1-p6)/5, 5), p6), replace=TRUE)
  }

  hmm <- thmm.init(dice, 0.1, p6=c(1/6, 3/6))
  x <- thmm.simulate(hmm, 100, with.values=T)
  v <- thmm.viterbi(hmm, x$values, .useC=T)

  pal <- c("black", "red")
  plot(x$values, type="l", lwd=0.5, col="gray")
  points(x$values, type='p', pch=19, col=pal[x$states])
  lines(1+(x$states-1)*5, col=3, lwd=2)
  lines(1+(v$states-1)*5, col=2, lty=2, lwd=2)
}

