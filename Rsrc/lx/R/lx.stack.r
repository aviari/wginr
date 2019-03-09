# -------------------------------------------------
# $Id: lx.stack.r 396 2019-01-02 22:53:10Z viari $
# lx stack utility
#

# -------------------------------------------------
#' new (FILO) stack
#' @return empty stack
#' @note this implementation uses lists and is not efficient.
#' this is mostly intended for storing small number of elements.
#' if you need a better implementation, use the 'rstack' library
#' @examples
#' stk <- lx.stack.new()
#' stk <- lx.stack.push(stk, 1)
#' stk <- lx.stack.push(stk, "two")
#' stk <- lx.stack.push(stk, NULL)
#' stk <- lx.stack.push(stk, 1:4)
#' while (! lx.stack.is.empty(stk)) {
#'  stk <- lx.stack.pop(stk)
#'  val <- lx.stack.value(stk)
#'  lx.out(typeof(val), ": ", val)
#' }
#
lx.stack.new <- function() {
  lx.new("Stack", data=list())
}

# -------------------------------------------------
#' tell if stack is empty
#' @param stk Stack
#' @return TRUE if stack is empty
#' @note this implementation uses lists and is not efficient.
#' this is mostly intended for storing small number of elements.
#' if you need a better implementation, use the 'rstack' library
#' @examples
#' stk <- lx.stack.new()
#' stk <- lx.stack.push(stk, 1)
#' stk <- lx.stack.push(stk, "two")
#' stk <- lx.stack.push(stk, NULL)
#' stk <- lx.stack.push(stk, 1:4)
#' while (! lx.stack.is.empty(stk)) {
#'  stk <- lx.stack.pop(stk)
#'  val <- lx.stack.value(stk)
#'  lx.out(typeof(val), ": ", val)
#' }
#
lx.stack.is.empty <- function(stk) {
  length(stk$data) == 0
}

# -------------------------------------------------
#' push value into stack
#' @param stk Stack
#' @param value any R object (including NULL)
#' @return modified stack
#' @note this implementation uses lists and is not efficient.
#' this is mostly intended for storing small number of elements.
#' if you need a better implementation, use the 'rstack' library
#' @examples
#' stk <- lx.stack.new()
#' stk <- lx.stack.push(stk, 1)
#' stk <- lx.stack.push(stk, "two")
#' stk <- lx.stack.push(stk, NULL)
#' stk <- lx.stack.push(stk, 1:4)
#' while (! lx.stack.is.empty(stk)) {
#'  stk <- lx.stack.pop(stk)
#'  val <- lx.stack.value(stk)
#'  lx.out(typeof(val), ": ", val)
#' }
#
lx.stack.push <- function(stk, value) {
  stk$data <- c(stk$data, list(value))
  stk
}

# -------------------------------------------------
#' pop value from stack
#' @description
#' pop top element from stack and store it in stk->value
#' @param stk Stack
#' @return modified stack
#' @note no error is raised if stack is empty
#' @note this implementation uses lists and is not efficient.
#' this is mostly intended for storing small number of elements.
#' if you need a better implementation, use the 'rstack' library
#' @examples
#' stk <- lx.stack.new()
#' stk <- lx.stack.push(stk, 1)
#' stk <- lx.stack.push(stk, "two")
#' stk <- lx.stack.push(stk, NULL)
#' stk <- lx.stack.push(stk, 1:4)
#' while (! lx.stack.is.empty(stk)) {
#'  stk <- lx.stack.pop(stk)
#'  val <- lx.stack.value(stk)
#'  lx.out(typeof(val), ": ", val)
#' }
#
lx.stack.pop <- function(stk) {
  stk$value <- unlist(tail(stk$data, 1), recursive=F)
  stk$data <- head(stk$data, -1)
  stk
}

# -------------------------------------------------
#' get last popped value from stack
#' @param stk Stack
#' @return last popped value
#' @note this implementation uses lists and is not efficient.
#' this is mostly intended for storing small number of elements.
#' if you need a better implementation, use the 'rstack' library
#' @examples
#' stk <- lx.stack.new()
#' stk <- lx.stack.push(stk, 1)
#' stk <- lx.stack.push(stk, "two")
#' stk <- lx.stack.push(stk, NULL)
#' stk <- lx.stack.push(stk, 1:4)
#' while (! lx.stack.is.empty(stk)) {
#'  stk <- lx.stack.pop(stk)
#'  val <- lx.stack.value(stk)
#'  lx.out(typeof(val), ": ", val)
#' }
#
lx.stack.value <- function(stk) {
  stk$value
}

# -------------------------------------------------
#' print method for Stack
#' @param x Stack
#' @param ... further arguments passed to or from other methods.
#' @return (invisible) stk
#
print.Stack <- function(x, ...) {
  print(paste0("Stack (size=", length(x$data), ")"))
  invisible(x)
}

