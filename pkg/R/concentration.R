

#' @export
herfindahl <- function(x) {
  y <- sumex(x, ex = order(x))^2
  y <- sum(y)

  y
}


#' @export
rosenbluth <- function(x) {
  n <- length(x)

  y <- sumex(x, ex = order(x, decreasing = TRUE))
  y <- sum((1:n) * y)
  y <- 1 / ((2 * y) - 1)

  y
}


#' @export
exogeny <- function(x, ex) {
  n <- length(x)

  y <- sumex(x, ex = ex)
  y <- sum((1:n) * y)
  y <- 1 / ((2 * y) - 1)

  y
}


#' @export
concentration_ratio <- function(x, g, ex = order(x)) {
  y <- sumex(x, ex = ex)
  y <- sum(y[1:g])

  y
}


#' @export
sumex <- function(x, ex) {
  x <- x[ex]
  x / sum(x)
}


