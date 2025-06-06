#' Calculates distance between two points
#'
#' @param x1,y1,x2,y2 xy coordinates of the two points (normally 6 digit BNG)
#' @returns distance in coordinate units (normally m)

euclidean_dist <- function(x1, y1, x2, y2) {
  r <- sqrt((x1 - x2)^2 +
              (y1 - y2)^2)

  return(r)
}

#' returns max(x) from a vector if it has rows, if it has no rows returns
#' 0
#'
#' @param x a numeric vector
#' @returns numeric

max0 <- function(x) {
  if (NROW(x) == 0) {
    0
  } else {
    max(x)
  }
}
