#' @title Pipe operator
#'
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @name %>%
NULL





#' easing function for timed animations
#' @name animate
#'
#' @keywords internal
animate <- function(theTime,
					timeMax,
					easer = "Bezier") {
  smooth <- theTime / timeMax
  if(easer == "Bezier") return((smooth * smooth * (3.0 - 2.0 * smooth)) * timeMax)
  return(((smooth * smooth) / (2.0 * (smooth * smooth - smooth) + 1.0)) * timeMax) #parametric
}


#' converting raw values into standardized coordinates within an 80 by 80 widget
#' @name value_to_coordinate
#'
#' @keywords internal
value_to_coordinate <- function(theValue,
                                min_value,
                                max_value,
                                size = 1,
                                adjust = c(0, 0)) {

  max_Y <- 75 * size + adjust[1]; min_Y <- adjust[2];
  if(theValue > max_value) return(min_Y)
  else if(theValue < min_value) return(max_Y)
  return(max_Y - (theValue - min_value) * ((max_Y - min_Y) / (max_value - min_value)))
}
