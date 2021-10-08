#' A pair of vertical sliders to modify the range of two continuous simulation variables.
#'
#' The \code{control_slider_Y_pair} widget displays two vertical sliders to drag and
#' select new numeric values of two simulation variable.
#'
#' @section Usage:
#'    \preformatted{control_slider_Y_pair(inject = c("", ""), minimum = c(0, 0),
#'    maximum = c(100, 100), label = "", size = 1, placeOnGrid = c(1, 1))}
#'
#' @param inject A vector of the two strings for each variable name to be
#'    modified/injected by the two sliders. For example, \code{inject = c("A", "B")}.
#' @param minimum A vector of the two minimum values for each variable in \code{inject}.
#' @param maximum A vector of the two maximum values for each variable in \code{inject}.
#' @param label A vector of the two small caption/labels for each slider.
#' @param size A number used to designate the size (magnification) of the
#'    widget. The default is set to \code{1} which is 80 by 80 pixels. For
#'    example, setting to \code{3} will results in a widget 3-times the default
#'    size (240 by 240 pixels) and will occupy a grid area of 3 by 3.
#' @param placeOnGrid A row by column coordinate (e.g., \code{c(row-number, column-number)})
#'    of a grid that designates the position to draw the widget on the
#'    \code{switchboard}. Use \code{showGrid()} to help organize widget placement
#'    on dashboard. The default places the first widget in pipe chain to
#'    the \code{c(1, 1)} position, and all following on the same row stacked to
#'    the right.
#'
#' @return Nothing.
#'
#' @examples \dontrun{
#'
#'      varToSlideA <- 0
#'      varToSlideB <- 50
#'      for (i in 1:500) {
#'        switchboard(delay = 0.01) %>%
#'          control_slider_pair_Y(inject = c("varToSlideA", "varToSlideB"),
#'                              label = c("0 to 100", "0 to 100")) %>%
#'          number_pair(c(varToSlideA, varToSlideB))
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family injectors
#' @name control_slider_Y_pair
NULL


#' @inheritParams control_slider_Y_pair
#' @import tcltk magrittr
#' @export
control_slider_Y_pair <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = control_slider_Y_pair_construct(.switchboard, ...),
                     updater = NULL, ...)
}


#' helper function that constructs canvas items of a control_slider_Y_pair widget
#' @keywords internal
control_slider_Y_pair_construct <- function(.switchboard,
                                  inject = c("", ""),
                                  minimum = c(0, 0),
                                  maximum = c(100, 100),
                                  label = c(" ", " "),
                                  size = 1,
                                  ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)


  if(inject[1] != "") {
    if(as.integer(tcltk::tcl("info", "exists", inject[1])) == 1) tcltk::tcl("unset", inject[1])
    tcltk::.Tcl(paste("set", inject[1], mget(inject[1], envir = parent.frame(n = 3), ifnotfound = NA)))
    injectorSlider1 <- tcltk::ttkscale(aCanvas, from = maximum[1], to = minimum[1], orient = "vertical", variable = inject[1], value = as.numeric(tcltk::tclvalue(inject[1])),
                                       command = function(...){assign(inject[1], as.numeric(tcltk::tclvalue(inject[1])), envir = parent.frame(n = 4));})
    tcltk::tkcreate(aCanvas, "window", 12.5 * size, 5, height = 80 * size - 5, anchor = "nw", window = injectorSlider1)
  }

  if(inject[2] != "") {
    if(as.integer(tcltk::tcl("info", "exists", inject[2] )) == 1) tcltk::tcl("unset", inject[2] )
    tcltk::.Tcl(paste("set", inject[2], mget(inject[2], envir = parent.frame(n = 3), ifnotfound = NA)))
    injectorSlider2 <- tcltk::ttkscale(aCanvas, from = maximum[2], to = minimum[2], orient = "vertical", variable = inject[2] , value = as.numeric(tcltk::tclvalue(inject[2])),
                                       command = function(...){assign(inject[2] , as.numeric(tcltk::tclvalue(inject[2])), envir = parent.frame(n = 4));})
    tcltk::tkcreate(aCanvas, "window", 42.5 * size, 5, height = 80 * size - 5, anchor = "nw", window = injectorSlider2)
  }

  tcltk::tkcreate(aCanvas, "text", 12.5 * size - 10, 37.5 * size, text = label[1], anchor = "center", angle = 90, font = paste(switchboard.env$font, floor(9 / 1.2)), fill = switchboard.env$mainColors[4])
  tcltk::tkcreate(aCanvas, "text", 42.5 * size + 28, 37.5 * size, text = label[2], anchor = "center", angle = 90, font = paste(switchboard.env$font, floor(9 / 1.2)), fill = switchboard.env$mainColors[4])

  return(aCanvas)
}
