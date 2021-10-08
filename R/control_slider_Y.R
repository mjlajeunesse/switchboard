#' A vertical slider to modify the range of a continuous simulation variable.
#'
#' The \code{control_slider_Y} widget displays a vertical slider to drag and select a new
#' numeric value of a simulation variable.
#'
#' @section Usage:
#' @section Usage:
#'    \preformatted{control_slider_Y(inject = "", minimum = 0, maximum = 100,
#'    label = "", size = 1, placeOnGrid = c(1, 1))}
#'
#' @param inject String of the variable name to be modified/injected by the
#'    switch widget. For example, \code{inject = "i"}. The variable should be in
#'    boolean form (e.g., 0 or 1, FALSE or TRUE).
#' @param minimum The minimum value of \code{inject}.
#' @param maximum The maximum value of \code{inject}.
#' @param label A small caption/label for the widget.
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
#'      varToSlide <- 0
#'      for (i in 1:500) {
#'        switchboard(delay = 0.01) %>%
#'          control_slider_Y("varToSlide", label = "0 to 100") %>%
#'          number(varToSlide)
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family injectors
#' @name control_slider_Y
NULL


#' @inheritParams control_slider_Y
#' @import tcltk magrittr
#' @export
control_slider_Y <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = control_slider_Y_construct(.switchboard, ...),
                     updater = NULL, ...)
}


#' helper function that constructs canvas items of a control_slider_Y widget
#' @keywords internal
control_slider_Y_construct <- function(.switchboard,
                               inject = "",
                               minimum = 0,
                               maximum = 100,
                               label = " ",
                               size = 1,
                               ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)
  if(inject != "") {
    if(as.integer(tcltk::tcl("info", "exists", inject)) == 1) tcltk::tcl("unset", inject)
    tcltk::.Tcl(paste("set", inject, mget(inject, envir = parent.frame(n = 3), ifnotfound = NA)))
    injectorSlider <- tcltk::ttkscale(aCanvas, from = maximum, to = minimum, orient = "vertical", variable = inject, value = as.numeric(tcltk::tclvalue(inject)),
                                      command = function(...){assign(inject, as.numeric(tcltk::tclvalue(inject)), envir = parent.frame(n = 5));})
    tcltk::tkcreate(aCanvas, "window", 32.5 * size, 5, height = 80 * size - 5, anchor = "n", window = injectorSlider)
  }
  tcltk::tkcreate(aCanvas, "text", 32.5 * size + 15, 5, text = label, angle = 90, anchor = "ne", font = paste(switchboard.env$font, floor(9 / 1.2)), fill = switchboard.env$mainColors[4])

  return(aCanvas)
}
