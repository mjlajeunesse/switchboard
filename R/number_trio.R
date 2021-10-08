#' Display the value of three continuous variables in a simulation.
#'
#' The \code{number_trio} widget displays the numerical value of three continuous
#' variables.
#'
#' @section Usage:
#'    \preformatted{number_trio(eavesdrop = c(NULL, NULL, NULL), digits = 5,
#'    updates = 1, label = c("", "", ""), size = 1, placeOnGrid = c(1, 1))}
#'
#' @param eavesdrop A vector of three variables to track.
#' @param digits The number of digits to display in a widget.
#' @param label A vector of three small captions/labels for the widget.
#' @param updates The number of times the widget is to be updated (e.g., when
#'     it be modified/changed). The default updates with each iteration.
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
#'      for (i in 1:250) {
#'        switchboard(delay = 0.01) %>%
#'          number_trio(c(i, i/2, i/3), label = c("i", "i/2", "i/3"))
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family eavesdroppers
#' @name number_trio
NULL


#' @inheritParams number_trio
#' @import tcltk magrittr
#' @export
number_trio <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = number_trio_construct(.switchboard, ...),
                     updater = number_trio_update(.switchboard, ...), ...)
}


#' helper function that constructs canvas items of a number_trio widget
#' @keywords internal
number_trio_construct <- function(.switchboard,
                                  eavesdrop = c(NULL, NULL, NULL),
                                  size = 1,
                                  digits = 5,
                                  label = c(" ", " ", " "),
                                  ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)
  tcltk::tkcreate(aCanvas, "text", 0, 4, text = signif(eavesdrop[1], digits), anchor = "nw", font = paste("courier", floor(15 * size / 1.2), "bold"), fill = switchboard.env$mainColors[1], tags = "number1")
  tcltk::tkcreate(aCanvas, "text", 0, 16 * size, text = label[1], anchor = "nw",  font = paste(switchboard.env$font, floor(10 * size / 1.2)), fill = switchboard.env$mainColors[4])
  tcltk::tkcreate(aCanvas, "text", 37.5 * size, 37 * size + 3, text = signif(eavesdrop[2], digits), anchor = "center", font = paste("courier", floor(15 * size / 1.2), "bold"), fill = switchboard.env$mainColors[1], tags = "number2")
  tcltk::tkcreate(aCanvas, "text", 37.5 * size, 39 * size + 7 * size + 3, text = label[2], anchor = "center",  font = paste(switchboard.env$font, floor(10 * size / 1.2)), fill = switchboard.env$mainColors[3])
  tcltk::tkcreate(aCanvas, "text", 80 * size - 5, 80 * size -  6 * size, text = signif(eavesdrop[3], digits), anchor = "se", font = paste("courier", floor(15 * size / 1.2), "bold"), fill = switchboard.env$mainColors[1], tags = "number3")
  tcltk::tkcreate(aCanvas, "text", 80 * size - 5, 80 * size, text = label[3], anchor = "se",  font = paste(switchboard.env$font, floor(10 * size / 1.2)), fill = switchboard.env$mainColors[4])

  return (aCanvas)
}


#' helper function that updates canvas items of a number_trio widget
#' @keywords internal
number_trio_update <- function(.switchboard,
                               eavesdrop = c(NULL, NULL, NULL),
                               digits = 5,
                               updates = 1,
                               ...) {

  if(switchboard.env$totalIterations %% updates == 0) {
    aCanvas <- paste0(.switchboard$theSwitchboard, ".", .switchboard$theWidget)
    tcltk::.Tcl(paste(aCanvas, "itemconfigure", "number1", "-text", signif(eavesdrop[1], digits)))
    tcltk::.Tcl(paste(aCanvas, "itemconfigure", "number2", "-text", signif(eavesdrop[2], digits)))
    tcltk::.Tcl(paste(aCanvas, "itemconfigure", "number3", "-text", signif(eavesdrop[3], digits)))
  }

}
