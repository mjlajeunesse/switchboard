#' Display the value of a continuous variable in a simulation.
#'
#' The \code{number} widget displays the numerical value of a continuous variable.
#'
#' @section Usage:
#'    \preformatted{number(eavesdrop = NULL, digits = 5, updates = 1, label = "",
#'    size = 1, placeOnGrid = c(1, 1))}
#'
#' @param eavesdrop The variable to track.
#' @param digits The number of digits to display in a widget.
#' @param label A small caption/label for the widget.
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
#'          number(i * i, label = "i * i")
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family eavesdroppers
#' @name number
NULL


#' @inheritParams number
#' @import tcltk magrittr
#' @export
number <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = number_construct(.switchboard, ...),
                     updater = number_update(.switchboard, ...), ...)
}

#' helper function that constructs canvas items of a number widget
#' @keywords internal
number_construct <- function(.switchboard,
                             eavesdrop = NULL,
                             size = 1,
                             label = " ",
                             ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)
  tcltk::tkcreate(aCanvas, "text", 5 * size, 40 * size, anchor = "w", text = eavesdrop, font = paste("courier", floor(16 * size / 1.2)), fill = switchboard.env$mainColors[1], tags = "aNumber")
  tcltk::tkcreate(aCanvas, "text", 5 * size, 40 * size + 12 * size, anchor = "w", text = label, font =paste(switchboard.env$font, floor(9 * size / 1.2)), fill = switchboard.env$mainColors[4])

  return (aCanvas)
}



#' helper function that updates canvas items of a number widget
#' @keywords internal
number_update <- function(.switchboard,
                          eavesdrop = NULL,
                          digits = 5,
                          updates = 1,
                          ...) {

  if(switchboard.env$totalIterations %% updates == 0) {
    aCanvas <- paste0(.switchboard$theSwitchboard, ".", .switchboard$theWidget)
    tcltk::tkitemconfigure(aCanvas, "aNumber", "-text", signif(eavesdrop, digits))
  }

}

