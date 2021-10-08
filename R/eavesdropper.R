#' Eavesdrop a continuous variable.
#'
#' The \code{eavesdropper} widget displays a horizontal moving window of data
#' (see \code{eavesdropper_X} for a vertical window). The X-axis is time-lagged
#' and the widget keeps track of each point until it reaches the end of the
#' plot. This widget is univariate and only eavesdrops one variable; for a
#' bivariate version use \code{eavesdropper_2D}. The number of data points in the
#' window can be throttled with \code{delay}.
#'
#' @section Usage:
#'    \preformatted{eavesdropper(eavesdrop, minimum = 1, maximum = 100,
#'    label = "", size = 1, placeOnGrid = c(1, 1), updates = 100, delay = 0,
#'    plotMean = FALSE, switch = FALSE)}
#'
#' @param eavesdrop The variable to track.
#' @param minimum The minimum value of \code{eavesdrop}. It defines the plot boundary
#' @param maximum The maximum value of \code{eavesdrop}. It defines the plot boundary
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
#' @param updates The number of times the widget is to be updated (e.g., when
#'     it be modified/changed). The default updates the widget 100 times. Increase
#'     number for smoother progress bar animation.
#' @param delay Pause each update of the switchboard. Default has no delay,
#'    values are in seconds (e.g., \code{delay = 0.01} results in 0.01 second
#'    delay with each iteration).
#' @param plotMean Display a small switchboard-estimated mean and standard deviation
#'     whisker plot on a widget.
#' @param switch Display an on/off switch on a widget that controls widget updates.
#'     When \code{TRUE} it will add the switch in the off-state on the switchboard.
#'     The user must activate the switch to start the widget. The simulation proceeds
#'     even if the switch in in the off-state.
#'
#' @return Nothing.
#'
#' @examples \dontrun{
#'
#'      for (i in 1:400) {
#'        randomNormal <- rnorm(1, 0, 1)
#'        switchboard(delay = 0.01) %>%
#'          eavesdropper(randomNormal, minimum = -5, maximum = 5)
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family moving windows
#' @family eavesdroppers
#' @name eavesdropper
NULL


#' @inheritParams eavesdropper
#' @import tcltk magrittr
#' @export
eavesdropper <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = eavesdropper_construct(.switchboard, ...),
                     updater = eavesdropper_update(.switchboard, ...), ...)
}


#' helper function that constructs canvas items of a eavesdropper widget
#' @keywords internal
eavesdropper_construct <- function(.switchboard,
                                   label = " ",
                                   size = 1,
                                   switch = FALSE,
                                   ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)
  tcltk::tkcreate(aCanvas, "line", 2, 5, 2, 80 * size - 2, 80 * size - 5, 80 * size - 2, width = 4, fill = switchboard.env$mainColors[4])
  tcltk::tkcreate(aCanvas, "text", 80 * size - 5, 80 * size - 4, text = label, anchor = "se", font = paste(switchboard.env$font, floor(9 * size / 1.2)), fill = switchboard.env$mainColors[3])
  tcltk::tkcreate(aCanvas, "rectangle", -5, -5, -5, -5, width = 5, tags = "theMean", outline = switchboard.env$mainColors[3])
  tcltk::tkcreate(aCanvas, "line", -5, -5, -5, -5, width = 2, tags = "theSD", fill = switchboard.env$mainColors[3])

  tcltk::.Tcl(paste("set", paste0(aCanvas[1], "Switch"), 1))
  if(switch == TRUE) {
    tcltk::.Tcl(paste("set", paste0(aCanvas[1], "Switch"), 0))
    stateSwitch <- tcltk::ttkcheckbutton(aCanvas, text = "", style = "MicroSwitch", variable = paste0(aCanvas[1], "Switch"))
    tcltk::tkcreate(aCanvas, "window", 80 * size - 5, 5, anchor = "ne", window = stateSwitch)
  }

  return (aCanvas)
}

#' helper function that updates the canvas items of a eavesdropper widget
#' @keywords internal
eavesdropper_update <- function(.switchboard,
                                eavesdrop = NULL,
                                minimum = " ",
                                maximum = " ",
                                size = 1,
                                updates = 1,
                                plotMean = FALSE,
                                ...) {

  if(switchboard.env$totalIterations %% updates == 0) {
    aCanvas <- paste0(.switchboard$theSwitchboard, ".", .switchboard$theWidget)
    if(as.numeric(tcltk::tclvalue(paste0(aCanvas, "Switch")))) {
      theY <- value_to_coordinate(eavesdrop, minimum, maximum, size, c(-1, 6))
      tcltk::.Tcl(paste("tcltk_eavesdropper_update", aCanvas, "movingPoint", size, theY, switchboard.env$rangeColors[floor(theY/size)]))
      tcltk::.Tcl(paste("tcltk_plotMean_Y", aCanvas, "theMean", "theSD", "movingPoint", plotMean))
    }
  }

}


