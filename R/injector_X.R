#' Inject a continuous variable and display as conveyor belt.
#'
#' The \code{injector_X} widget displays a vertical moving window of data with
#' a slider that injects/modifies characteristics of the data. (see
#' \code{injector} for a horizontal version). The Y-axis is time-lagged
#' and the widget keeps track of each point until it reaches the end of the
#' plot. This widget is univariate and only eavesdrops one variable; for a
#' bivariate version use \code{injector_2D}. The number of data points in the
#' window can be throttled with \code{delay}.
#'
#' @section Usage:
#'    \preformatted{injector_X(eavesdrop, minimum = 1, maximum = 100, inject = "",
#'    label = "", size = 1, placeOnGrid = c(1, 1), updates = 100, delay = 0,
#'    plotMean = FALSE, switch = FALSE)}
#'
#' @param eavesdrop The variable to track.
#' @param minimum The minimum value of \code{eavesdrop}. It defines the plot boundary.
#' @param maximum The maximum value of \code{eavesdrop}. It defines the plot boundary.
#' @param inject String of the variable name to be modified/injected by widgets.
#'    For example, if \code{eavesdrop = i} then \code{inject = "i"}.
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
#'      populationMean = 0
#'      for (i in 1:4000) {
#'        randomNormal <- rnorm(1, populationMean, 1)
#'        switchboard(delay = 0.01) %>%
#'          injector_X(randomNormal, inject = "populationMean", minimum = -5, maximum = 5)
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family eavesdroppers
#' @family injectors
#' @family moving windows
#' @name injector_X
NULL


#' @inheritParams injector_X
#' @import tcltk magrittr
#' @export
injector_X <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = injector_X_construct(.switchboard, ...),
                     updater = injector_X_update(.switchboard, ...), ...)
}


#' helper function that constructs canvas items of a injector_X widget
#' @keywords internal
injector_X_construct <- function(.switchboard,
                                 label = " ",
                                 inject = "",
                                 minimum = " ",
                                 maximum = " ",
                                 switch = FALSE,
                                 size = 1,
                                 ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)
  tcltk::tkcreate(aCanvas, "line", 2, 5, 2, 80 * size - 2, 80 * size - 5, 80 * size - 2, width = 4, fill = switchboard.env$mainColors[4])

  if(inject != "") {
    if(as.integer(tcltk::tcl("info", "exists", inject)) == 1) tcltk::tcl("unset", inject)
    tcltk::.Tcl(paste("set", inject, mget(inject, envir = parent.frame(n = 3), ifnotfound = NA)))
    injectorScaleX <- tcltk::ttkscale(aCanvas, from = maximum, to = minimum, variable = inject, value = as.numeric(tcltk::tclvalue(inject)),
                                      command = function(...){assign(inject, as.numeric(tcltk::tclvalue(inject)), envir = parent.frame(n = 5));})
    tcltk::tkcreate(aCanvas, "window", 5, 80 * size + 8, width = 80 * size - 10, anchor = "sw", window = injectorScaleX)
  }
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


#' helper function that updates the canvas items of a injector_X widget
#' @keywords internal
injector_X_update <- function(.switchboard,
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
      theX <- value_to_coordinate(eavesdrop, minimum, maximum, size, c(-1, 5))
      tcltk::.Tcl(paste("tcltk_injector_X_update", aCanvas, "movingPoint", size, theX, switchboard.env$rangeColors[floor(theX/size)]))
      tcltk::.Tcl(paste("tcltk_plotMean_X", aCanvas, "theMean", "theSD", "movingPoint", size, plotMean))
    }
  }

}
