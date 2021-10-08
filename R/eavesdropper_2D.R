#' Eavesdrop two continuous variables on a bivariate plot.
#'
#' The \code{eavesdropper_2D} widget displays a bivariate window of data. The
#' plotted data have a half-life and are deleted once their timer (parameter
#' \code{forget}) expires. For the univariate version use \code{eavesdropper}.
#' The number of data points in the window can be throttled with \code{forget} or
#' \code{delay}. Options also include automatic regression (\code{plotRegression})
#' and an the sample size of the number of data points currently
#' displayed (\code{plotSampleSize}).
#'
#' @section Usage:
#'    \preformatted{eavesdropper_2D(eavesdrop = c(NULL, NULL), minimum = c(1, 1),
#'    maximum = c(1, 1), label = c("", ""), size = 1, placeOnGrid = c(1, 1),
#'    updates = 100, forget = 400, delay = 0, plotRegression = FALSE,
#'    plotSampleSize = FALSE, switch = FALSE)}
#'
#' @param eavesdrop A vector of two variables to track on a bivariate plot. For
#'    example, \code{eavesdrop = c(A, B)} would result on A being plotted on X-axis
#'    (independent variable) and B on Y-axis (dependent variable).
#' @param minimum A vector of the two minimum values of variables in \code{eavesdrop}. For
#'    example, \code{eavesdrop = c(A, B)} then \code{minimum = c(C, D)} are the
#'    minimum values of A and B, respectively. They define plot boundaries.
#' @param maximum A vector of the two maximum values of variables in \code{eavesdrop}. For
#'    example, \code{eavesdrop = c(A, B)} then \code{maximum = c(C, D)} are the
#'    maximum values of A and B, respectively. They define plot boundaries.
#' @param label A vector of two short strings designating labels/captions
#'    for each variable used in \code{eavesdrop}.
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
#' @param forget A time-delay in milliseconds for when displayed points on a
#'     widget will be deleted.
#' @param delay Pause each update of the switchboard. Default has no delay,
#'    values are in seconds (e.g., \code{delay = 0.01} results in 0.01 second
#'    delay with each iteration).
#' @param plotRegression Display a switchboard-estimated regression line on a widget.
#' @param plotSampleSize Display the number items (N) displayed within a widget.
#' @param switch Display an on/off switch on a widget that controls widget updates.
#'     When \code{TRUE} it will add the switch in the off-state on the switchboard.
#'     The user must activate the switch to start the widget. The simulation proceeds
#'     even if the switch in in the off-state.
#'
#' @return Nothing.
#'
#' @examples \dontrun{
#'      
#'      for (i in 1:10000) {
#'        x <- cos(i/400) 
#'        y <- sin(2 * i/400) / 2 
#'        switchboard(skip = 4) %>%
#'          eavesdropper_2D(c(x, y), minimum = c(-1, -1), maximum = c(1, 1), forget = 100)
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family moving windows
#' @family eavesdroppers
#' @name eavesdropper_2D
NULL


#' @inheritParams eavesdropper_2D
#' @import tcltk magrittr
#' @export
eavesdropper_2D <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = eavesdropper_2D_construct(.switchboard, ...),
                     updater = eavesdropper_2D_update(.switchboard, ...), ...)
}


#' helper function that constructs canvas items of a eavesdropper_2D widget
#' @keywords internal
eavesdropper_2D_construct <- function(.switchboard,
                                      eavesdrop = c(NULL, NULL),
                                      minimum = c(" ", " "),
                                      maximum = c(" ", " "),
                                      label = c(" ", " "),
                                      size = 1,
                                      switch = FALSE,
                                      ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)

  tcltk::tkcreate(aCanvas, "line", 2, 5, 2, 80 * size - 2, 80 * size - 5, 80 * size - 2, width = 4, fill = switchboard.env$mainColors[4])
  tcltk::tkcreate(aCanvas, "text", 80 * size - 5, 80 * size - 14, text = label[1], anchor = "se", font = paste(switchboard.env$font, floor(9 * size / 1.2)), fill = switchboard.env$mainColors[3])
  tcltk::tkcreate(aCanvas, "text", 19 + 8 * size, 5, text = label[2], angle = 90, anchor = "se", font = paste(switchboard.env$font, floor(9 * size / 1.2)), fill = switchboard.env$mainColors[3])
  tcltk::tkcreate(aCanvas, "text", 70 * size, 18, text = "", anchor = "se", font = paste(switchboard.env$font, floor(6 * size / 1.2)), fill = switchboard.env$mainColors[4], tags = "N")
  tcltk::tkcreate(aCanvas, "line", -5, -5, -5, -5, width = 2, fill = switchboard.env$mainColors[3], tags = "slope")

  tcltk::.Tcl(paste("set", paste0(aCanvas[1], "Switch"), 1))
  if(switch == TRUE) {
    tcltk::.Tcl(paste("set", paste0(aCanvas[1], "Switch"), 0))
    stateSwitch <- tcltk::ttkcheckbutton(aCanvas, text = "", style = "MicroSwitch", variable = paste0(aCanvas[1], "Switch"))
    tcltk::tkcreate(aCanvas, "window", 80 * size - 5, 5, anchor = "ne", window = stateSwitch)
  }

  return (aCanvas)
}



#' helper function that updates the canvas items of a eavesdropper_2D widget
#' @keywords internal
eavesdropper_2D_update <- function(.switchboard,
                                   eavesdrop = c(NULL, NULL),
                                   minimum = c(" ", " "),
                                   maximum = c(" ", " "),
                                   size = 1,
                                   updates = 1,
                                   forget = 400,
                                   plotRegression = FALSE,
                                   plotSampleSize = FALSE,
                                   ...) {

  if(switchboard.env$totalIterations %% updates == 0) {
    aCanvas <- paste0(.switchboard$theSwitchboard, ".", .switchboard$theWidget)
    if(as.numeric(tcltk::tclvalue(paste0(aCanvas, "Switch")))) {
      theX <- value_to_coordinate(eavesdrop[1], minimum[1], maximum[1], size, switchboard.env$adjustX)
      theY <- value_to_coordinate(eavesdrop[2], minimum[2], maximum[2], size, switchboard.env$adjustY)
      tcltk::.Tcl(paste("tcltk_injector_2D_update", aCanvas, "timedPoint", forget, size, theX, theY, switchboard.env$rangeColors[floor(theY/size)]))
      tcltk::.Tcl(paste("tcltk_plotN", aCanvas, "N", "timedPoint", plotSampleSize))
      tcltk::.Tcl(paste("tcltk_plotRegression", aCanvas, "slope", "timedPoint", size, plotRegression))
    }
  }

}
