#' Inject and eavesdrop two continuous variables on a bivariate plot.
#'
#' The \code{injector_2D} widget displays a bivariate window of data with two
#' sliderd that injects/modifies characteristics of the data. The plotted data
#' have a half-life and are deleted once their timer (parameter \code{forget})
#' expires. For the univariate version use \code{injector}. The number of data
#' points in the window can be throttled with \code{forget} or \code{delay}.
#' Options also include automatic regression (\code{plotRegression}) and an the
#' sample size of the number of data points currently displayed (\code{plotSampleSize}).
#'
#' @section Usage:
#'    \preformatted{injector_2D(eavesdrop = c(NULL, NULL), minimum = c(1, 1),
#'    maximum = c(1, 1), inject = c("", "") label = c("", ""), size = 1,
#'    placeOnGrid = c(1, 1), updates = 100, forget = 400, delay = 0,
#'    plotRegression = FALSE, plotSampleSize = FALSE, switch = FALSE)}
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
#' @param inject A vector of the two strings for each variable name to be
#'    modified/injected by widgets. For example, if \code{eavesdrop = = c(A, B)}
#'    then \code{inject = c("A", "B")}.
#' @param label A vector of the two short strings designating labels/captions
#'    for each of in \code{eavesdrop}.
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
#'      populationMean = 0
#'      populationSD = 1
#'      for (i in 1:4000) {
#'        randomNormal <- rnorm(1, populationMean, populationSD)
#'        switchboard(delay = 0.01) %>%
#'          injector_2D(c(populationSD, randomNormal),
#'                      inject = c("populationSD", "populationMean"),
#'                      minimum = c(0.1, - 5),
#'                      maximum = c(3.1, 5),
#'                      label = c("pop mean", "pop SD"),
#'                      forget = 2000)
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family eavesdroppers
#' @family injectors
#' @family moving windows
#' @name injector_2D
NULL


#' @inheritParams injector_2D
#' @import tcltk magrittr
#' @export
injector_2D <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = injector_2D_construct(.switchboard, ...),
                     updater = injector_2D_update(.switchboard, ...), ...)
}


#' helper function that constructs canvas items of a injector_2D widget
#' @keywords internal
injector_2D_construct <- function(.switchboard,
                                  eavesdrop = c(NULL, NULL),
                                  inject = c("", ""),
                                  minimum = c(" ", " "),
                                  maximum = c(" ", " "),
                                  label = c(" ", " "),
                                  size = 1,
                                  switch = FALSE,
                                  ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)

  if(inject[1] != "") {
    if(as.integer(tcltk::tcl("info", "exists", inject[1])) == 1) tcltk::tcl("unset", inject[1])
    tcltk::.Tcl(paste("set", inject[1], mget(inject[1], envir = parent.frame(n = 3), ifnotfound = NA)))
    injectorScaleX <- tcltk::ttkscale(aCanvas, from = maximum[1], to = minimum[1], variable = inject[1], value = as.numeric(tcltk::tclvalue(inject[1])),
                                      command = function(...){assign(inject[1], as.numeric(tcltk::tclvalue(inject[1])), envir = parent.frame(n = 5));})
    tcltk::tkcreate(aCanvas, "window", 5, 80 * size + 8, width = 80 * size - 10, anchor = "sw", window = injectorScaleX)
  }

  if(inject[2] != "") {
    if(as.integer(tcltk::tcl("info", "exists", inject[2])) == 1) tcltk::tcl("unset", inject[2])
    tcltk::.Tcl(paste("set", inject[2], mget(inject[2], envir = parent.frame(n = 3), ifnotfound = NA)))
    injectorScaleY <- tcltk::ttkscale(aCanvas, from = maximum[2], to = minimum[2], orient = "vertical", variable = inject[2], value = as.numeric(tcltk::tclvalue(inject[2])),
                                      command = function(...){assign(inject[2], as.numeric(tcltk::tclvalue(inject[2])), envir = parent.frame(n = 5));})
    tcltk::tkcreate(aCanvas, "window", 12, 5, height = 80 * size - 10, anchor = "ne", window = injectorScaleY)
  }

  tcltk::tkcreate(aCanvas, "line", 2, 5, 2, 80 * size - 2, 80 * size - 5, 80 * size - 2, width = 4, fill = switchboard.env$mainColors[4])
  tcltk::tkcreate(aCanvas, "text", 80 * size - 5, 80 * size - 14, text = label[1], anchor = "se", font = paste(switchboard.env$font, floor(9 * size / 1.2)), fill = switchboard.env$mainColors[3])
  tcltk::tkcreate(aCanvas, "text", 19 + 8 * size, 5, text = label[2], angle = 90, anchor = "se", font = paste(switchboard.env$font, floor(9 * size / 1.2)), fill = switchboard.env$mainColors[3])
  tcltk::tkcreate(aCanvas, "text", 40 * size, 5, text = "", anchor = "ne", font = paste(switchboard.env$font, floor(6 * size / 1.2)), fill = switchboard.env$mainColors[4], tags = "N")
  tcltk::tkcreate(aCanvas, "line", -5, -5, -5, -5, width = 2, fill = switchboard.env$mainColors[3], tags = "slope")

  tcltk::.Tcl(paste("set", paste0(aCanvas[1], "Switch"), 1))
  if(switch == TRUE) {
    tcltk::.Tcl(paste("set", paste0(aCanvas[1], "Switch"), 0))
    stateSwitch <- tcltk::ttkcheckbutton(aCanvas, text = "", style = "MicroSwitch", variable = paste0(aCanvas[1], "Switch"))
    tcltk::tkcreate(aCanvas, "window", 80 * size - 5, 5, anchor = "ne", window = stateSwitch)
  }

  return (aCanvas)
}




#' helper function that updates the canvas items of a injector_2D widget
#' @keywords internal
injector_2D_update <- function(.switchboard,
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
      tcltk::.Tcl(paste("tcltk_injector_2D_update", aCanvas, "timedPoint", as.integer(forget), size, theX, theY, switchboard.env$rangeColors[floor(theY/size)]))
      tcltk::.Tcl(paste("tcltk_plotN", aCanvas, "N", "timedPoint", plotSampleSize))
      tcltk::.Tcl(paste("tcltk_plotRegression", aCanvas, "slope", "timedPoint", size, plotRegression))
    }
  }

}
