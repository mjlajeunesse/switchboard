#' A trio of switches to toggle on/off three simulation variables.
#'
#' The \code{control_switch_trio} widget displays three switches to toggle the state
#' (e.g., on or off, \code{TRUE} or \code{FALSE}) of three simulation variables.
#' A switch is blue when "on", and gray when "off".
#'
#' @section Usage:
#'    \preformatted{switch(inject = c("", "", ""), label = c("", "", ""),
#'    size = 1, placeOnGrid = c(1, 1))}
#'
#' @param inject A vector of the three strings for each variable name to be
#'    modified/injected by the switch widget. For example,
#'    \code{inject = c("A", "B", "C")}. These three variables should be in
#'    boolean form (e.g., 0 or 1, FALSE or TRUE).
#' @param label A vector of three short strings designating labels/captions
#'    for each switch.
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
#'      varToSlideB <- 0
#'      varToSlideC <- 1
#'      for (i in 1:500) {
#'        switchboard(delay = 0.01) %>%
#'          control_switch_trio(inject = c("varToSlideA", "varToSlideB", "varToSlideC"),
#'                              label = c("0 to 1", "0 to 1", "1 to 0")) %>%
#'          number_trio(c(varToSlideA, varToSlideB, varToSlideC))
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family injectors
#' @name control_switch_trio
NULL


#' @inheritParams control_switch_trio
#' @import tcltk magrittr
#' @export
control_switch_trio <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = control_switch_trio_construct(.switchboard, ...),
                     updater = NULL, ...)
}

#' helper function that constructs canvas items of a control_switch_trio widget
#' @keywords internal
control_switch_trio_construct <- function(.switchboard,
                                  inject = c("", "", ""),
                                  label = c("", "", ""),
                                  size = 1,
                                  ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)


  if(inject[1] != "") {
    if(as.integer(tcltk::tcl("info", "exists", inject[1])) == 1) tcltk::tcl("unset", inject[1])
    valueWithinLoop <- mget(inject[1], envir = parent.frame(n = 3), ifnotfound = NA)
    tcltk::.Tcl(paste("set", inject[1], ifelse(valueWithinLoop == FALSE, 0, 1)))
    injectorSwitch1 <- ttkcheckbutton(aCanvas, style = "Switch", variable = inject[1],
                                      command = function(...){
                                        theSwitchState <- as.numeric(tcltk::tclvalue(inject[1]))
                                        if(theSwitchState == 0) assign(inject[1], FALSE, envir = parent.frame(n = 5))
                                        else assign(inject[1], TRUE, envir = parent.frame(n = 5))
                                      })
    tcltk::tkcreate(aCanvas, "window", 20, 15, width = 40, anchor = "center", window = injectorSwitch1)
  }

  if(inject[2] != "") {
    if(as.integer(tcltk::tcl("info", "exists", inject[2])) == 1) tcltk::tcl("unset", inject[2])
    valueWithinLoop <- mget(inject[2], envir = parent.frame(n = 3), ifnotfound = NA)
    tcltk::.Tcl(paste("set", inject[2], ifelse(valueWithinLoop == FALSE, 0, 1)))
    injectorSwitch2 <- ttkcheckbutton(aCanvas, style = "Switch", variable = inject[2],
                                      command = function(...){
                                        theSwitchState <- as.numeric(tcltk::tclvalue(inject[2]))
                                        if(theSwitchState == 0) assign(inject[2], FALSE, envir = parent.frame(n = 5))
                                        else assign(inject[2], TRUE, envir = parent.frame(n = 5))
                                      })
    tcltk::tkcreate(aCanvas, "window", 20, 37.5 + 5, width = 40, anchor = "center", window = injectorSwitch2)
  }

  if(inject[3] != "") {
    if(as.integer(tcltk::tcl("info", "exists", inject[3])) == 1) tcltk::tcl("unset", inject[3])
    valueWithinLoop <- mget(inject[3], envir = parent.frame(n = 3), ifnotfound = NA)
    tcltk::.Tcl(paste("set", inject[3], ifelse(valueWithinLoop == FALSE, 0, 1)))
    injectorSwitch3 <- ttkcheckbutton(aCanvas, style = "Switch", variable = inject[3],
                                      command = function(...){
                                        theSwitchState <- as.numeric(tcltk::tclvalue(inject[3]))
                                        if(theSwitchState == 0) assign(inject[3], FALSE, envir = parent.frame(n = 5))
                                        else assign(inject[3], TRUE, envir = parent.frame(n = 5))
                                      })
    tcltk::tkcreate(aCanvas, "window", 20, 70, width = 40, anchor = "center", window = injectorSwitch3)
  }

  someTXT <- tcltk::tkcreate(aCanvas, "text", 43, 15 + 6, text = label[1], anchor = "sw", font = paste(switchboard.env$font, floor(9 / 1.2)), fill = switchboard.env$mainColors[4])
  tcltk::tkitemlower(aCanvas, injectorSwitch1, someTXT)
  tcltk::tkcreate(aCanvas, "text", 43, 43 + 6, text = label[2], anchor = "sw", font = paste(switchboard.env$font, floor(9 / 1.2)), fill = switchboard.env$mainColors[4])
  tcltk::tkcreate(aCanvas, "text", 43, 70 + 5, text = label[3], anchor = "sw", font = paste(switchboard.env$font, floor(9 / 1.2)), fill = switchboard.env$mainColors[4])


  return(aCanvas)
}

