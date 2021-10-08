#' A switch to toggle on/off a simulation variable.
#'
#' The \code{control_switch} widget displays a switch to toggle the state (e.g., on or
#' off, TRUE or FALSE) of a simulation variable. A switch is blue when "on",
#' and gray when "off".
#'
#' @section Usage:
#'    \preformatted{control_switch(inject = "", label = "", size = 1, placeOnGrid = c(1, 1))}
#'
#' @param inject String of the variable name to be modified/injected by the
#'    switch widget. For example, \code{inject = "i"}. The variable should be in
#'    boolean form (e.g., 0 or 1, FALSE or TRUE).
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
#'      varToSwitch <- 0
#'      for (i in 1:500) {
#'        switchboard(delay = 0.01) %>%
#'          control_switch("varToSwitch", label = "0 to 1") %>%
#'          number(varToSwitch)
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family injectors
#' @name control_switch
NULL


#' @inheritParams control_switch
#' @import tcltk magrittr
#' @export
control_switch <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = control_switch_construct(.switchboard, ...),
                     updater = NULL, ...)
}


#' helper function that constructs canvas items of a control_switch widget
#' @keywords internal
control_switch_construct <- function(.switchboard,
                            inject = "",
                            label = " ",
                            size = 1,
                            ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)

  if(inject != "") {
    if(as.integer(tcltk::tcl("info", "exists", inject)) == 1) tcltk::tcl("unset", inject)
    valueWithinLoop <- mget(inject, envir = parent.frame(n = 3), ifnotfound = NA)
    tcltk::.Tcl(paste("set", inject, ifelse(valueWithinLoop == FALSE, 0, 1)))
    injectorSwitch <- tcltk::ttkcheckbutton(aCanvas, style = "Switch", variable = inject,
                                            command = function(...){
                                              theSwitchState <- as.numeric(tcltk::tclvalue(inject))
                                              if(theSwitchState == 0) assign(inject, FALSE, envir = parent.frame(n = 5))
                                              else assign(inject, TRUE, envir = parent.frame(n = 5))
                                            })
    tcltk::tkcreate(aCanvas, "window", 45 * size, 38.5 * size, anchor = "center", window = injectorSwitch)
  }

  tcltk::tkcreate(aCanvas, "text", 45 * size - 3, 38.5 * size + 20, text = label, anchor = "center", font = paste(switchboard.env$font, floor(9 / 1.2)), fill = switchboard.env$mainColors[4])

  return(aCanvas)
}
