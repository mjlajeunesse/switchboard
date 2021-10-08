#' Initialize a switchboard window
#'
#' The \code{switchboard} initializes a switchboard window, then using \code{\%>\%}
#' you can pipe in various widgets into this window to create a dashboard.
#'
#' @param title The title of the switchboard Tcl/Tk window. Switchboards hide
#'    the titlebar, but is can be un-hidden by adding a title (e.g., \code{title = "my new title"}.
#' @param delay Pause each update of the switchboard. Default has no delay,
#'    values are in seconds (e.g., \code{delay = 0.01} results in 0.01 second
#'    delay with each iteration).
#' @param skip Skips updating the switchboard. The default does not skip updates,
#'    values are in number of iterations to skip (e.g., \code{skip = 50} results
#'    in updates only occurring after every 50 iterations).
#' @param padX Horizontal padding in pixels between widgets.
#' @param padY Vertical padding in pixels between widgets.
#' @param font The primary font used in all widgets, default is "Helvetica".
#' @param mainColors The color set used in all widgets. It is a vector of four
#'    hex colors with each element corresponding to: c("main color", "background",
#'    "text color", "alt text color").
#'
#' @return NULL
#'
#' @examples \dontrun{
#'
#'    for(i in 1:100)
#'       switchboard(delay = 0.05) %>%
#'       progress_ibis(i, maximum = 100, closeAtMaximum = TRUE)
#'
#' }
#'
#' @family dashboard
#' @import tcltk magrittr utils grDevices
#' @export
switchboard <- function(title = "hidden title",
                        delay = 0,
                        skip = 0,
                        padX = 10,
                        padY = 0,
                        font = "Helvetica",
                        mainColors = c("#007fff",
                                       "#ffffff",
                                       "#666666",
                                       "#cccccc")) {

  .switchboard <- get_switchboardVariable(title)

  if(!is.na(.switchboard)) { # update switchboard window

    switchboard.env$totalIterations <- switchboard.env$totalIterations + 1

    # when userCancelled == 1 skip updating switchboard
    if(switchboard.env$userCancelled != 0) return(NULL)

    # only update switchboard on each skip'th step
    if((skip != 0) && ((switchboard.env$totalIterations %% skip) != 0)) return(NULL)

    # return the ID of main window and first widget in pipe chain
    # NOTE: theWidget set/skip to 3 since window already has 2 children (drag & exit buttons)
    .switchboard <- list("theSwitchboard" = .switchboard, "theWidget" = 3)
    class(.switchboard) <- "switchboardWidget"

    # pause simulation by delay time in seconds
    if(delay != 0) Sys.sleep(delay)

  } else { # construct switchboard window

    # first check if there are 'withdrawn' switchboards and remove them
    for(aWindow in as.character(tcltk::tkwinfo("children", ".")))
      if(as.numeric(tcltk::tkwinfo("viewable", aWindow)) == 0) tcltk::tcl("destroy", aWindow)

    #####################################
    # START: construct switchboard window

    .switchboard <- tcltk::tktoplevel(padx = 15, pady = 15)
    tcltk::tkwm.title(.switchboard, title)
    tcltk::tcl("wm", "attributes", .switchboard, "-topmost", TRUE)
    if(title == "hidden title") tcltk::tcl("wm", "overrideredirect", .switchboard, 1)

    # Special protocol to 'withdraw' switchboard window rather than close
    # and break simulation run. NOTE: withdrawn window(s) are only destroyed
    # when a new switchboard is constructed.
    tcltk::tkwm.protocol(.switchboard,
                         "WM_DELETE_WINDOW",
                         function() {
                           if(switchboard.env$userCancelled == 0) {
                             switchboard.env$userCancelled <- 1
                             tcltk::tcl("wm", "withdraw", .switchboard)
                             # withdrawn windows can be resurrected with 'deiconify'
                           }
                           else {
                             switchboard_close()
                             tcltk::tkdestroy(.switchboard)
                           }
                         })

    # END: construct switchboard window
    ###################################

    # initialize switchboard environment variables
    set_switchboardVariable(title, .switchboard$ID)
    set_switchboardVariable("userCancelled", 0)
    set_switchboardVariable("totalIterations", 1)

    set_switchboardVariable("font", font)
    set_switchboardVariable("mainColors", mainColors)
    set_switchboardVariable("rangeColors",
                            grDevices::colorRampPalette(c(switchboard.env$mainColors[1],
                                               switchboard.env$mainColors[2]))(100))
    set_switchboardVariable("rangeColorsAlt",
                            grDevices::colorRampPalette(c(switchboard.env$mainColors[1],
                                               switchboard.env$mainColors[4]))(100))
    set_switchboardVariable("padX", padX)
    set_switchboardVariable("padY", padY)

    set_switchboardVariable("adjustX", c(-5, 15))
    set_switchboardVariable("adjustY", c(-10, 5))

    set_switchboardVariable("dragX", 0)
    set_switchboardVariable("dragY", 0)
    set_switchboardVariable("winPosition", c(0,0))

    # initialize switchboard drag
    dragWindow <- tcltk::tkbutton(.switchboard,
                                  image = tcltk::tcl("image", "create", "photo"),
                                  height = 500, width = 500, relief = "flat",
                                  background = switchboard.env$mainColors[2],
                                  activebackground = switchboard.env$mainColors[2])
    tcltk::tkbind(.switchboard, "<Configure>",
                  function() {
                    windowInfo <- as.character(tcltk::tkwm.geometry(.switchboard))
                    resize <- unlist(regmatches(windowInfo, gregexpr("[[:digit:]]+", windowInfo)))[1:2]
                    tcltk::tkconfigure(dragWindow,
                                       height = as.numeric(resize[2]),
                                       width = as.numeric(resize[1]))
                  })
    tcltk::tkbind(dragWindow, "<Button-1>", function(x,y) {
      switchboard.env$dragX <- as.numeric(tcltk::tkwinfo("pointerx", "."))
      switchboard.env$dragY <- as.numeric(tcltk::tkwinfo("pointery", "."))
      windowInfo <- as.character(tcltk::tkwm.geometry(.switchboard))
      switchboard.env$winPosition <- as.numeric(unlist(regmatches(windowInfo, gregexpr("[[:digit:]]+", windowInfo)))[3:4])
    })
    tcltk::tkbind(dragWindow, "<Button1-Motion>", function(x,y) {
      dragToX <- switchboard.env$winPosition[1] + as.numeric(tcltk::tkwinfo("pointerx", ".")) - switchboard.env$dragX
      dragToY <- switchboard.env$winPosition[2] + as.numeric(tcltk::tkwinfo("pointery", ".")) - switchboard.env$dragY
      tcltk::tkwm.geometry(.switchboard, paste0("+", dragToX, "+", dragToY))
    })
    tcltk::tkplace(dragWindow, relx = 0, rely = 0, x = -15, y = -15)

    # initialize switchboard's custom exit button
    exitButton <- tcltk::tkbutton(.switchboard, text = "X",
                                  font = "Arial 6", height = 0,
                                  width = 1, relief = "flat",
                                  foreground = switchboard.env$mainColors[2],
                                  background = switchboard.env$mainColors[2],
                                  command = function() {
                                    if(switchboard.env$userCancelled == 0) {
                                      switchboard.env$userCancelled <- 1
                                      tcltk::tcl("wm", "withdraw", .switchboard)
                                    }
                                    else switchboard_close()
                                  })
    tcltk::tkbind(exitButton, "<Enter>",
           function() tcltk::tkconfigure(exitButton, background = switchboard.env$mainColors[1]))
    tcltk::tkbind(exitButton, "<Leave>",
           function() tcltk::tkconfigure(exitButton, background = switchboard.env$mainColors[2]))
    tcltk::tkplace(exitButton, relx = 1, rely = 0, x = -5, y = -10)


  }

  # exit as tkwin class when constructing but as list when updating
  return(.switchboard)
}


#' widget grid and dashboard layout helper
#' @param aCanvas Tcl/Tk pathName for a canvas widget
#' @param placeOnGrid The x-y grid coordinates for where to place on window
#' @param size User specified size of the widget
#' @param extendRow Used when widget is long but not wide in grid space
#' @noRd
placeOnSwitchboard <- function(aCanvas,
                               placeOnGrid = c(NULL, NULL),
                               size = 1,
                               extendRow = 1) {

  if(is.null(placeOnGrid[1]) & is.null(placeOnGrid[2])) {
    tcltk::tkpack(aCanvas,
                  side = "left",
                  anchor = "nw",
                  padx = switchboard.env$padX,
                  pady = switchboard.env$padY)
  } else {
    if(extendRow != 1) sizeModified <- extendRow
    else sizeModified <- size
    tcltk::tkgrid(aCanvas,
                  row = placeOnGrid[1],
                  column = placeOnGrid[2],
                  columnspan = sizeModified,
                  rowspan = size,
                  padx = switchboard.env$padX,
                  pady = switchboard.env$padY)
  }
  return(NULL)
}


#' Widget engine to construct/update/layout piped items on switchboard
#' @param .switchboard A parent switchboard window. It is defined when \code{layout}
#'    is piped to a switchboard with \code{\%>\%}. If \code{layout} is called without a
#'    parent \code{switchboard()}, it will create it's own internally. NOTE:
#'    \code{switchboard} manages this parameter and you should not modify it.
#' @param eavesdrop The variable(s) to display or track within a widget.
#' @param inject Variable names(s) as strings of variables to be modified by widgets.
#' @param minimum The minimum value of \code{eavesdrop}.
#' @param maximum The maximum value of \code{eavesdrop}.
#' @param label The caption/label(s) of the widget.
#' @param size A number used to designate the size (magnification) of the
#'    widget. The default is set to \code{1} which is 80 by 80 pixels. For
#'    example, setting to \code{3} will results in a widget 3-times the default
#'    size (240 by 240 pixels) and will occupy a grid area of 3 by 3.
#' @param placeOnGrid A row by column coordinate (e.g., \code{c(row-number, column-number)})
#'    of a grid that designates the position to draw the widget on the
#'    \code{switchboard}. Use \code{layout()} to help organize widget placement
#'    on dashboard. The default places the first widget in pipe chain to
#'    the \code{c(1, 1)} position, and all following on the same row stacked to
#'    the right.
#' @param extendRow Extends the width of a widget and not the height. Used to better 
#'     organize text within \code{caption} widget.
#' @param updates The number of times the widget is to be updated (e.g., when
#'     it be modified/changed). The default updates the widget 100 times.
#' @param digits The number of digits to display in a widget.
#' @param forget A time-delay in millisecond for when displayed points on a
#'     widget will be deleted.
#' @param delay Pause each update of the switchboard. Default has no delay,
#'    values are in seconds (e.g., \code{delay = 0.01} results in 0.01 second
#'    delay with each iteration).
#' @param plotRegression Display a switchboard-estimated regression line on a widget.
#' @param plotSampleSize Display the number items (N) displayed within a widget.
#' @param plotMean Display a switchboard-estimated mean and standard deviation
#'     whisker plot on a widget.
#' @param switch Display an on/off switch on a widget that controls the
#'     updating of the widget. When \code{TRUE} it will add the switch in the
#'     off-state on the switchboard. The user must activate the switch to start
#'     the widget.
#' @param honest When \code{TRUE}, it updates the widget by the true percentage.
#'     The default (\code{FALSE}) has a cosmetic modification to the percentage
#'    value that helps update it in a prettier way.
#' @param closeAtMaximum Functions like \code{switchboard_close} by closing
#'      the switchboard window when the eavesdropped value equals maximum. NOTE:
#'      if a widget has \code{closeAtMaximum = TRUE}, then this widget MUST
#'      be placed at the end (i.e., last widget) of the pipe chain.
#' @param fill The direction of how things are animated when displaying
#'      progression. The default is \code{horizontal}, which tracks progression
#'      from left to right, but \code{vertical} can also be used for progress to
#'      occur in a bottom to up animation.
#' @param file A *.png filename with a transparent background that designates an
#'      image to be used as a progress-bar. The total image size should be
#'      80 by 80 pixels, but for best integration into switchboard layout, the
#'      actual image must be 75 by 75 pixels placed at the bottom left of the
#'      80 by 80 image. This will leave a 5 pixel whitespace at both the top
#'      and right side of the image.
#' @param benchmark The numerical value that will trigger a visual benchmark
#'      on a widget.
#' @param constructor A widget-specific function that contains the Tcl/Tk script to
#'      build the widget.
#' @param updater A widget-specific function that contains the Tcl/Tk script to
#'      update the constructed widget.
#' @keywords internal
switchboard_engine <- function(.switchboard,
                               eavesdrop = c(NULL, NULL),
                               inject = c("", ""),
                               minimum = c(" ", " "),
                               maximum = c(" ", " "),
                               label = c(" ", " "),
                               size = 1,
                               placeOnGrid = c(NULL, NULL),
							   extendRow = 1,
                               updates = 1,
                               digits = 5,
                               forget = 400,
                               delay = 0,
                               plotRegression = FALSE,
                               plotSampleSize = FALSE,
                               plotMean = FALSE,
                               switch = FALSE,
                               honest = TRUE,
                               closeAtMaximum = FALSE,
                               fill = "horizontal",
                               file = "",
                               benchmark = NA,
                               constructor,
                               updater) {

  placeholderClass <- class(.switchboard)

  # check if called by itself and not part of pipe chain
  if(placeholderClass == "numeric" | placeholderClass == "integer") {
    eavesdrop <- .switchboard
    .switchboard <- switchboard(delay = delay)
    placeholderClass <- class(.switchboard)
    closeAtMaximum <- TRUE
  }

  if(placeholderClass == "switchboardWidget") { # update switchboard widget

    force(updater)
    .switchboard$theWidget <- .switchboard$theWidget + 1

  } else if(placeholderClass == "NULL") { # skip updating switchboard widget

    return(NULL)

  } else if(placeholderClass == "tkwin") { # construct switchboard widget

    placeOnSwitchboard(force(constructor), placeOnGrid, ceiling(size), extendRow)

  }

  return(.switchboard)
}
