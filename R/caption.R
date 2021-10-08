#' A caption widget
#'
#' The \code{caption} widget displays a small title caption and smaller subtitle
#' caption.
#'
#' @section Usage:
#'    \preformatted{caption(c("", ""), size = 1, placeOnGrid = c(1,1))}
#'
#' @param eavesdrop Two strings of the caption widget as c("Title", "subTitle").
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
#' @param extendRow An integer describing the number of columns a row should 
#'     extend through. Extends the width of the widget but not it's height. Used to  
#'     better organize text along a row within \code{caption} widget. For example,
#'     when \code{size = 1} and \code{extendRow = 2} the caption widget will extend
#'     to two columns rather than one, but still have a height of one.
#'
#' @return Nothing.
#'
#' @examples \dontrun{
#'
#'    for(i in 1:250) {
#'      switchboard(delay = 0.01) %>%
#'        caption(c("A Title", "A small sub title"))
#'    }
#'    switchboard_close()
#'
#' }
#'
#' @family caption widgets
#' @name caption
NULL


#' @inheritParams caption
#' @import tcltk magrittr
#' @export
caption <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = caption_construct(.switchboard, ...),
                     updater = NULL, ...)
}


#' helper function that constructs canvas items of a caption widget
#' @keywords internal
caption_construct <- function(.switchboard,
                              eavesdrop = c(NULL, NULL),
							  extendRow = 1,
                              size = 1,
                              ...) {

  aFrame <- tcltk::tkframe(.switchboard, width = 80 * size * extendRow, height = 80 * size)
  aTitle <- tcltk::tktext(aFrame, borderwidth = 0, font = "Arial 12", wrap = "word",
                          foreground = switchboard.env$mainColors[4])
  tcltk::tcl("pack", aTitle, "-in", aFrame, "-fill", "y", "-expand", 1)
  tcltk::tcl("pack", "propagate", aFrame, 0)
  tcltk::tkinsert(aTitle, "end", eavesdrop[1])
  tcltk::tkinsert(aTitle, "end",
                  paste0("\n\n",
                         ifelse(is.na(eavesdrop[2]), "", eavesdrop[2])))
  tcltk::tktag.add(aTitle, "sub", paste0("1.", nchar(eavesdrop[1])), "end")
  tcltk::tktag.configure(aTitle, "sub", font = "Arial 8",
                         foreground = switchboard.env$mainColors[3])

  return (aFrame)
}
