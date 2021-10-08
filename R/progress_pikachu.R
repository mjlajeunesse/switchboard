#' Display pikachu as a progress bar.
#'
#' The \code{progress_pikachu} widget displays pikachu as a progressBar. He's the best.
#'
#' @section Usage:
#'    \preformatted{progress_pikachu(eavesdrop, maximum = 100, size = 1,
#'    placeOnGrid = c(1, 1), updates = 100, delay = 0, honest = FALSE,
#'    closeAtMaximum = FALSE, fill = "horizontal")}
#'
#' @param eavesdrop The variable to track with a progress bar.
#' @param maximum The maximum value of \code{eavesdrop} that marks the end of
#'    what to progress track.
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
#' @param honest When \code{TRUE}, it updates the widget by the true progression value.
#'     The default (\code{FALSE}) has a cosmetic modification to the progression
#'    value that helps update it in a prettier way.
#' @param closeAtMaximum Functions like \code{switchboard_close()} by closing
#'      the switchboard window when the eavesdropped value equals maximum. NOTE:
#'      if a widget has \code{closeAtMaximum = TRUE}, then this widget MUST
#'      be placed at the end (i.e., last widget) of the pipe chain.
#' @param fill The direction of how things are animated when displaying
#'      progression. The default is \code{horizontal}, which tracks progression
#'      from left to right (maximum), but \code{vertical} can also be used for progress to
#'      occur in a bottom to up (maximum) animation.
#'
#' @return Nothing.
#'
#' @examples \dontrun{
#'     
#'     for (i in 1:250) {
#'       switchboard(delay = 0.01) %>%
#'         progress_pikachu(i, maximum = 250)
#'     }
#'     switchboard_close()
#'
#' }
#'
#' @family progress bars
#' @name progress_pikachu
NULL


#' @inheritParams progress_pikachu
#' @import tcltk magrittr
#' @export
progress_pikachu <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = progress_image_construct(.switchboard,
                                                            file = system.file("images", "pikachu.png", package = "switchboard"), ...),
                     updater = progress_image_update(.switchboard, ...), ...)
}
