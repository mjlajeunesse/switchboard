#' Display a layout grid on the switchboard.
#'
#' The \code{showGrid} widget displays a collection of buttons positioned
#' along a grid to help devise where you would like to organize/place multiple
#' widgets on a switchboard. Buttons contain the row by column coordinates of the
#' grid. NOTE: You can click on any button to copy-to-clipboard the small
#' coordinate script. You can then paste in your switchboard widget
#' \code{placeOnGrid} option (e.g., \code{placeOnGrid = c(1, 2)}).
#'
#' @section Usage:
#'    \preformatted{showGrid(nrows = 4, ncolumns = 4)}
#'
#' @param nrows Number of rows to plot on grid.
#' @param ncolumns Number of columns to plot on grid.
#'
#' @return Nothing.
#'
#' @examples \dontrun{
#'      
#'      for (i in 1:250) {
#'        switchboard(delay = 0.01) %>%
#'          progress_ring(i, maximum = 250, placeOnGrid = c(1,1)) %>%
#'          progress_ring(i, maximum = 250, placeOnGrid = c(2,2)) %>%
#'          progress_ring(i, maximum = 250, placeOnGrid = c(3,3)) %>%
#'          progress_ring(i, maximum = 250, placeOnGrid = c(4,4)) %>%
#'          showGrid()
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family dashboard
#' @name showGrid
NULL


#' @inheritParams showGrid
#' @import tcltk magrittr
#' @export
showGrid <- function(.switchboard,
					nrows = 4,
					ncolumns = 4) {

  if(class(.switchboard) != "tkwin") return(.switchboard)

  for(j in 0:nrows) {
    for(i in 0:ncolumns) {
      aCoord <- paste0("(", j + 1, ", ", i + 1, ")")
      someButton <- tcltk::ttkbutton(.switchboard,
                                     width = 5,
                                     text = aCoord,
                                     command = function() utils::writeClipboard(charToRaw(paste0("c", aCoord, " "))))
      tcltk::tkplace(someButton, x = j * 80 + j * switchboard.env$padX * 2 + 20, y = i * 80 + i * switchboard.env$padY * 2 + 26)
     }
  }

  return(.switchboard)
}
