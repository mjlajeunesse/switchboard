% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/showGrid.R
\name{showGrid}
\alias{showGrid}
\title{Display a layout grid on the switchboard.}
\arguments{
\item{nrows}{Number of rows to plot on grid.}

\item{ncolumns}{Number of columns to plot on grid.}
}
\value{
Nothing.
}
\description{
The \code{showGrid} widget displays a collection of buttons positioned
along a grid to help devise where you would like to organize/place multiple
widgets on a switchboard. Buttons contain the row by column coordinates of the
grid. NOTE: You can click on any button to copy-to-clipboard the small
coordinate script. You can then paste in your switchboard widget
\code{placeOnGrid} option (e.g., \code{placeOnGrid = c(1, 2)}).
}
\section{Usage}{

   \preformatted{showGrid(nrows = 4, ncolumns = 4)}
}

\examples{
\dontrun{
     
     for (i in 1:250) {
       switchboard(delay = 0.01) \%>\%
         progress_ring(i, maximum = 250, placeOnGrid = c(1,1)) \%>\%
         progress_ring(i, maximum = 250, placeOnGrid = c(2,2)) \%>\%
         progress_ring(i, maximum = 250, placeOnGrid = c(3,3)) \%>\%
         progress_ring(i, maximum = 250, placeOnGrid = c(4,4)) \%>\%
         showGrid()
     }
     switchboard_close()

}

}
\seealso{
Other dashboard: 
\code{\link{switchboard_close}()},
\code{\link{switchboard}()}
}
\concept{dashboard}
