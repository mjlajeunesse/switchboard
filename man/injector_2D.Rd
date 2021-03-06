% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/injector_2D.R
\name{injector_2D}
\alias{injector_2D}
\title{Inject and eavesdrop two continuous variables on a bivariate plot.}
\arguments{
\item{eavesdrop}{A vector of two variables to track on a bivariate plot. For
example, \code{eavesdrop = c(A, B)} would result on A being plotted on X-axis
(independent variable) and B on Y-axis (dependent variable).}

\item{minimum}{A vector of the two minimum values of variables in \code{eavesdrop}. For
example, \code{eavesdrop = c(A, B)} then \code{minimum = c(C, D)} are the
minimum values of A and B, respectively. They define plot boundaries.}

\item{maximum}{A vector of the two maximum values of variables in \code{eavesdrop}. For
example, \code{eavesdrop = c(A, B)} then \code{maximum = c(C, D)} are the
maximum values of A and B, respectively. They define plot boundaries.}

\item{inject}{A vector of the two strings for each variable name to be
modified/injected by widgets. For example, if \code{eavesdrop = = c(A, B)}
then \code{inject = c("A", "B")}.}

\item{label}{A vector of the two short strings designating labels/captions
for each of in \code{eavesdrop}.}

\item{size}{A number used to designate the size (magnification) of the
widget. The default is set to \code{1} which is 80 by 80 pixels. For
example, setting to \code{3} will results in a widget 3-times the default
size (240 by 240 pixels) and will occupy a grid area of 3 by 3.}

\item{placeOnGrid}{A row by column coordinate (e.g., \code{c(row-number, column-number)})
of a grid that designates the position to draw the widget on the
\code{switchboard}. Use \code{showGrid()} to help organize widget placement
on dashboard. The default places the first widget in pipe chain to
the \code{c(1, 1)} position, and all following on the same row stacked to
the right.}

\item{updates}{The number of times the widget is to be updated (e.g., when
it be modified/changed). The default updates the widget 100 times. Increase
number for smoother progress bar animation.}

\item{forget}{A time-delay in milliseconds for when displayed points on a
widget will be deleted.}

\item{delay}{Pause each update of the switchboard. Default has no delay,
values are in seconds (e.g., \code{delay = 0.01} results in 0.01 second
delay with each iteration).}

\item{plotRegression}{Display a switchboard-estimated regression line on a widget.}

\item{plotSampleSize}{Display the number items (N) displayed within a widget.}

\item{switch}{Display an on/off switch on a widget that controls widget updates.
When \code{TRUE} it will add the switch in the off-state on the switchboard.
The user must activate the switch to start the widget. The simulation proceeds
even if the switch in in the off-state.}
}
\value{
Nothing.
}
\description{
The \code{injector_2D} widget displays a bivariate window of data with two
sliderd that injects/modifies characteristics of the data. The plotted data
have a half-life and are deleted once their timer (parameter \code{forget})
expires. For the univariate version use \code{injector}. The number of data
points in the window can be throttled with \code{forget} or \code{delay}.
Options also include automatic regression (\code{plotRegression}) and an the
sample size of the number of data points currently displayed (\code{plotSampleSize}).
}
\section{Usage}{

   \preformatted{injector_2D(eavesdrop = c(NULL, NULL), minimum = c(1, 1),
   maximum = c(1, 1), inject = c("", "") label = c("", ""), size = 1,
   placeOnGrid = c(1, 1), updates = 100, forget = 400, delay = 0,
   plotRegression = FALSE, plotSampleSize = FALSE, switch = FALSE)}
}

\examples{
\dontrun{
     
     populationMean = 0
     populationSD = 1
     for (i in 1:4000) {
       randomNormal <- rnorm(1, populationMean, populationSD)
       switchboard(delay = 0.01) \%>\%
         injector_2D(c(populationSD, randomNormal),
                     inject = c("populationSD", "populationMean"),
                     minimum = c(0.1, - 5),
                     maximum = c(3.1, 5),
                     label = c("pop mean", "pop SD"),
                     forget = 2000)
     }
     switchboard_close()

}

}
\seealso{
Other eavesdroppers: 
\code{\link{benchmark}()},
\code{\link{counter_tally}()},
\code{\link{counter}()},
\code{\link{eavesdropper_2D}()},
\code{\link{eavesdropper_X}()},
\code{\link{eavesdropper}()},
\code{\link{injector_X}()},
\code{\link{injector}()},
\code{\link{number_pair}()},
\code{\link{number_quartet}()},
\code{\link{number_trio}()},
\code{\link{number}()},
\code{\link{progress_benchmark}()}

Other injectors: 
\code{\link{control_slider_Y_pair}()},
\code{\link{control_slider_Y}()},
\code{\link{control_slider_pair}()},
\code{\link{control_slider}()},
\code{\link{control_switch_pair}()},
\code{\link{control_switch_trio}()},
\code{\link{control_switch}()},
\code{\link{injector_X}()},
\code{\link{injector}()}

Other moving windows: 
\code{\link{eavesdropper_2D}()},
\code{\link{eavesdropper_X}()},
\code{\link{eavesdropper}()},
\code{\link{injector_X}()},
\code{\link{injector}()}
}
\concept{eavesdroppers}
\concept{injectors}
\concept{moving windows}
