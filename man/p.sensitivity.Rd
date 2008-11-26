\name{p.sensitivity}
\alias{p.sensitivity}
\title{ Plot a vector (e.g. time series) of sensitivities  }
\description{
  This function plots the results from the \code{\link{sensitivity_rep}} function.
}
\usage{
p.sensitivity(sen, xval, legend, range =
1:NROW(sen), col = 1:NROW(sen), smooth = rep(FALSE, NROW(range)),
x.range = 1:length(xval), m.max = max(sen[range, ],
na.rm = TRUE),limits=rep(FALSE,NROW(range)), ...)
}
\arguments{
  \item{sen}{ returned object from \code{\link{sensitivity_rep}} }
  \item{xval}{ Data defining the position on the x-axis. Usually a vector of POSIX-Dates }
  \item{legend}{ Entries for the legend text }
  \item{range}{ List of parameter-sensitivies to plot }
  \item{col}{ Vector of colors for parameters}
  \item{smooth}{ Vector of TRUE or FALSE values. Sensitivity series
  are smooted using a LOWES smoother if TRUE.  }
  \item{x.range}{ Range of x-values to plot. }
  \item{m.max}{ Maximum value for the y-axis }
  \item{limits}{ Vector of limits for sensitivities. Ploted as \ldots If FALSE, the limit for the corresponding series is ignored}
  \item{\dots}{ Parameters passed to the plot-function }
}
\details{
}
\value{
   This value is used for its plot-function.
}
\references{  }
\author{ Dominik Reusser }
\note{ 
}
\seealso{   \code{\link{sensitivity_rep}} }
\examples{
}
\keyword{ hplot }
\keyword{ utilities }
