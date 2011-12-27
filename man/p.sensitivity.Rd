\name{p.sensitivity}
\alias{p.sensitivity}
\title{ Plot a vector (e.g. time series) of sensitivities  }
\description{
  This function plots the results from the \code{\link{sensitivity_rep}} function.
}
\usage{
p.sensitivity(sen, xval, legend, legend.cex = 0.5, range =
                 1:NROW(sen), col = 1:NROW(sen), lty = rep(1,
                 NROW(sen)), smooth = rep(FALSE, NROW(range)), x.range
                 = 1:length(xval), m.max = max(sen[range, ], na.rm =
                 TRUE), limits = rep(FALSE, NROW(range)), xlab =
                 "time", ylab = "Sensitivity", ...)
}
\arguments{
  \item{sen}{ returned object from \code{\link{sensitivity_rep}} }
  \item{xval}{ Data defining the position on the x-axis. Usually a vector of POSIX-Dates }
  \item{legend}{ Entries for the legend text }
  \item{legend.cex}{ Size of the legend }
  \item{range}{ List of parameter-sensitivies to plot }
  \item{col}{ Vector of colors for parameters}
  \item{smooth}{ Vector of TRUE or FALSE values. Sensitivity series
  are smooted using a LOWES smoother if TRUE.}
  
  \item{lty}{ see \code{\link{par}}}
  \item{xlab}{ see \code{\link{par}}}
  \item{ylab}{ see \code{\link{par}}}
  \item{x.range}{ Range of x-values to plot. }
  \item{m.max}{ Maximum value for the y-axis }
  \item{limits}{ Vector of limits for sensitivities. Ploted as \ldots If FALSE, the limit for the corresponding series is ignored}
  \item{\dots}{ Parameters passed to the plot-function }
}
\value{
   This value is used for its plot-function.
}
\author{ Dominik Reusser }
\seealso{   \code{\link{sensitivity_rep}} }
\keyword{ hplot }
\keyword{ utilities }
