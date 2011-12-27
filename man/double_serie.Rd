\name{double_serie}
\alias{double_serie}
\title{ Double the length of a data series for the FAST algoritm }
\description{
This function is used internally for the FAST-algorithm. It duplicates
the length of a data series assuming that the second part is a mirror
image of the first part
}
\usage{
double_serie(x)
}
\arguments{
  \item{x}{ \code{x} is the data series to make longer  }
}
\details{
   This function reverses the model output series from a 
   number of model runs for the FAST analysis and appends it to the
   original series. The last element of the existing series is not
   duplicated during this process.

   This is required in order to process the model run results for the
   FAST analysis with the \code{ \link{fft}} function.
}
\value{
   If x=c(1,2,3,4) the returned vector is
   c(1,2,3,4,3,2,1)
}
\references{ cukier 1978 }
\author{ Dominik Reusser }
\examples{
x=c(1,2,3,4)
double_serie(x)
}
\keyword{ utilities }
