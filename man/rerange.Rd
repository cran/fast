\name{rerange}
\alias{rerange}
\title{ Linear transformation of data }
\description{
 The function performs a linear transformation of the data, such that
 afterwards range(data)=c(theMin,theMax).
}
\usage{
rerange(data, min.goal = 0, max.goal = 1, min.data =
                 min(data, na.rm=na.rm), max.data = max(data, na.rm=na.rm), 
                 center = NA, na.rm=FALSE)
}
\arguments{
  \item{data}{ vector with the \code{data} to transform }
  \item{min.goal}{ new minimum value }
  \item{max.goal}{ new maximum value }
  \item{min.data}{ old minimum value }
  \item{max.data}{ old maximum value }
  \item{center}{which old value should become the new center ( (max.goal + min.goal) / 2 )}
  \item{na.rm}{ boolean, passed to \code{\link{min}} and \code{\link{max}} }
}
\value{
  vector with the transformed data
}
\author{ Dominik Reusser }
\examples{
   rerange(data=1:20)
   rerange(data=1:30, center=5)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }
