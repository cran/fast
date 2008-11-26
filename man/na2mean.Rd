\name{na2mean}
\alias{na2mean}
\title{ Replace NA in a vector by mean of neighboring values. }
\description{
  This method replaces NA-Values in a vector by the mean of the
  neighboring values. Example: c(1,NA,3) is converted to c(1,2,3).
}
\usage{
na2mean(x)
}
\arguments{
  \item{x}{ Vector to fill }
}
\details{
   The method does not work if multiple NA are in sequence.
}
\value{
Vector with NA replaced by mean. 
}
\references{ }
\author{ Dominik Reusser}
\note{}
\seealso{}
\examples{
na2mean(c(1,NA,2))
na2mean(c(1,NA,NA,2))
}
\keyword{ utilities }
