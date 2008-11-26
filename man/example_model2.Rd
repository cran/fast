
\name{example_model2}
\alias{example_model2}
\title{ A function to demonstrate FAST}
\description{
The model depends on 4 parameters.  It produces a weighted sum of the
4 parameters and returns this sum.  The weights depend on an
additional parameter x=1:200.
}
\usage{
example_model2(p, fig=FALSE)
}
\arguments{
  \item{p}{ A vector of 4 parameters}
  \item{fig}{ boolean: Plot the model(x) }
}
\details{
}
\value{
A vector of the weighted sum of parameters.
}
\references{ }
\author{ Dominik Reusser }
\note{ 

}
\seealso{  \code{\link{fast}}, \code{\link{sensitivity_rep}}}
\examples{
#The model depends on 4 parameters
#
#It produces a weighted sum of the 4 parameters and returns this sum
#
#The weights depend on an additional parameter x=1:200 
example_model2(p=c(1,3,1,1),fig=TRUE)
example_model2(p=c(1,2,2,3),fig=TRUE)
}
\keyword{ utilities }
\keyword{ datagen }
