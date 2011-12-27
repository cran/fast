\name{example_model2}
\alias{example_model2}
\alias{example_model1}
\title{ Functions to demonstrate FAST}
\description{
Model 1 depends on a flexible number of parameters. It is documented in
Saltelli and Sobol 1995 and Davis and Rabinowith 1984.

Model 2 depends on 4 parameters.  It produces a weighted sum of the
4 parameters and returns this sum.  The weights depend on an
additional parameter x=1:200.
}
\usage{
example_model1(par,a, output=c("model", "analytical sensitivities"))
example_model2(p, fig=FALSE)
}
\arguments{
  \item{par}{A vector with parameters}
  \item{a}{A vector with a values of the same length as par. \code{a} is a measure for the importance of each parameter, with highest importance for a=0, non important parameters for a=9 and neglegible parameters for a=99}
  \item{output}{A character indicating whether to return the model results or the analytical parameter sensitivities}
  \item{p}{ A vector of 4 parameters}
  \item{fig}{ boolean: Plot the model(x) }
}
\value{
A vector of the weighted sum of parameters.
}
\references{
Saltelli, Andrea, and Ilya M Sobol. "About the use of rank transformation in sensitivity analysis of model output." Reliability Engineering & System Safety 50, no. 3 (1995): 225-239.

Davis, P. J. & Rabinowitz, P., Methods of Numerical Integration, 2nd edition, Academic Press, New York, 1984.
}
\author{ Dominik Reusser }
\seealso{  \code{\link{fast}}, \code{\link{sensitivity_rep}}}
\examples{
example_model1(par=c(0.5,0.5,0.5),a=c(1,1,1))

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
