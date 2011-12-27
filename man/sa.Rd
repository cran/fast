\name{sa}
\alias{sa}
\title{
Calculate sensitivities
}
\description{
 sa provides a general interface to sensitivity analysis, similar to \code{\link{optim}}.
}
\usage{
sa(par, fn, method = c("FAST"), ..., xval = NULL)
}
\arguments{
  \item{par}{
 A matrix with to columns. The first giving the lower, the second giving the upper bound to the parameters of fn.
}
  \item{fn}{
   Function for which to analyise sensitivities, with first
          argument the vector of parameters for which to analyze sensitivities.
}
  \item{method}{
  Currently, only fast is available.
}
  \item{\dots}{
 Additional arguments passed to fn
}
  \item{xval}{
 Values for the x-axis if fn returnes a vector.
}
}
\details{
Let me know what details you would like to know (email to the maintainer)!
}
\value{
A vector of sensitivities
}
\references{
Reusser, Dominik E., Wouter Buytaert, and Erwin Zehe. "Temporal dynamics of model parameter sensitivity for computationally expensive models with FAST (Fourier Amplitude Sensitivity Test)." Water Resources Research 47 (2011): W07551.
}
\author{
Dominik Reusser
}

\seealso{
See Also as \code{\link{sensitivity}}, \code{\link{sensitivity_rep}} for the internally used functions.
}
\examples{
#Does the same as the example in sensitivity_rep but with less code

sa(par=matrix(c(0,0,0,0,  1,2,2,3), ncol=2), fn=example_model2)

}
\keyword{ util }
\keyword{ ~sensitivity }
