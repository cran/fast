\name{S}
\alias{S}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generate S-set for FAST method}
\description{
  This function generates an array of values which provide the base
  for parameters for the FAST method. I is usually not used directly
  but called from \code{\link{fast_parameters}}
}
\usage{
S(m, factor = 1, cukier = TRUE, par.names = NULL, reorder = 1:m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{ Number of parameters/frequencies }
  \item{factor}{ Create more values then the minimum requires. Passed
  to \code{\link{s}}}
  \item{cukier}{boolean. Indicates wheter to use \code{\link{freq_cukier}}
  or \code{\link{freq_mcrae82}}  }
  \item{par.names}{ A vector of parameter names.}
  \item{reorder}{A vector of indices that allows to use a different order for the parameters. This is important to check effects of the sampling scheme on results}
}
\value{
An array of dimension \code{c(min_number_of_runs, n)}
}
\author{ Dominik Reusser }
\seealso{ \code{\link{fast_parameters}} }
\examples{
S(3)
}
\keyword{ utilities }
