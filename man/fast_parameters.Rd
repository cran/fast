
\name{fast_parameters}
\alias{fast_parameters}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generate a parameter set for the FAST method}
\description{
  This function generates an array of parameters for the FAST method. 
}
\usage{
fast_parameters(minimum, maximum, names=paste(sep="", "P", 1:n),factor=1,logscale=rep(FALSE, n), cukier=TRUE, reorder=1:n)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{minimum}{ Vector of lower boundaries for parameters}
  \item{maximum}{ Vector of upper boundaries for parameters}
  \item{names}{ A vector of parameter names.}
  \item{factor}{ Create more values then the minimum requires. Passed to \code{\link{s}}}
  \item{logscale}{ A vector of booleans indicating whether a parameter is varied on a logarithmic scale. In this case, minimum and maximum are exponents}
  \item{cukier}{boolean. Indicates wheter to use \code{\link{freq_cukier}}
  or \code{\link{freq_mcrae82}}  }
  \item{reorder}{A vector of indices that allows to use a different order for the parameters. This is important to check effects of the sampling scheme on results}
}
\value{
An array of dimension \code{c(min_number_of_runs, n)}
}
\author{ Dominik Reusser }
\seealso{ \code{\link{fast_parameters}} }
\examples{
paras<-fast_parameters(minimum=c(0,0,0),maximum=c(1,2,2))
}
\keyword{ utilities }
\keyword{ datagen }
