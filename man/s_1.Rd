\name{s}
\alias{s}
\title{ Generate an s-set for FAST-method }
\description{
  Generates a number of equally spaced values between -pi/2 and
  pi/2. The number is determined by the number of runs required for
  the FAST method for a number of runs.
}
\usage{
s(m, factor = 1, cukier = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{ number of frequencies/parameters}
  \item{factor}{ the length of the returned vector is the minimum
  number required for FAST time \code{factor} }
  \item{cukier}{ boolean. Indicates wheter to use \code{\link{freq_cukier}}
  or \code{\link{freq_mcrae82}}}
}
\value{
 A vector of equally spaced values between -pi/2 and pi/2
}
\author{ Dominik Reusser }
\seealso{\link{fast} }
\examples{
  s(4)
  diff(s(3))
}
\keyword{ utilities }
