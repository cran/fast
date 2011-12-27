\name{freq_cukier}
\alias{freq_cukier}
\alias{freq_mcrae82}
\alias{d_m_mcrae82}
\alias{omega_m_mcrae82}
\alias{omega_m_cukier75}
\alias{min_runs_cukier75}
\alias{min_runs_mcrae82}
\title{ Calculate independent frequencies according to CUKIER1975 or McRae1982}
\description{
   This function returns a vector of independent frequencies for usage
   in the \code{\link{fast}} method.
}
\usage{
freq_cukier(m, i = 1, omega_before = 0)
freq_mcrae82(m, i = 1, omega_before = 0)
}
\arguments{
  \item{m}{ Number of frequencies (parameters) needed. }
  \item{i}{ Used internally, recursion counter }
  \item{omega_before}{ Used internally, previous frequency. }
}
\value{
 A vector of independet frequencies to the order of 4.
}
\references{ CUKIER, R. I.; SCHAIBLY, J. H. & SHULER, K. E. Study Of Sensitivity Of Coupled Reaction Systems To Uncertainties In Rate Coefficients .3. Analysis Of Approximations Journal Of Chemical Physics, 1975 , 63 , 1140-1149

McRae, G.; Tilden, J. & Seinfeld, J. Global sensitivity analysis - a computational implementation of the Fourier amplitude sensitivity test (FAST) Comput. Chem. Eng., 1982 , 6 , 15-25

 }
\author{ Dominik Reusser }
\seealso{ \code{\link{fast}} }
\examples{
freq_cukier(5)
}
\keyword{ utilities }
\keyword{ datagen }
