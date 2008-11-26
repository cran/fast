\name{sensitivity}
\alias{sensitivity}
\title{Calculate sensitivity according to the FAST algorithm}
\description{
 This function calculates the sensitivity from
 a series of model outputs (\code{x}) according
 to the FAST alogrithm. 
}
\usage{
sensitivity(x, numberf, order = 4, make.plot = FALSE, plot.max = FALSE, include.total.variance = FALSE, plot.main = "", cukier=TRUE, names=paste(sep="", "P", 1:numberf) )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ A vector of model outputs where parameters vary between
  runs according to the fast algorithm. }
  \item{numberf}{ Number of parameters varied. }
  \item{order}{ Order of parameter frequency independence (see Cukier)   }
  \item{make.plot}{ plot the Fourier spectrum? }
  \item{plot.max}{ xmax in the spectrum }
  \item{include.total.variance}{ include the sum of all variances in the result list.}
  \item{plot.main}{ Title for the plot }
  \item{names}{ A vector of parameter names.}
  \item{cukier}{ boolean: Calculate FAST-parameters according to Cukier 1975  or
  McRae 1982}
}
\details{
}
\value{
  A list of the partial variance accounted for by each parameter.
}
\references{ 
CUKIER, R. I.; SCHAIBLY, J. H. & SHULER, K. E. Study Of Sensitivity Of Coupled Reaction Systems To Uncertainties In Rate Coefficients .3. Analysis Of Approximations Journal Of Chemical Physics, 1975 , 63 , 1140-1149

McRae, G.; Tilden, J. & Seinfeld, J. Global sensitivity analysis - a computational implementation of the Fourier amplitude sensitivity test (FAST) Comput. Chem. Eng., 1982 , 6 , 15-25

}
\author{ Dominik Reusser }
\note{ 

}
\seealso{  \code{\link{S}}, \code{\link{fast}} }
\examples{
example_model1<-function(p,x){
   return(p[1]*x+p[2]*(1-x))
}
paras<-fast_parameters(min=c(0,0,0),max=c(1,2,2))
paras
model_results <- apply(paras, 1, example_model1, x=0.5)
model_results
sensitivity <- sensitivity(x=model_results, numberf=3, make.plot=TRUE)
sensitivity
}
\keyword{ utilities }
