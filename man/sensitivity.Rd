\name{sensitivity}
\alias{sensitivity}
\title{Calculate sensitivity according to the FAST algorithm}
\description{
 sensitivity calculates the sensitivity from
 a series of model outputs (\code{x}) according
 to the FAST alogrithm. 

}
\usage{
sensitivity(x, numberf, order = 4, make.plot = FALSE, show.legend
                 = TRUE, plot.max = max(ff[-1]), include.total.variance
                 = FALSE, cukier = TRUE, names = paste(sep = "", "P",
                 1:numberf), main = "", xlab = "frequency", ylab =
                 "Fourier Coef", pch = rep(0, numberf), col =
                 (1:numberf) + 1, reorder = 1:numberf, ...)
}
\arguments{
  \item{x}{ A vector of model outputs where parameters vary between
  runs according to the fast algorithm. }
  \item{numberf}{ Number of parameters varied. }
  \item{order}{ Order of parameter frequency independence (see Cukier)   }
  \item{make.plot}{ plot the Fourier spectrum? }
  \item{plot.max}{ xmax in the spectrum }
  \item{include.total.variance}{ include the sum of all variances in the result list.}
  \item{pch}{ see \code{\link{par}}}
  \item{col}{ see \code{\link{par}}}
  \item{...}{ Additional parameters passed to plot}
  \item{xlab}{ see \code{\link{par}}}
  \item{ylab}{ see \code{\link{par}}}
  \item{main}{ Title for the plot }
  \item{show.legend}{ Boolean indicating whether to plot the legend }
  \item{names}{ A vector of parameter names.}
  \item{cukier}{ boolean: Calculate FAST-parameters according to Cukier 1975  or
  McRae 1982}
  \item{reorder}{A vector of indices that allows to use a different order for the parameters. This is important to check effects of the sampling scheme on results. Use the same as for \code{\link{fast_parameters}}}
}
\value{
  A list of the partial variance accounted for by each parameter.
}
\references{ 
Reusser, Dominik E., Wouter Buytaert, and Erwin Zehe. "Temporal dynamics of model parameter sensitivity for computationally expensive models with FAST (Fourier Amplitude Sensitivity Test)." Water Resources Research 47 (2011): W07551.

CUKIER, R. I.; SCHAIBLY, J. H. & SHULER, K. E. Study Of Sensitivity Of Coupled Reaction Systems To Uncertainties In Rate Coefficients .3. Analysis Of Approximations Journal Of Chemical Physics, 1975 , 63 , 1140-1149

McRae, G.; Tilden, J. & Seinfeld, J. Global sensitivity analysis - a computational implementation of the Fourier amplitude sensitivity test (FAST) Comput. Chem. Eng., 1982 , 6 , 15-25

}
\author{ Dominik Reusser }
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
