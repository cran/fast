\name{fast-package}
\alias{fast-package}
\alias{fast}
\docType{package}
\title{
Implementation of the Fourier Amplitute Sensitivity Test (FAST)
}
\description{
The Fourier Amplitute Sensitivity Test (FAST) is a method to
deterimine global sensitivities of a model on parameter changes with
relavtively few model runs. This package implements this sensitivity
analysis method.
}
\details{
\tabular{ll}{
Package: \tab fast\cr
Type: \tab Package\cr
Version: \tab 0.5\cr
Date: \tab 2007-12-15\cr
License: \tab GPL2\cr
}
Generate a set of parameter sets with the function \code{\link{fast_parameters}}. Run
your model with each parameter set. \code{\link{sensitivity}} then evaluates
the sensitivities of the model results on each of the parameters.
}
\author{
Dominik Reusser
Maintainer: Dominik Reusser <dreusser@uni-potsdam.de>
}
\references{
Reusser, Dominik E., Wouter Buytaert, and Erwin Zehe. "Temporal dynamics of model parameter sensitivity for computationally expensive models with FAST (Fourier Amplitude Sensitivity Test)." Water Resources Research 47 (2011): W07551.


CUKIER, R. I.; LEVINE, H. B. & SHULER, K. E. Non-Linear Sensitivity Analysis Of Multi-Parameter Model Systems Journal Of Computational Physics, 1978 , 26 , 1-42

CUKIER, R. I.; FORTUIN, C. M.; SHULER, K. E.; PETSCHEK, A. G. & SCHAIBLY, J. H. Study Of Sensitivity Of Coupled Reaction Systems To Uncertainties In Rate Coefficients .1. Theory Journal Of Chemical Physics, 1973 , 59 , 3873-3878

SCHAIBLY, J. H. & SHULER, K. E. Study Of Sensitivity Of Coupled Reaction Systems To Uncertainties In Rate Coefficients .2. Applications Journal Of Chemical Physics, 1973 , 59 , 3879-3888

CUKIER, R. I.; SCHAIBLY, J. H. & SHULER, K. E. Study Of Sensitivity Of Coupled Reaction Systems To Uncertainties In Rate Coefficients .3. Analysis Of Approximations Journal Of Chemical Physics, 1975 , 63 , 1140-1149

}
\keyword{ package }
\keyword{ utilities }
\keyword{ misc }
\keyword{ multivariate }

\examples{
#A simple model depending on two
#parameters and an additional
#"hyperparameter" x. Depending on
#x the model is sensitive to p[1] only (x=1) 
#or p[2] only (x=0) or both (0<x<1)

example_model1<-function(p,x){
   return(p[1]*x+p[2]*(1-x))
}
paras<-fast_parameters(minimum=c(0,0,0),maximum=c(1,1,1))
paras
model_results <- apply(paras, 1, example_model1, x=0.5)
plot(model_results)
sensitivity <- sensitivity(x=model_results, numberf=3, make.plot=TRUE)
sensitivity


#In the second example, sensitivities are calculated for
#200 model results (which might be a time series).
#
#The model depends on 4 parameters
#
#It produces a weighted sum of the 4 parameters and returns this sum
#
#The weights depend on an additional parameter x=1:200 

example_model2(p=c(1,3,1,1),fig=TRUE)
example_model2(p=c(1,2,2,3),fig=TRUE)
paras<-fast_parameters(min=c(0,0,0,0),max=c(1,2,2,3))
paras
model_results <- apply(paras, 1, example_model2)
plot(model_results)
x11()
sensitivity <- sensitivity_rep(data = model_results, xval=1:200, direction = 1, order=4 , numberf=4)
p.sensitivity(sen=sensitivity, xval=1:200, legend=names(paras))

}
