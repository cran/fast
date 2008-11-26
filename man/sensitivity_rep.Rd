\name{sensitivity_rep}
\alias{sensitivity_rep}
\title{ Repeat calculation of sensitivities for lots of data }
\description{
  This function calculates the sensitivity for a series of data, e.g.
  a time series. 
}
\usage{
sensitivity_rep(data, xval, direction, numberf, order=4, legend = paste("P", 1:order, sep = ""), cukier=TRUE)
}
\arguments{
  \item{data}{ Array of data to use of the form todo }
  \item{xval}{ Vector to use as x data for plotting }
  \item{direction}{ Dimension which contains the todo }
  \item{numberf}{ Number of parameters used  }
  \item{order}{Order of parameter frequency independence (see Cukier1975)  }
  \item{legend}{ legend text to plot }
  \item{cukier}{ boolean. Indicates wheter to use \code{\link{freq_cukier}}}
}
\details{
}
\value{
  An array of sensitivities of the form ... 
}
\references{ }
\author{ Dominik Reusser }
\note{ 

}
\seealso{  \code{\link{fast}}, \code{\link{sensitivity}}}
\examples{
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
model_results
sensitivity <- sensitivity_rep(data = model_results, xval=1:200, direction = 1, order=4 , numberf=4)
p.sensitivity(sen=sensitivity, xval=1:200, legend=names(paras))

}
\keyword{ utilities }