`sensitivity_rep` <-
function(data, xval, direction, numberf, order=4, legend = paste("P",1:order, sep=""), cukier=TRUE){
sen <- apply(data,direction,sensitivity, numberf=numberf, order=order, cukier=cukier, make.plot=FALSE)
p.sensitivity(sen=sen, xval=xval, legend=legend)
return(sen)
}

