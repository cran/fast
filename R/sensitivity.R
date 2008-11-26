`sensitivity` <-
function(x, numberf, order=4,make.plot=FALSE, plot.max=FALSE, include.total.variance=FALSE, plot.main="", cukier=TRUE, names=paste(sep="", "P", 1:numberf)){
if(cukier){
        t.runs= min_runs_cukier75[numberf]
        t.freq = freq_cukier(numberf)
} else {
        t.runs <- min_runs_mcrae82[numberf]
        t.freq <- freq_mcrae82(numberf)
}
if(NROW(x)<t.runs){
cat("x is too short. Expected number of values: ", t.runs,
    "\n but found ", NROW(x), " values")
    return
}
y<- na2mean(x)
ff <- abs(fft(double_serie(y)))/NROW(x)
#Debug-Message 
#cat("Frequencies: ", t.freq, "Using data up to ", (NROW(x)+1), "\n")
ff <- ff[1:(NROW(x)+1)]

#Frequenzen und deren vielfache 
# McRae garantiert unabh. bis Ordnung 4
freq <- t.freq %o% 1:order

if(make.plot){
freqcol= rep(1:numberf,order)+1
if(plot.max){
 plot(c(1,NROW(ff))-1,c(0,plot.max),t="n", xlab="frequency", ylab="Fourier Coef", main=plot.main)
} else {
 plot(c(1,NROW(ff))-1,c(0,max(ff[-1])),t="n", xlab="frequency", ylab="Fourier Coef", main=plot.main)
}
 points((1:NROW(ff))-1,ff)
points(as.vector(freq), ff[as.vector(1+freq)], col=freqcol)
legend("topright", inset=0.1, names, pch=1, col=(1:numberf)+1)
}

#Sensitivitaeten berechnen
sens<-function(frequency){
sum((ff[drop(frequency)])^2, na.rm=TRUE)
}
# Sensitivitäten für Frequenzen berechnen
# Zu beachten, dass fft[1] dem A0 entspricht
# und nicht der Frequenz 1. Für Total auch A0 wegnehmen?

total<-sum(ff[-1]^2)
#browser()
#total<-sum(ff^2)
#total<-1
toReturn <- apply(1 +freq, 1, sens)/total
if(include.total.variance) toReturn <- c(toReturn ,total)
return(toReturn)

}

