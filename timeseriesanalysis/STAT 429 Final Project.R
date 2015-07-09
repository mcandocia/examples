#STAT 429 Final Project

#bacon data
library(rdatamarket)
bacon_url='http://data.is/188XaQt'
bacon_base=dmseries(bacon_url)

plot(bacon)#note a change about 2/5 into the data in overall shape

bacon=bacon_base[233:length(bacon_base),]#composite: 301; changes to 300 after diff
acf(bacon)
pacf(bacon)

bacon=diff(bacon)#needs to be differenced because of acf

plot(bacon)

acf(as.numeric(bacon),lag.max=70)#peaks at 1, 2, and 20 (and 21)
pacf(as.numeric(bacon),lag.max=50)#peaks at 1, 2, and 20

bacon=as.ts(bacon)

#check for linear trend
t=1:(length(bacon))
bacon_lm=lm(bacon~t)
summary(bacon_lm)#both coefficients found to be insignificant (p-values around 0.2-0.3)

bacon_save=bacon#for easy reversions to time series
bacon=bacon_save#see above

###SEASONAL AREA
N=length(bacon)

blist=list()
aics=numeric(9)
bics=numeric(9)
for (P in 0:2)
for (Q in 0:2){
n=P*3+Q+1
blist[[n]]=arima(bacon,order=c(0,0,0),
seasonal=list(order=c(P,0,Q),period=24))
aics[n]=blist[[n]]$aic
bics[n]=aics[n]-2*(P+Q)+log(N)*(P+Q)
}

par(mfrow=c(2,1))
plot(aics,type='o')
plot(bics,type='o')

#it looks like the BIC for SARIMA(...)x(2,0,1) has a BIC of 24.166 less than 
#the BIC of SARIMA(...)x(0,0,0); let's see if this removes the seasonal trend

bacons=blist[[9]]$residuals#the biennial seasonal trend is removed

acf(as.numeric(bacons),lag.max=70)
pacf(as.numeric(bacons),lag.max=70)

#since the ACF of the series is large at lag=1 and smaller but still significant
#at lag=2, it's quite possibly an AR(1) process; the PACF shows a stronger 
#correlation at lags 1 and 2, so it may be an MA(1) process.
#for completeness, ARMA(p,q), for (p)>(0-3), (q)>(0-3) will be explored

bacons=bacon#seasonal effect isn't that big

baconstr=bacons[1:266]

modlist=list()
aics=numeric(16)
bics=numeric(16)
aiccs=numeric(16)
for (p in 0:3)
for (q in 0:3){
	k=p+q
	n=4*p+q+1
	modlist[[n]]=Arima(baconstr,order=c(p,0,q))
	aics[n]=modlist[[n]]$aic
	bics[n]=aics[n]-2*(p+q)+log(N)*(p+q)
	aiccs[n]=aics[n]+2*k*(k+1)/(N-k-1)
}
orders=paste0("ARMA(",rep(0:3,rep(4,4)),",",rep(0:3,4),")")

par(mfrow=c(3,1))
plot(aics,type='o',ylab="AIC",axes=FALSE,main="AIC vs. model",frame.plot=TRUE,xlab="")
Axis(side=1, labels=FALSE,at='n')
Axis(side=2, labels=TRUE)
text(aics/2+mean(aics)/2+1,labels=orders,cex=0.6)

plot(bics,type='o',ylab="BIC",axes=FALSE,main="BIC vs. model",frame.plot=TRUE,xlab="")
Axis(side=1, labels=FALSE,at='n')
Axis(side=2, labels=TRUE)
text(bics/2+mean(bics)/2+1,labels=orders,cex=0.6)

plot(aiccs,type='o',ylab="AICc",axes=FALSE,main="AICc vs. model",frame.plot=TRUE,xlab="")
Axis(side=1, labels=FALSE,at='n')
Axis(side=2, labels=TRUE)
text(aiccs/2+mean(aiccs)/2+1,labels=orders,cex=0.6)


bacon_eacf=eacf(bacons)

#looks like ARMA(3,0) is coming out ahead initially

0.95*N
#285, so use the first 285 points to predict 286-300 (15 total)

library(forecast)


fc=forecast.Arima(modlist[[10]],15)

prune15<-function(obj){
	n=length(obj$residuals)
	obj$residuals=as.ts(obj$residuals[1:(n-15)])
	obj$x=as.ts(obj$x[1:(n-15)])
	obj
}


last=286:300
rss=numeric(16)
for (p in 0:3)
for (q in 0:3){
	k=p+q
	n=4*p+q+1
	#modlist[[n]]=Arima(bacons,order=c(p,0,q))
	#aics[n]=modlist[[n]]$aic
	#bics[n]=aics[n]-2*(p+q)+log(N)*(p+q)
	#aiccs[n]=aics[n]+2*k*(k+1)/(N-k-1)
	
	pruned_model=prune15(modlist[[n]])
	fc=forecast.Arima(pruned_model,15)#trims the last 15 points
	rss[n]=sum((bacons[last]-as.numeric(fc$mean)))^2
	
}

rss2=numeric(16)
first=1:285
modlist2=list()
for (p in 0:3)
for (q in 0:3){
	k=p+q
	n=4*p+q+1
	modlist2[[n]]=Arima(bacons[first],order=c(p,0,q))

	
	fc=forecast.Arima(modlist2[[n]],15)
	rss2[n]=sum((bacons[last]-as.numeric(fc$mean)))^2
}

plot(rss2,type='o',ylab="RSS",axes=FALSE,main="Prediction RSS vs. model"
,frame.plot=TRUE,xlab="")
Axis(side=1, labels=FALSE,at='n')
Axis(side=2, labels=TRUE)
text(rss2/2+mean(rss2)/2,labels=orders,cex=0.8,srt=270)

fc=forecast.Arima(modlist[[2]],14)

library(ggplot2)

times=time(bacons)
latter_times=times[last]
fcd=as.data.frame(fc)
cust_times=c("","","October 1941","","","January 1942","","","April 1942","","","July 1942","","")
colnames(fcd)[c(1,4,5)]<-c("Est","Upper95","Lower95")
ggplot()+geom_line(aes(x=latter_times,y=bacons[last],color='black'))+
geom_line(aes(x=latter_times,y=fcd$Est,color='blue'))+
geom_ribbon(aes(x=latter_times,ymin=fcd$Lower95,ymax=fcd$Upper95,fill='red',alpha=0.15))+
guides(alpha="none",fill="none",color=guide_legend(title="Series"))+
scale_color_identity("Series",guide="legend",labels=c("Original","Forecasted"))+
xlab("Date")+ylab("Change in bacon price in cents per pound")+
ggtitle("Predictions of bacon time series using MA(1) model")+
scale_x_continuous(breaks=latter_times,labels=cust_times)





#####SPECTRAL ANALYSIS#########

bts=as.numeric(baconstr)
spec.pgram(bts,taper=0.0,fast=FALSE)#it's already composite enough
spec.pgram(bts,taper=0.0,spans=c(2,2),fast=FALSE)
spec.pgram(bts,taper=0.0,spans=c(7,7),fast=FALSE)
spec.pgram(bts,taper=0.0,spans=c(14,14),fast=FALSE)

co1=coef(modlist[[2]])
co2=coef(modlist[[3]])
co3=coef(modlist[[6]])

arma.spec(co1,main="MA(1) Spectral Density")
arma.spec(co2,main="MA(2) Spectral Density")
arma.spec(co3,main="ARMA(1,1) Spectral Density")


pdf.options(pointsize=7)
Sweatex(filename)

filename="STAT429Final"
setwd("C:/Users/maxcan2/Desktop/Dropbox/STAT 429")
Sweatex<-function(filename,extension='Rnw',command='pdflatex',silent=FALSE,preview=FALSE)
{
if (command=='latex') command='simpdftex latex --maxpfb'
extension<-paste('.',extension,sep='')
path=options('latexcmd')[[1]]
path=substr(path,start=1,stop=nchar(path)-5)
Sweave(paste(filename,extension,sep=''))
system(paste(path,command,' ',filename,sep=''),intern=silent)
# if (command=='latex')
# {
# system(paste(path,'dvipdfm',' ',filename,sep=''))
# }
if (preview)
{
system(paste(options('pdfviewer')[[1]],' ',filename,'.pdf',sep=''))
}
}


