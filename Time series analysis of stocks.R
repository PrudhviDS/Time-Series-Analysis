#extracting the data into ms excel and reading it in R 

data= read.csv(file.choose(),header=T)

head(data)
View(data)
tail(data)


#extracting, cleaning and plotting the adjusted closing prices  

d=na.omit(data)
sp=data.frame(data$Close)

adj_cl_values=as.numeric(paste(d$Close))
adj_cl_values
sp=adj_cl_values[!is.na(adj_cl_values)]
sp

plot.ts(sp,xlab="Number of Days",ylab="Adjusted Closing Prices(in INR)",
        main="Adjusted Closing Prices Plot for the last five years")

#install tseries 
install.packages("tseries")
library(tseries)
print(adf.test(sp))     # Augmented Dickey fuller test to test stationary

#transformation of the data applying log and diffrencing again to make it stationary and linear

d_sp=diff(sp)
log_sp=log(sp)
log_sp2=(log(sp))**2
d_log_sp=diff(log_sp,lag=1)

#time series plots

par(mfrow=c(2,2))
plot.ts(sp,main="Plot of Orignal Prices ", xlab="Number of Days",ylab="Orignal Adjusted Closing Prices ")
plot.ts(log_sp,main="Plot of Log Prices ", xlab="Number of Days",ylab="Log Prices ")
plot.ts(d_sp,main="Plot of Diffrenced Prices ", xlab="Number of Days",ylab="Differenced Prices ")
plot.ts(d_log_sp,main="Plot of Diffrenced Log Prices ", xlab="Number of Days",ylab="Differenced Log Prices ")

#Stationarity of the diffrenced log transformed data

print(adf.test(d_log_sp))


##ACF AND PACF PLOTS##

par(mfrow=c(2,2))
acf(log_sp,main="ACF of Log Prices",lag.max=100,ylim=c(-0.5,1))
pacf(log_sp,main="PACF of Log Prices ",lag.max=100,ylim=c(-0.5,1))
acf(d_log_sp,main="ACF of Diffrenced Log Prices ",lag.max=100,ylim=c(-0.5,1))
pacf(d_log_sp,main="PACF of Diffrenced Log Prices",lag.max=100,ylim=c(-0.5,1))



#Model Identification
#without the drift 


install.packages("forecast")

library(forecast)




#parameters estimation

arima_out=auto.arima(log_sp,trace=F)
arima_out
arima111 = arima_out
summary(arima111)
install.packages("lmtest")

library(lmtest)
coeftest(arima111)

#forecasting and forecasting errors

install.packages('forecast', dependencies = TRUE)
library(forecast)
log_forecast=forecast(arima111,100)
log_forecast
log_fc=as.numeric(log_forecast$mean)

fc=as.numeric(log_forecast$mean)
par(mfrow=c(1,1))
plot(log_forecast,main="Log Forecasts from Arima(1,1,1)",xlab = "Number of days ",ylab = "Log Adjusted Closed Prices")
legend("bottomright",inset=0.05,title="Legend",c("95% CI","80% CI"),fill=c("light Grey","Dark Grey"))       
accuracy(log_forecast)
#comparision of the original and the fitted stock prices 
log_fit_values=fitted (arima111) 
fit_Values=exp(log_fit_values)
par(mfrow=c(1,1))
plot.ts(sp,col="Red",main="Original Prices vs Fitted Prices Plot",xlab="Number of days",ylab="Adjusted Closed Prices")
lines(fit_Values,col="green")
legend("bottomright",lty=c(1,1),lwd=2,inset=0.5,title="legend",c("Orignal","Fitted"),col = c("Red","Green"),bty = "o")
#residual plot
res=resid(arima111)
par(mfrow=c(1,1))
plot(res,type="o",main="Residuals Plot of the ARIMA(1,1,1) model",xlab="Number of days", ylab="Residuals")


#Acf and Pacf plots
par(mfrow=c(2,1))
acf(res,main="ACF of the Residuals of the ARIMA(1,1,1) model",lag.max=100,ylim=c(-0.5,1))
pacf(res,main="PACF of the Residuals of the ARIMA(1,1,1) model",lag.max = 100,ylim=c(-0.5,1))
#QQ PLOT
par(mfrow=c(1,2))
qqnorm(res,main="Normal Q-Q plot(Residulas)")
qqline(res)
qqnorm(res^2,main="Normal Q-Q plot(Squared Residuals)")
qqline(res^2)
#Ljung- Box test 
Box.test(res,lag=10,type="Ljung-Box")
Box.test(res^2,lag=10,type="Ljung-Box")



acf(d_log_sp^2,main="ACF Squared returns ",lag.max = 100,ylim=c(-0.5,1))
pacf(d_log_sp^2,main="PACF Squared returns ",lag.max = 100,ylim=c(-0.5,1))
aic=c()
n = length(d_log_sp^2)
q = c(1:10)

z1=garch(d_log_sp^2,order=c(0,1))
z2=garch(d_log_sp^2,order=c(0,2))
z3=garch(d_log_sp^2,order=c(0,3))
z4=garch(d_log_sp^2,order=c(0,4))
z5=garch(d_log_sp^2,order=c(0,5))
z6=garch(d_log_sp^2,order=c(0,6))
z7=garch(d_log_sp^2,order=c(0,7))
z8=garch(d_log_sp^2,order=c(0,8))
z9=garch(d_log_sp^2,order=c(0,9))
garch11=garch(d_log_sp^2,order=c(1,1))

log_lik= c(logLik(z1),logLik(z2),logLik(z3),logLik(z4),logLik(z5),logLik(z6),logLik(z7),logLik(z8),logLik(z9),logLik(garch11))
log_lik

AIC=-2*log_lik+2*(q+1)*n/(n-q-1)
AIC
z2=garch(d_log_sp^2,order=c(0,2))


summary(z2)
par(mfrow=c(2,1))
sigt.z2=z2$fit[,1]
plot(sigt.z2,main="Conditional standard deviations from GARCH(0,2) model",xlab= "Number of days",ylab="Conditional variances",type='l')
ht.z2=z2$fit[,1]^2
plot(ht.z2,main="Conditional Variances from GARCH(0,2) model",xlab= "Number of days",ylab="Conditional variances",type='l')

#volatility checking using plots
par(mfrow=c(1,1))
plot.ts(log_sp,main="Plot of Log Prices",xlab="Number of Days",ylab="Differenced Prices")

plot.ts(d_log_sp,main="Plot of diffrenced Prices",xlab="Number of Days",ylab="Differenced Prices")
plot(ht.z2,main="Conditional Variances from GARCH(0,2) model",xlab= "Number of days",ylab="Conditional variances",type='l')

#combined ARIMA (1,1,1)+GARCH(0,2)

#95% CONFIDENCE INTERVALS OF THE PRICES USING THE COMBINED MODEL

fit111=fitted(arima111)
log_low= fit111-1.96*sqrt(ht.z2)
log_up=fit111+1.96*sqrt(ht.z2)
low=exp(log_low)
up=exp(log_up)
par(mfrow=c(1,1))
plot(sp,type='l',col="Red",main="Orignal Adjusted Closed Prices with 95% CI",xlab="Number of days",ylab="Adjusted Closed Prices with 95% CI")
lines(low,col="Blue")
lines(up,col="Green")
legend("bottomright",lty=c(1,1),lwd=2,inset=0.5,title="Legend",c("95%UCL","Orignal Prices","95% LCL"),col=c("Green","Red","Blue"),bty="o")

#QQ-Plot of the Combined Residuals

comb_retn= d_log_sp/sqrt(ht.z2)
par(mfrow=c(1,2))
qqnorm(d_log_sp,main='QQ-Plot of the ARIMA Returns')
qqline(d_log_sp)
qqnorm(comb_retn,main='QQ-Plot of the ARIMA-GARCH Returns')
qqline(comb_retn)

#LJUNG-BOX TEST OF RETURNS,SQUARED RETURNS OF COMBINED MODEL


Box.test(comb_retn,lag=10,type="Ljung-Box")
Box.test(comb_retn^2,lag=10,type="Ljung-Box")
