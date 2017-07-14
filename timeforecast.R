###############################################
#LUVO Time Series Analysis
#Author: Ali@Qileap.com
###############################################

#This code gets the the Luvo timeseries data and performs following procedures:
#  1. Read the data
#  2. Prediction values using ETS, ARIMA and Holtwinters
#  3. Write the error rates and produce the prediction plots

#loading the library
library(psych)
library(forecast)
library(astsa)

#Set the working directory (Note that this working diectory should be changed from
# pc to pc)
setwd("C:/Users/Ali/Desktop/Recommender systems/Timeseries/data")
#For store data use store_data.csv and for product data use product_data.csv
data<-paste("store_data.csv", sep = "")

#Reading the time-series data
database=read.csv(data,header=FALSE)


#Counting the number of rows and columns
n_row<-nrow(database)
n_column<-ncol(database)

#generate two blunk datasets for forecast accuracy
ets_acc<-data.frame(matrix(NA, nrow = 0, ncol = 0))
ar_acc<-data.frame(matrix(NA, nrow = 0, ncol = 0))
holt_acc<-data.frame(matrix(NA, nrow = 0, ncol = 0))
stlf_acc<-data.frame(matrix(NA, nrow = 0, ncol = 0))


# For all time-series data, predict sales amount using ARIMA and ETS
for (i in 2:200){
tsdata=strtoi(as.vector(t(database)[n_column:2,i]))

#If one of time-series components is non-zero, then it analyzes the time-series data
if (sum(tsdata)!=0){  
titl=paste(database[i,1])  
titl<-gsub("/","-",titl)

#Analysis using the ETS
myts<-ts(tsdata,start=2014,frequency = 52)
fc.ets=forecast(myts,h=52)

# analysis using the arima
ar=auto.arima(myts)
fc.ar = forecast(ar,h=52)

# Prediction using Holtwinters
if(i!=71){
pr.holt=HoltWinters(myts)
fc.holt <- forecast.HoltWinters(pr.holt, h=52)}

# Prediction using STLF
fc.stlf <- stlf(myts,h=52)

print(i)

ets_acc=rbind(ets_acc,cbind(as.character(database$V1[i]), accuracy(fc.ets)))
ar_acc=rbind(ar_acc,cbind(as.character(database$V1[i]), accuracy(fc.ar)))
holt_acc=rbind(holt_acc,cbind(as.character(database$V1[i]), accuracy(fc.holt)))
stlf_acc=rbind(stlf_acc,cbind(as.character(database$V1[i]), accuracy(fc.stlf)))



#Figures for ETS prediction
bmptlt1<-paste("plots/ets/",titl,".bmp",sep="")
bmp(filename=bmptlt1, width = 800, height = 580)
plot(fc.ets,main=paste("TimeSeries forecast \n", titl),xlab="Time", ylab="Sales amount")
dev.off()

#Figures for ARIMA prediction
bmptlt2<-paste("plots/ar/",titl,".bmp",sep="")
bmp(filename=bmptlt2, width = 800, height = 580)
plot(fc.ar,main=paste("TimeSeries forecast \n", titl),xlab="Time", ylab="Sales amount")
dev.off()


#Figures for HoltWinters prediction
bmptlt3<-paste("plots/holt/",titl,".bmp",sep="")
bmp(filename=bmptlt3, width = 800, height = 580)
plot.forecast(fc.holt,main=paste("TimeSeries forecast \n", titl),xlab="Time", ylab="Sales amount")
dev.off()

#Figures displays ACF-PACF for all time-series data
bmptlt4<-paste("plots/acf-pacf/",titl,".bmp",sep="")
bmp(filename=bmptlt4, width = 1200, height = 1680)
par(mfrow=c(2,1))
acf(myts,main=paste("ACF -", titl))
pacf(myts,main=paste("PACF -", titl))
dev.off()

#Figures displays ACF-PACF for all time-series data
bmptlt5<-paste("plots/stlf/",titl,".bmp",sep="")
bmp(filename=bmptlt5, width = 800, height = 580)
plot.forecast(fc.stlf,main=paste("TimeSeries forecast \n", titl),xlab="Time", ylab="Sales amount")
dev.off()

}}

#Export prediction Error as a csv file
write.csv(ets_acc, file = "plots/ets_acc.csv")
write.csv(ar_acc, file = "plots/ar_acc.csv")
write.csv(holt_acc, file = "plots/holt_acc.csv")
write.csv(stlf_acc, file = "plots/stlf_acc.csv")
################################################################




