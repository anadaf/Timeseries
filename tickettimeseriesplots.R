library(psych)
library(forecast)
library(astsa)
attach(mtcars)


#Set the working directory (Note that this working diectory should be changed from
# pc to pc)
setwd("C:/Users/Ali/Desktop/Recommender systems/Timeseries/charles/")
#For store data use store_data.csv and for product data use product_data.csv
data<-paste("venuekingsgame1.csv", sep = "")
#Reading the time-series data
database=read.csv(data,header=TRUE)

#a list of teams
#teams<-database$Away.team[!duplicated(database$Away.team)]
teams<-database$Category[!duplicated(database$Category)]



#generate two blunk datasets for forecast accuracy
error_acc<-data.frame(matrix(NA, nrow = 0, ncol = 0))
stlf_acc<-data.frame(matrix(NA, nrow = 0, ncol = 0))
holt_acc<-data.frame(matrix(NA, nrow = 0, ncol = 0))

#timeseries analysis for all teams
for (i in 1:31){ 
  
x<-database[database$Category==teams[i],]$Average.ticket.price
stlf_acc<-data.frame(matrix(NA, nrow = 0, ncol = 0))
for (j in 2:14){
y<-ts(x,frequency=j)
fc <- forecast(y,h=16)

#teams[i]

#ets_acc<-rbind(ets_acc,accuracy(fc)[2])

f1 <- stlf(y,h=16)
stlf_acc<-rbind(stlf_acc,cbind(accuracy(f1)[2],as.character(teams[i]),j))

#m<-HoltWinters(y)
#f2<-forecast.HoltWinters(m, 16)
#holt_acc<-rbind(holt_acc,accuracy(f2)[2])

}
k=strtoi(as.vector(stlf_acc[stlf_acc$V1==min(as.numeric(as.vector(stlf_acc$V1))),]$j))
y<-ts(x,frequency=k)
fr <- stlf(y,h=16)
error_acc<-rbind(error_acc,cbind(accuracy(fr)[2],as.character(teams[i]),k))#error rates
bmptlt1<-paste("plot/",teams[i],".bmp",sep="")#blank plots
bmp(filename=bmptlt1, width = 800, height = 580)
plot(fr,ylab="Average ticket price",xlab="Time (Season)",main=paste("Ticket price prediction for \n ", teams[i]))
dev.off()

print(i)}

#errors<-cbind(teams,stlf_acc)
write.csv(error_acc, file = "plot/errors.csv")#error rates on a CSV file