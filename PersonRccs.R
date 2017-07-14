library(psych)
setwd("~/Desktop/Charge Files for Ali/")

#Check the existence and Create the output directories if they don't exist 
if (file.exists("person_rccs_output")){}else {
    dir.create(file.path(getwd(), "person_rccs_output"))
    dir.create(file.path(getwd(), "person_rccs_output/Data"))
    dir.create(file.path(getwd(), "person_rccs_output/plots"))
}

#create a dataframe for percentile and descriptive
prc<-data.frame(matrix(NA, nrow = 9, ncol = 6))

colnames(prc) <- c("Year","Categories","95th","99th","S95th","S99th")
descrp<-data.frame(matrix(NA, nrow = 0, ncol = 0))
meanvlue<-data.frame(matrix(NA, nrow = 200, ncol = 2))

for (i in 2004:2011){
#Read the data from the database for different years from 2004 to 2011
addrss<-paste("2014_12_05_part_rcc/part_",i,"_combine.txt", sep = "")
ttl<-read.table(addrss,header=T,sep="\t")
Year=i
######################################################

#Read different columns from the data 
#Read column names
Category="Total Charges per Person RCC"

k=ttl[,2]/ttl[,1]
#Generate the descriptive statistics data
descrp=rbind(descrp,cbind(Year,Category, describe(k)))

n=(i-2004)+1
mx=max(ttl[,1])
#Generate the percentile data
prc[n,]<-c(i,colnames(ttl[1]),quantile(ttl[,1],c(0.95,0.99)),quantile(ttl[,3],c(0.95,0.99)))

#Create BMP format for the generating figures
bmptlt<-paste("person_rccs_output/plots/","Hist-",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)
#par(mfrow=c(1,2))

titl="Average Number of Total Counts for Accused with N number of Person RCCs  in "

#Generate Histogram
for (v1 in 1:max(ttl[,1])){
if (nrow(ttl[ttl[,1]==v1,])==0){
meanvlue[v1,]=c(0,v1)}else{
meanvlue[v1,]= c(mean(ttl[ttl[,1]==v1,][,2]/ttl[ttl[,1]==v1,][,1]),v1)
}}
 plot(meanvlue[,2],meanvlue[,1],type="s",col=c(2),ylab="Average Number of Charges per Person RCCs ", xlab="Number of Person RCCs  per Accused",main=paste(titl,i,sep=" "))

dev.off()


######################################################
######################################################

k=ttl[,4]/ttl[,3]
Category="Substantive Charges per Person RCC"
descrp=rbind(descrp,cbind(Year,Category, describe(k)))
mx=max(ttl[,3])

#Create BMP format for the generating figures
bmptlt<-paste("person_rccs_output/plots/","Hist-",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)


titl="Average Number of Substantive Counts for Accused with N number of Person RCCs  in "

#Generate Histogram
for (v1 in 1:max(ttl[,3])){
if (nrow(ttl[ttl[,3]==v1,])==0){
meanvlue[v1,]=c(0,v1)}else{
meanvlue[v1,]= c(mean(ttl[ttl[,3]==v1,][,4]/ttl[ttl[,3]==v1,][,3]),v1)
}}
 plot(meanvlue[,2],meanvlue[,1],type="s",col=c(2),ylab="Average Number of Charges per Person RCCs ", xlab="Number of Person RCCs  per Accused",main=paste(titl,i,sep=" "))

dev.off()
######################################################
bmptlt<-paste("person_rccs_output/plots/","percentage-less-95",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)
#Generate Percentile lines for less than 95%
ttlg=ttl[ttl[,1]>=as.numeric(prc[n,3]),]
ttll=ttl[ttl[,1]<as.numeric(prc[n,3]),]
kl=ttll[,2]/ttll[,1]
kg=ttlg[,2]/ttlg[,1]
kl=replace(kl,is.nan(kl),0)
kg=replace(kg,is.nan(kg),0)
titl="Person RCCs  with N Total Charges per Person RCC in"

plot(density(kl,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with less than ",prc[n,3],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","percentage-greater-95",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 95%
titl="or more Person RCCs  with N Total Charges per Person RCC in"

plot(density(kg,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with",prc[n,3],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-less-95",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate histogram for less than 95%

titl="Person RCCs  with N Total Charges per Person RCC in"

hist(kl,col=c(2),main=paste("Number of Accused with less than ",prc[n,3],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kl)+1,xlim=c(1,max(kl,kg)+1))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-greater-95",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 95%
titl="or more Person RCCs  with N Total Charges per Person RCC in"
hist(kg,col=c(2),main=paste("Number of Accused with",prc[n,3],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kg)+1,xlim=c(1,max(kl,kg)+1))
dev.off()
###############################################################
bmptlt<-paste("person_rccs_output/plots/","percentage-less-99",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)
#Generate Percentile lines for less than 99%
ttlg=ttl[ttl[,1]>=as.numeric(prc[n,4]),]
ttll=ttl[ttl[,1]<as.numeric(prc[n,4]),]
kl=ttll[,2]/ttll[,1]
kg=ttlg[,2]/ttlg[,1]
titl="Person RCCs  with N Total Charges per Person RCC in"

plot(density(kl,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with less than ",prc[n,4],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","percentage-greater-99",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 99%
titl="or more Person RCCs  with N Total Charges per Person RCC in"

plot(density(kg,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with",prc[n,4],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-less-99",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate histogram for less than 99%

titl="Person RCCs  with N Total Charges per Person RCC in"

hist(kl,col=c(2),main=paste("Number of Accused with less than ",prc[n,4],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kl)+1,xlim=c(1,max(kl,kg)+1))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-greater-99",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 99%
titl="or more Person RCCs  with N Total Charges per Person RCC in"
hist(kg,col=c(2),main=paste("Number of Accused with",prc[n,4],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kg)+1,xlim=c(1,max(kl,kg)+1))
dev.off()
######################################################
bmptlt<-paste("person_rccs_output/plots/","percentage-less-95",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)
#Generate Percentile lines for less than 95%
ttlg=ttl[ttl[,3]>=as.numeric(prc[n,5]),]
ttll=ttl[ttl[,3]<as.numeric(prc[n,5]),]
kl=ttll[,4]/ttll[,3]
kg=ttlg[,4]/ttlg[,3]
kl=replace(kl,is.nan(kl),0)
kg=replace(kg,is.nan(kg),0)
titl="Person RCCs  with N Substantive Charges per Person RCC in"

plot(density(kl,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with less than ",prc[n,5],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","percentage-greater-95",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 95%
titl="or more Person RCCs  with N Substantive Charges per Person RCC in"

plot(density(kg,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with",prc[n,5],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-less-95",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate histogram for less than 95%

titl="Person RCCs  with N Substantive Charges per Person RCC in"

hist(kl,col=c(2),main=paste("Number of Accused with less than ",prc[n,5],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kl)+1,xlim=c(1,max(kl,kg)+1))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-greater-95",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 95%
titl="or more Person RCCs  with N Substantive Charges per Person RCC in"
hist(kg,col=c(2),main=paste("Number of Accused with",prc[n,5],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kg)+1,xlim=c(1,max(kl,kg)+1))
dev.off()

###############################################################
bmptlt<-paste("person_rccs_output/plots/","percentage-less-99",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)
#Generate Percentile lines for less than 99%
ttlg=ttl[ttl[,3]>=as.numeric(prc[n,6]),]
ttll=ttl[ttl[,3]<as.numeric(prc[n,6]),]
kl=ttll[,4]/ttll[,3]
kg=ttlg[,4]/ttlg[,3]
kl=replace(kl,is.nan(kl),0)
kg=replace(kg,is.nan(kg),0)
titl="Person RCCs  with N Substantive Charges per Person RCC in"

plot(density(kl,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with less than ",prc[n,4],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","percentage-greater-99",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 99%
titl="or more Person RCCs  with N Substantive Charges per Person RCC in"

plot(density(kg,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with",prc[n,6],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-less-99",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate histogram for less than 99%

titl="Person RCCs  with N Substantive Charges per Person RCC in"

hist(kl,col=c(2),main=paste("Number of Accused with less than ",prc[n,6],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kl)+1,xlim=c(1,max(kl,kg)+1))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-greater-99",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 99%
titl="or more Person RCCs  with N Substantive Charges per Person RCC in"
hist(kg,col=c(2),main=paste("Number of Accused with",prc[n,6],titl,i,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kg)+1,xlim=c(1,max(kl,kg)+1))
dev.off()


}
####################################################
#####################################################
#####################################################

#Generate Histogram and Density plots for entire timeframe data

#Read the data
ttl<-read.table("2014_12_05_part_rcc/part_combine.txt",header=T,sep="\t")
ttl3=na.omit(ttl)
Year="Total"
i="-Total"

Category="Total Charges per Person RCC"
k=ttl[,2]/ttl[,1]
descrp=rbind(descrp,cbind(Year,Category, describe(k)))
n=9

#Descriptive Statistics
prc[n,]<-c("Total",colnames(ttl[1]),quantile(ttl[,1],c(0.95,0.99)),quantile(ttl3[,3],c(0.95,0.99)))

bmptlt<-paste("person_rccs_output/plots/","Hist-",colnames(ttl[2]),"-Total",".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

titl="Average Number of Total Counts for Accused with N number of Person RCCs  \nfrom 2004 to 2011."

for (v1 in 1:max(ttl[,1])){
if (nrow(ttl[ttl[,1]==v1,])==0){
meanvlue[v1,]=c(0,v1)}else{
meanvlue[v1,]= c(mean(ttl[ttl[,1]==v1,][,2]/ttl[ttl[,1]==v1,][,1]),v1)
}}
 plot(meanvlue[,2],meanvlue[,1],col=c(2),type="s",ylab="Average Number of Charges per Person RCCs ", xlab="Number of Person RCCs  per Accused",main=titl)

dev.off()

k=ttl3[,4]/ttl3[,3]
Category="Substantive Charges per Person RCC"
descrp=rbind(descrp,cbind(Year,Category, describe(k)))
mx=max(ttl3[,3])

#Create BMP format for the generating figures
bmptlt<-paste("person_rccs_output/plots/","Hist-",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)


titl="Average Number of Substantive Counts for Accused with N number of Person RCCs  \nfrom 2004 to 2011."

#Generate Histogram
for (v1 in 1:max(ttl3[,3])){
if (nrow(ttl3[ttl3[,3]==v1,])==0){
meanvlue[v1,]=c(0,v1)}else{
meanvlue[v1,]= c(mean(ttl3[ttl3[,3]==v1,][,4]/ttl3[ttl3[,3]==v1,][,3]),v1)
}}
 plot(meanvlue[,2],meanvlue[,1],type="s",col=c(2),ylab="Average Number of Charges per Person RCCs ", xlab="Number of Person RCCs  per Accused",main=paste(titl,sep=" "))

dev.off()
######################################################
bmptlt<-paste("person_rccs_output/plots/","percentage-less-95",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)
#Generate Percentile lines for less than 95%
ttlg=ttl[ttl[,1]>=as.numeric(prc[n,3]),]
ttll=ttl[ttl[,1]<as.numeric(prc[n,3]),]
kl=ttll[,2]/ttll[,1]
kg=ttlg[,2]/ttlg[,1]
kl=replace(kl,is.nan(kl),0)
kg=replace(kg,is.nan(kg),0)
titl="Person RCCs  with N Total Charges per Person RCC from \n2004 to 2011."

plot(density(kl,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with less than ",prc[n,3],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","percentage-greater-95",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 95%
titl="or more Person RCCs  with N Total Charges per Person RCC \nfrom 2004 to 2011."

plot(density(kg,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with",prc[n,3],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-less-95",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate histogram for less than 95%

titl="Person RCCs  with N Total Charges per Person RCC \nfrom 2004 to 2011."

hist(kl,col=c(2),main=paste("Number of Accused with less than ",prc[n,3],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kl)+1,xlim=c(1,max(kl,kg)+1))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-greater-95",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 95%
titl="or more Person RCCs  with N Total Charges per Person RCC \nfrom 2004 to 2011."
hist(kg,col=c(2),main=paste("Number of Accused with",prc[n,3],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kg)+1,xlim=c(1,max(kl,kg)+1))
dev.off()
###############################################################
bmptlt<-paste("person_rccs_output/plots/","percentage-less-99",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)
#Generate Percentile lines for less than 99%
ttlg=ttl[ttl[,1]>=as.numeric(prc[n,4]),]
ttll=ttl[ttl[,1]<as.numeric(prc[n,4]),]
kl=ttll[,2]/ttll[,1]
kg=ttlg[,2]/ttlg[,1]
titl="Person RCCs  with N Total Charges per Person RCC \nfrom 2004 to 2011."

plot(density(kl,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with less than ",prc[n,4],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","percentage-greater-99",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 99%
titl="or more Person RCCs  with N Total Charges per Person RCC \nfrom 2004 to 2011."

plot(density(kg,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with",prc[n,4],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-less-99",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate histogram for less than 99%

titl="Person RCCs  with N Total Charges per Person RCC \nfrom 2004 to 2011."

hist(kl,col=c(2),main=paste("Number of Accused with less than ",prc[n,4],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kl)+1,xlim=c(1,max(kl,kg)+1))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-greater-99",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 99%
titl="or more Person RCCs  with N Total Charges per Person RCC \nfrom 2004 to 2011."
hist(kg,col=c(2),main=paste("Number of Accused with",prc[n,4],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kg)+1,xlim=c(1,max(kl,kg)+1))
dev.off()
######################################################
bmptlt<-paste("person_rccs_output/plots/","percentage-less-95",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)
#Generate Percentile lines for less than 95%
ttlg=ttl3[ttl3[,3]>=as.numeric(prc[n,5]),]
ttll=ttl3[ttl3[,3]<as.numeric(prc[n,5]),]
kl=ttll[,4]/ttll[,3]
kg=ttlg[,4]/ttlg[,3]
kl=replace(kl,is.nan(kl),0)
kg=replace(kg,is.nan(kg),0)
titl="Person RCCs  with N Substantive Charges per Person RCC \nfrom 2004 to 2011."

plot(density(kl,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with less than ",prc[n,5],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","percentage-greater-95",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 95%
titl="or more Person RCCs  with N Substantive Charges per Person RCC \nfrom 2004 to 2011."

plot(density(kg,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with",prc[n,5],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1),xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-less-95",colnames(ttl[1]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate histogram for less than 95%

titl="Person RCCs  with N Substantive Charges per Person RCC \nfrom 2004 to 2011."

hist(kl,col=c(2),main=paste("Number of Accused with less than ",prc[n,5],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kl)+1,xlim=c(1,max(kl,kg)+1))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-greater-95",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 95%
titl="or more Person RCCs  with N Substantive Charges per Person RCC \nfrom 2004 to 2011."
hist(kg,col=c(2),main=paste("Number of Accused with",prc[n,5],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kg)+1,xlim=c(1,max(kl,kg)+1))
dev.off()

###############################################################
bmptlt<-paste("person_rccs_output/plots/","percentage-less-99",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)
#Generate Percentile lines for less than 99%
ttlg=ttl3[ttl3[,3]>=as.numeric(prc[n,6]),]
ttll=ttl3[ttl3[,3]<as.numeric(prc[n,6]),]
kl=ttll[,4]/ttll[,3]
kg=ttlg[,4]/ttlg[,3]
kl=replace(kl,is.nan(kl),0)
kg=replace(kg,is.nan(kg),0)
titl="Person RCCs  with N Substantive Charges per Person RCC \nfrom 2004 to 2011."

plot(density(kl,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with less than ",prc[n,4],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","percentage-greater-99",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 99%
titl="or more Person RCCs  with N Substantive Charges per Person RCC \nfrom 2004 to 2011."

plot(density(kg,adjust=3,bw = 0.1),col=c(2),main=paste("Percentage of Accused with",prc[n,6],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="% of Accused",yaxt="n",xlim=c(1,max(kl,kg)+1))
axis(2, at = seq(0, 1, 0.1), labels = paste(seq(0,1,0.1)*100))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-less-99",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate histogram for less than 99%

titl="Person RCCs  with N Substantive Charges per Person RCC \nfrom 2004 to 2011."

hist(kl,col=c(2),main=paste("Number of Accused with less than ",prc[n,6],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kl)+1,xlim=c(1,max(kl,kg)+1))
dev.off()

bmptlt<-paste("person_rccs_output/plots/","hist-greater-99",colnames(ttl[3]),i,".bmp",sep="")
bmp(filename=bmptlt, width = 800, height = 580)

#Generate Percentile lines for more than 99%
titl="or more Person RCCs  with N Substantive Charges per Person RCC \nfrom 2004 to 2011."
hist(kg,col=c(2),main=paste("Number of Accused with",prc[n,6],titl,sep=" "),xlab="Number of Counts per Person RCC", ylab="Number of Accused",breaks=max(kg)+1,xlim=c(1,max(kl,kg)+1))
dev.off()

#####################################################


#Export Percentile Data as a csv file
write.csv(prc, file = "person_rccs_output/Data/percentiles.csv")


#Export Descriptive Statistical data as a csv file
write.csv(descrp, file = "person_rccs_output/Data/descriptiveData.csv")
