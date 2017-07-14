library(aod)
library(ggplot2)
library(Rcpp)
library(pscl)
library(Amelia)
library(SDMTools)
require(lattice)
require(ggplot2)

#Set the working directory (Note that this working diectory should be changed from
# pc to pc)
setwd("C:/Users/Ali/Desktop/Recommender systems/Timeseries/regression")
#For store data use store_data.csv and for product data use product_data.csv
raw_data<-paste("venuekingsgame1.csv", sep = "")

#Reading the time-series data
database=read.csv(raw_data,header=TRUE)

data <- subset(database,select=c(15,18,19,20,21,22,23,24,25,26,27))

dataplot<-database[241:nrow(data),]

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.01, 0.1, 0.2, 0.4, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}


#correlation pairs plot
pairs(dataplot[c(17,19,20,21,22,24,27)], lower.panel=panel.smooth, upper.panel=panel.cor)

#setting up the train and test data
#traindata<-data[216:853,]
#testdata<-data[854:nrow(data),]

traindata<-data[241:952,]
testdata<-data[953:nrow(data),]




modellogit<-glm(Outcome~Home.team.ave.Rank+as.numeric(as.vector(Away.team.ave.ticket.prc))
                +as.numeric(as.vector(Home.team.ave.ticket.prc))+as.numeric(as.vector(Away.team.revenue))
                +as.numeric(as.vector(Home.team.revenue)),family = binomial(link='logit'),data=traindata)



#modellogit<-glm(game_records_Outcome~avg_aggregate_num_reactions+
#                 avg_aggregate_num_comments+avg_aggregate_num_shares,family = binomial(link='logit'),data=traindata)

summary(modellogit)
# anova(modellogit, test="Chisq")

pR2(modellogit)


fitted.results <- predict(modellogit,newdata=subset(testdata,select=c(1:length(data))),type="response")
fitted.results <- ifelse(fitted.results > 0.6,1,0)

misClasificError <- mean(fitted.results != testdata$Outcome)
print(paste('Accuracy',1-misClasificError))
confusion.matrix(fitted.results, testdata$Outcome, threshold = 0.6)
