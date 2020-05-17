#Adding Libraries
library(sqldf)
library(psych)
library(dplyr)
library(caret)
library(eply)

# Joining all the features created into a single table along with the target variable :Gender
fulldata<-sqldf("select a.* , b.Gender, c.skewf,c.kurtf,c.sdf from flist1 a inner join fldata b 
                on a.fold=b.fold and a.sound=b.filename 
                inner join flist2 c on  a.fold=c.fold and a.sound=c.filename")
describe(fulldata)
str(fulldata)
fulldata$Gender<-as.factor(fulldata$Gender)
str(fulldata$Gender)
count(fulldata$Gender)
c<-ncol(fulldata)
r<-nrow(fulldata)




fulldata<-subset(fulldata,Gender=="Female" | Gender=="Male")

fulldata$Gender<-factor(fulldata$Gender)
str(fulldata$Gender)
count(fulldata$Gender)

#============================================
# data cleaning removing null and irrelevant variables
#============================================

fulldata<-subset(fulldata,select=-c(pitchCep_mean,pitchCep_median,pitchCep_sd,duration))
c<-ncol(fulldata)
r<-nrow(fulldata)
mnm<-names(fulldata)
mnm<-mnm[-c(1,75,76)]

fulldata[mnm] <- sapply(fulldata[mnm],as.character)
str(fulldata)
fulldata[mnm] <- sapply(fulldata[mnm],as.double)
str(fulldata)

########################################################
# R Function for Outlier Treatment : Percentile Capping
########################################################
pcap <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}

# Replacing extreme values with percentiles
pcap(fulldata)

#================================
#replace missing values
#================================
summary(fulldata)
mnm<-names(fulldata)
for(i in 2:75){
  fulldata[is.na(fulldata[,i]), i] <- mean(fulldata[,i], na.rm = TRUE)
}
summary(fulldata)

#=====================================================
# create plots for all independenet variables and saving to PDF
# Ranks each independent variable into a decile and calculates %of males in each decile
# Plots decile avg (X-Axis) V/s % males (Y-axis)
#=====================================================
fulldatac<-fulldata


n1 <- names(t3)
n1<-n1[-11]
t5<-subset(t3,select=-c(Gender))

library(gsubfn)

pdf("rplot.pdf") 
for (i in 1:16)
{
  var<-n1[i]
  vart<-n1
  training$r2<-ntile(t5[i],10)
  
  vart <-sqldf(sprintf("select r2, sum (case when Gender = 'Male' then 1 end) as 'Male',
                             sum (case when Gender = 'Female' then 1 end) as  'Female',
                       avg(%s)  avg_hnr_mean from training group by r2",var)) 
  vart$total <- vart$Male + vart$Female
  vart$Male_Per <- vart$Male / vart$total 
  plot(vart$avg_hnr_mean, vart$Male_Per, main=var, xlab=" Decile Avg ", 
       ylab="% Of Males", pch=19)
}
dev.off()

