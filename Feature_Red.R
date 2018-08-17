library(stats)
library(xlsx)
library(caret)
#=============================train and test=================

Train <- createDataPartition(fulldata$Gender, p=0.7, list=FALSE)
training <- fulldata[ Train, ]
testing <- fulldata[ -Train, ]

count(training$Gender)

#==============================Creating a correlation matrix============


setwd("/Users/ssadasivan/Documents/Reme/Sandvik/Models")
t1<-subset(training,select = -c(Gender,fold,sound))

t1m<-cor(t1)

write.xlsx(t1m, "t1.xlsx") 

training<-subset(training,select = -c(harmonics_mean))

#=================Variable importance through a Randomforest model=============

library(randomForest)
library(party)

setwd("/Users/ssadasivan/Documents/Reme/Sandvik/Models")
training<-subset(training,select=(-c(sound,fold)))

count(training$Gender)
rf <- randomForest(Gender ~ ., ntree=100,
                    data = training)

print(rf) 
cat(capture.output(print(rf), file="conf_RF.txt"))
cat(capture.output(summary(rf), file="summ_RF.txt"))
cat(capture.output(rf$importance, file="Vimp_RF.txt"))
pdf("rferr.pdf") 
plot(rf)
dev.off()

testing$rfpred<-predict(rf,testing)
confusionMatrix(testing$rfpred, testing$Gender)
plot(rf) 

oob.err=double(13)
test.err=double(13)

# Set of variable  being dropped after looking at correlation matrix

t1<-subset(training,select=-c(HNR_mean,	ampl_mean,	ampl_median,	ampl_sd,	amplVoiced_mean,	dom_mean,	
                        entropy_mean,	entropy_median,	entropy_sd,	f1_freq_mean,	f1_freq_median,	
                        f1_width_mean,	f1_width_median,	f2_freq_mean,	f2_freq_median,	f2_width_mean,
                        f2_width_median,	f3_freq_mean,	f3_width_mean,	medianFreq_mean,
                        peakFreq_mean,	peakFreqCut_mean,	pitch_mean,	pitchAutocor_mean,	pitchSpec_mean,
                        quartile25_mean,	quartile50_mean,	quartile75_mean,	specCentroid_mean,pitch_median,
                        specCentroid_median,	specCentroidCut_mean,	specCentroidCut_median,	specSlope_mean,
                        specSlope_median))

#77 to 38
str(t1)
summary(t1)



rf1 <- randomForest(Gender ~ ., ntree=50,
                    data = t1)

print(rf1) 
cat(capture.output(print(rf1), file="conf_RF1.txt"))
cat(capture.output(summary(rf1), file="summ_RF1.txt"))
cat(capture.output(rf1$importance, file="Vimp_RF1.txt"))


t2<-subset(training,select=-c(Gender,HNR_mean,	ampl_mean,	ampl_median,	ampl_sd,	amplVoiced_mean,	dom_mean,	
                              entropy_mean,	entropy_median,	entropy_sd,	f1_freq_mean,	f1_freq_median,	
                              f1_width_mean,	f1_width_median,	f2_freq_mean,	f2_freq_median,	f2_width_mean,
                              f2_width_median,	f3_freq_mean,	f3_width_mean,	medianFreq_mean,
                              peakFreq_mean,	peakFreqCut_mean,	pitch_mean,	pitchAutocor_mean,	pitchSpec_mean,
                              quartile25_mean,	quartile50_mean,	quartile75_mean,	specCentroid_mean,pitch_median,
                              specCentroid_median,	specCentroidCut_mean,	specCentroidCut_median,	specSlope_mean,
                              specSlope_median))

t2m<-cor(t2)


write.xlsx(t2m, "t2.xlsx")

# top  15 vars selected based on variable importance 
t3<-subset(training,select= c(pitchAutocor_median,	pitchSpec_median,	f3_freq_median,	HNR_median,	
                              quartile75_median,	harmonics_median,	dom_median,	pitchSpec_sd,	
                              specSlope_sd,	voiced,Gender,f3_freq_sd,	quartile50_median,
                              f1_freq_sd,	dom_sd,harmonics_sd,peakFreqCut_median))

rf3 <- randomForest(Gender ~ ., ntree=100,
                    data = t3)

print(rf3) 
cat(capture.output(print(rf3), file="conf_RF3.txt"))
cat(capture.output(summary(rf3), file="summ_RF3.txt"))
cat(capture.output(rf3$importance, file="Vimp_RF3.txt"))

testing$rf3pred<-predict(rf3,testing)
confusionMatrix(testing$rf3pred, testing$Gender)
plot(rf3)

#================== Stepwise logistic model to get another set of variables


#create a null model
model.null = glm(Gender ~ 1,
                 data=t2,
                 family = binomial(link="logit"))

#create a full model
model.full = glm(Gender ~ .,
                 data=t2,
                 family = binomial(link="logit"))

#use a step function to extract the best n vars 
step1<-step(model.null,
            scope = list(upper=model.full),
            direction="both", trace=1,steps=15)

