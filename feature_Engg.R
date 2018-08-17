library(dplyr)
library(neuralnet)
library(nnet)

#======================================
#creating a decision tree model
#======================================


library(party)

t5<-subset(t3,select=c(pitchAutocor_median,	pitchSpec_median,	f3_freq_median,	quartile75_median,	
                        peakFreqCut_median,	harmonics_median,	HNR_median,	specSlope_sd,	pitchSpec_sd,	
                        harmonics_sd,Gender,dom_median))

treem <- ctree(Gender ~ ., data = t5)
summary(treem)
print(treem)
plot(treem)
testing$tr_prd<-predict(treem,testing)

confusionMatrix(testing$tr_prd,testing$Gender)

for (i in 1:8045) if(t5$pitchSpec_median[i] <143) 
{t5$pitchSpec_median_b1[i]=1 } else {t5$pitchSpec_median_b1[i]=0}
for (i in 1:8045) if(between(t5$pitchSpec_median[i] ,143,179)) 
{t5$pitchSpec_median_b2[i]=1 } else {t5$pitchSpec_median_b2[i]=0}
for (i in 1:8045) if(t5$pitchSpec_median[i] >179) 
{t5$pitchSpec_median_b3[i]=1 } else {t5$pitchSpec_median_b3[i]=0}

for (i in 1:8045) if(t5$dom_median[i] <215) 
{t5$dom_median_b1[i]=1 } else {t5$dom_median_b1[i]=0}
for (i in 1:8045) if(between(t5$dom_median[i] ,180,215)) 
{t5$dom_median_b2[i]=1 } else {t5$dom_median_b2[i]=0}
for (i in 1:8045) if(between(t5$dom_median[i] ,215,240)) 
{t5$dom_median_b3[i]=1 } else {t5$dom_median_b3[i]=0}
for (i in 1:8045) if(t5$dom_median[i] >240) 
{t5$dom_median_b4[i]=1 } else {t5$dom_median_b4[i]=0}

for (i in 1:8045) if(t5$harmonics_median[i] <3.82) 
{t5$harmonics_median_b1[i]=1 } else {t5$harmonics_median_b1[i]=0}
for (i in 1:8045) if(between(t5$harmonics_median[i] ,3.16,8.11)) 
{t5$harmonics_median_b2[i]=1 } else {t5$harmonics_median_b2[i]=0}
for (i in 1:8045) if(between(t5$harmonics_median[i] ,8.11,10.07)) 
{t5$harmonics_median_b3[i]=1 } else {t5$harmonics_median_b3[i]=0}
for (i in 1:8045) if(t5$harmonics_median[i] >10.07) 
{t5$harmonics_median_b4[i]=1 } else {t5$harmonics_median_b4[i]=0}


for (i in 1:8045) if(t5$quartile75_median[i] <680) 
{t5$quartile75_median_b1[i]=1 } else {t5$quartile75_median_b1[i]=0}
for (i in 1:8045) if(between(t5$quartile75_median[i] ,680,1760)) 
{t5$quartile75_median_b2[i]=1 } else {t5$quartile75_median_b2[i]=0}
for (i in 1:8045) if(between(t5$quartile75_median[i] ,1760,1790)) 
{t5$quartile75_median_b3[i]=1 } else {t5$quartile75_median_b3[i]=0}
for (i in 1:8045) if(between(t5$quartile75_median[i] ,1790,2290)) 
{t5$quartile75_median_b4[i]=1 } else {t5$quartile75_median_b4[i]=0}
for (i in 1:8045) if(t5$quartile75_median[i] >2290) 
{t5$quartile75_median_b5[i]=1 } else {t5$quartile75_median_b5[i]=0}

for (i in 1:8045) if(t5$HNR_median[i] <5.22) 
{t5$HNR_median_b1[i]=1 } else {t5$HNR_median_b1[i]=0}
for (i in 1:8045) if(between(t5$HNR_median[i] ,5.2,7.5)) 
{t5$HNR_median_b2[i]=1 } else {t5$HNR_median_b2[i]=0}
for (i in 1:8045) if(between(t5$HNR_median[i] ,7.5,9.8)) 
{t5$HNR_median_b3[i]=1 } else {t5$HNR_median_b3[i]=0}
for (i in 1:8045) if(t5$HNR_median[i] >9.8) 
{t5$HNR_median_b4[i]=1 } else {t5$HNR_median_b4[i]=0}


for (i in 1:8045) if(t5$pitchAutocor_median[i] <172) 
{t5$pitchAutocor_median_b1[i]=1 } else {t5$pitchAutocor_median_b1[i]=0}
for (i in 1:8045) if(between(t5$quartile75_median[i] ,172,206)) 
{t5$pitchAutocor_median_b2[i]=1 } else {t5$pitchAutocor_median_b2[i]=0}
for (i in 1:8045) if(between(t5$quartile75_median[i] ,206,225)) 
{t5$pitchAutocor_median_b3[i]=1 } else {t5$pitchAutocor_medianb3[i]=0}
for (i in 1:8045) if(between(t5$quartile75_median[i] ,225,246)) 
{t5$pitchAutocor_median_b4[i]=1 } else {t5$pitchAutocor_median_b4[i]=0}
for (i in 1:8045) if(between(t5$quartile75_median[i] ,246,323)) 
{t5$pitchAutocor_median_b5[i]=1 } else {t5$pitchAutocor_median_b5[i]=0}
for (i in 1:8045) if(t5$quartile75_median[i] >323) 
{t5$pitchAutocor_median_b6[i]=1 } else {t5$pitchAutocor_median_b6[i]=0}



for (i in 1:8045) if(t5$f3_freq_median[i] <2706) 
{t5$f3_freq_median_b1[i]=1 } else {t5$f3_freq_median_b1[i]=0}
for (i in 1:8045) if(between(t5$f3_freq_median[i] ,2706,2962)) 
{t5$f3_freq_median_b2[i]=1 } else {t5$f3_freq_median_b2[i]=0}
for (i in 1:8045) if(between(t5$f3_freq_median[i] ,2962,3033)) 
{t5$f3_freq_median_b3[i]=1 } else {t5$f3_freq_median_b3[i]=0}
for (i in 1:8045) if(t5$f3_freq_median[i] >3033) 
{t5$f3_freq_median_b4[i]=1 } else {t5$f3_freq_median_b4[i]=0}

t5<-subset(t5,select=-c(pitchAutocor_median,	pitchSpec_median,	f3_freq_median,	quartile75_median,	
                        peakFreqCut_median,	harmonics_median,	HNR_median,	specSlope_sd,	pitchSpec_sd,	
                        harmonics_sd,dom_median))


n <- names(t5)
n<-n[-1]
f <- as.formula(paste("Gender ~", paste(n[!n %in% "t5"], collapse = " + ")))
t5$Gender<-as.factor(t5$Gender)

#=========================
#neural net
#================
nn1<-nnet(f,data=t5,size=10)


tt5<-subset(testing,select=c(pitchAutocor_median,	pitchSpec_median,	f3_freq_median,	quartile75_median,	
                       peakFreqCut_median,	harmonics_median,	HNR_median,	specSlope_sd,	pitchSpec_sd,	
                       harmonics_sd,Gender,dom_median))



for (i in 1:3446) if(tt5$pitchSpec_median[i] <143) 
{tt5$pitchSpec_median_b1[i]=1 } else {tt5$pitchSpec_median_b1[i]=0}
for (i in 1:3446) if(between(tt5$pitchSpec_median[i] ,143,179)) 
{tt5$pitchSpec_median_b2[i]=1 } else {tt5$pitchSpec_median_b2[i]=0}
for (i in 1:3446) if(tt5$pitchSpec_median[i] >179) 
{tt5$pitchSpec_median_b3[i]=1 } else {tt5$pitchSpec_median_b3[i]=0}

for (i in 1:3446) if(tt5$dom_median[i] <215) 
{tt5$dom_median_b1[i]=1 } else {tt5$dom_median_b1[i]=0}
for (i in 1:3446) if(between(tt5$dom_median[i] ,180,215)) 
{tt5$dom_median_b2[i]=1 } else {tt5$dom_median_b2[i]=0}
for (i in 1:3446) if(between(tt5$dom_median[i] ,215,240)) 
{tt5$dom_median_b3[i]=1 } else {tt5$dom_median_b3[i]=0}
for (i in 1:3446) if(tt5$dom_median[i] >240) 
{tt5$dom_median_b4[i]=1 } else {tt5$dom_median_b4[i]=0}

for (i in 1:3446) if(tt5$harmonics_median[i] <3.82) 
{tt5$harmonics_median_b1[i]=1 } else {tt5$harmonics_median_b1[i]=0}
for (i in 1:3446) if(between(tt5$harmonics_median[i] ,3.16,8.11)) 
{tt5$harmonics_median_b2[i]=1 } else {tt5$harmonics_median_b2[i]=0}
for (i in 1:3446) if(between(tt5$harmonics_median[i] ,8.11,10.07)) 
{tt5$harmonics_median_b3[i]=1 } else {tt5$harmonics_median_b3[i]=0}
for (i in 1:3446) if(tt5$harmonics_median[i] >10.07) 
{tt5$harmonics_median_b4[i]=1 } else {tt5$harmonics_median_b4[i]=0}


for (i in 1:3446) if(tt5$quartile75_median[i] <680) 
{tt5$quartile75_median_b1[i]=1 } else {tt5$quartile75_median_b1[i]=0}
for (i in 1:3446) if(between(tt5$quartile75_median[i] ,680,1760)) 
{tt5$quartile75_median_b2[i]=1 } else {tt5$quartile75_median_b2[i]=0}
for (i in 1:3446) if(between(tt5$quartile75_median[i] ,1760,1790)) 
{tt5$quartile75_median_b3[i]=1 } else {tt5$quartile75_median_b3[i]=0}
for (i in 1:3446) if(between(tt5$quartile75_median[i] ,1790,2290)) 
{tt5$quartile75_median_b4[i]=1 } else {tt5$quartile75_median_b4[i]=0}
for (i in 1:3446) if(tt5$quartile75_median[i] >2290) 
{tt5$quartile75_median_b5[i]=1 } else {tt5$quartile75_median_b5[i]=0}

for (i in 1:3446) if(tt5$HNR_median[i] <5.22) 
{tt5$HNR_median_b1[i]=1 } else {tt5$HNR_median_b1[i]=0}
for (i in 1:3446) if(between(tt5$HNR_median[i] ,5.2,7.5)) 
{tt5$HNR_median_b2[i]=1 } else {tt5$HNR_median_b2[i]=0}
for (i in 1:3446) if(between(tt5$HNR_median[i] ,7.5,9.8)) 
{tt5$HNR_median_b3[i]=1 } else {tt5$HNR_median_b3[i]=0}
for (i in 1:3446) if(tt5$HNR_median[i] >9.8) 
{tt5$HNR_median_b4[i]=1 } else {tt5$HNR_median_b4[i]=0}


for (i in 1:3446) if(tt5$pitchAutocor_median[i] <172) 
{tt5$pitchAutocor_median_b1[i]=1 } else {tt5$pitchAutocor_median_b1[i]=0}
for (i in 1:3446) if(between(tt5$quartile75_median[i] ,172,206)) 
{tt5$pitchAutocor_median_b2[i]=1 } else {tt5$pitchAutocor_median_b2[i]=0}
for (i in 1:3446) if(between(tt5$quartile75_median[i] ,206,225)) 
{tt5$pitchAutocor_median_b3[i]=1 } else {tt5$pitchAutocor_medianb3[i]=0}
for (i in 1:3446) if(between(tt5$quartile75_median[i] ,225,246)) 
{tt5$pitchAutocor_median_b4[i]=1 } else {tt5$pitchAutocor_median_b4[i]=0}
for (i in 1:3446) if(between(tt5$quartile75_median[i] ,246,323)) 
{tt5$pitchAutocor_median_b5[i]=1 } else {tt5$pitchAutocor_median_b5[i]=0}
for (i in 1:3446) if(tt5$quartile75_median[i] >323) 
{tt5$pitchAutocor_median_b6[i]=1 } else {tt5$pitchAutocor_median_b6[i]=0}



for (i in 1:3446) if(tt5$f3_freq_median[i] <2706) 
{tt5$f3_freq_median_b1[i]=1 } else {tt5$f3_freq_median_b1[i]=0}
for (i in 1:3446) if(between(tt5$f3_freq_median[i] ,2706,2962)) 
{tt5$f3_freq_median_b2[i]=1 } else {tt5$f3_freq_median_b2[i]=0}
for (i in 1:3446) if(between(tt5$f3_freq_median[i] ,2962,3033)) 
{tt5$f3_freq_median_b3[i]=1 } else {tt5$f3_freq_median_b3[i]=0}
for (i in 1:3446) if(tt5$f3_freq_median[i] >3033) 
{tt5$f3_freq_median_b4[i]=1 } else {tt5$f3_freq_median_b4[i]=0}

t5<-subset(t5,select=-c(pitchAutocor_median,	pitchSpec_median,	f3_freq_median,	quartile75_median,	
                        peakFreqCut_median,	harmonics_median,	HNR_median,	specSlope_sd,	pitchSpec_sd,	
                        harmonics_sd,dom_median))


tt5$pred<-predict(nn1,newdata=tt5,type="class")
tt5$pred<-as.factor(tt5$pred)
confusionMatrix(tt5$Gender,tt5$pred)
