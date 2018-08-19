#==========Models with hyperparamter tuning========

#=================Randomforest model=============

library(randomForest)
library(party)
library(mlr)
library(e1071)
#Begin modelling
setwd("/Users/ssadasivan/Documents/Reme/Sandvik/Models")
training<-subset(training,select=(-c(sound,fold)))

n.trees = seq(from=50 ,to=100, by=10) #no of trees-a vector of 100 values 
m.try = seq(from=2 ,to=7, by=1) #no of vars to be considered for a split



obj <- tune(randomForest, Gender ~ ., data = t3, 
            ranges = list(ntree=n.trees, mtry=m.try),
            tunecontrol = tune.control(sampling = "fix")
)

summary(obj)

rf3 <- randomForest(Gender ~ ., ntree=80,mtry=4,
                    data = t3)


print(rf3) 
cat(capture.output(print(rf3), file="conf_RF3.txt"))
cat(capture.output(summary(rf3), file="summ_RF3.txt"))
cat(capture.output(rf3$importance, file="Vimp_RF3.txt"))
pdf("rferr.pdf") 
plot(rf3)
dev.off()

testing$rf3pred<-predict(rf3,testing)


confusionMatrix(testing$Gender,testing$rf3pred)
plot(rf3) #plots the error rate with the number of trees
# can be used to fix the number of trees while we tune other paramters

oob.err=double(13)
test.err=double(13)


#==========Stepwise selection and logistic regression========

step.model <- model.full %>% stepAIC(trace = FALSE)
cat(capture.output(print(step1), file="Step_Model.txt"))

testing$pred = predict(step1, newdata=testing,type = "response")
testing$Glpred = as.factor(ifelse(testing$pred > 0.5, "Male", "Female"))
confusionMatrix(testing$Gender, testing$Glpred)

#===========================neural net model==============
library(neuralnet)
library(nnet)
#===========================================
# creating a neural net model library neuralnet
# need to scale the variables before aplying the neural net model . Using min max rescaling
# 2 in the apply function means applying it to variables in dataset
#===========================================



mgrd1 = subset(fulldata, select = c(Gender) )
mgrdi = subset(fulldata, select = c( pitchAutocor_median, pitchSpec_median ,   f3_freq_median , HNR_median ,        
                                quartile75_median,   harmonics_median ,   dom_median   ,pitchSpec_sd    ,   
                                specSlope_sd ,       voiced   ,f3_freq_sd    ,     
                                quartile50_median,   f1_freq_sd  ,dom_sd, harmonics_sd  ,     
                                peakFreqCut_median) )


max = apply(mgrdi , 2 , max)
min = apply(mgrdi, 2 , min)
scaled = as.data.frame(scale(mgrdi, center = min, scale = max - min))
scaled$Gender <- mgrd1$Gender

Train <- createDataPartition(scaled$Gender, p=0.7, list=FALSE)
trainingn <- scaled[ Train, ]
testingn <- scaled[ -Train, ]



set.seed(2)
n <- names(mgrdi)
f <- as.formula(paste("Gender ~", paste(n[!n %in% "trainingn"], collapse = " + ")))
#nn<-neuralnet(f,data=trainingn,hidden=1,linear.output=FALSE,threshold=0.01)
nn<-nnet(f,data=trainingn,size=11)
# parameters to be tuned 
#no of hidden layers
# no of units in hidden layers
# activation function
# decay rate


testingn$pred<-predict(nn,newdata=testingn,type="class")
testingn$pred<-as.factor(testingn$pred)
confusionMatrix(testingn$Gender,testingn$pred)
#===================GBM Model

library(gbm)

t4<-t3
t4$G1<-as.numeric(ifelse(t4$Gender=="Female", "1", "0"))
t4<-subset(t4,select=-c(Gender))

n.trees = seq(from=50 ,to=100, by=10) #no of trees-a vector of 100 values 
# id = seq(from=2 ,to=7, by=1) #no of vars to be considered for a split
# oin = seq(from=50 ,to=100, by=10) #no of vars to be considered for a split
# sh = seq(from=0.01 ,to=1, by=0.1)

t4$G1<-as.factor(t4$G1)

count(t4$G1)

fboost=gbm(G1 ~ . ,data = t4 ,distribution = "bernoulli",n.trees = 100,
           shrinkage = 0.5, interaction.depth = 5)
# distrbution is for type of response  variable its gaussian for continous 
# bernoulli for binaryy response
#Summary gives a table of Variable Importance and a plot of Variable Importance
summary(fboost) 


predmatrix<-predict(fboost,newdata=testing,n.trees = n.trees,type="response")
pred<-"pred"

pr<-paste(pred,n.trees,sep="_")

testing$pred_50 <- predmatrix[,1]
testing$pred_100 <- predmatrix[,6]
testing$p50c <- ifelse(testing$pred_50>0.5,"Female","Male")
testing$p50c<- as.factor(testing$p50c)

testing$pred_90 <- predmatrix[,5]
testing$p90c <- ifelse(testing$pred_90>0.5,"Female","Male")
testing$p90c<- as.factor(testing$p90c)

testing$Gender<- as.factor(testing$Gender)

confusionMatrix(testing$Gender,testing$p90c)
confusionMatrix(testing$Gender,testing$p100c)
