library(sqldf)
library(tuneR)
library(seewave)
library(soundgen)

score<- sqldf("select * from fldata where fold not in (select fold from fulldata)")


an<-function (x)
{
  
  a1<-analyzeFolder(x,samplingRate = 16000, summary = TRUE)
  
}

#creating an empty dataset for reading the filenames and attributes

slist1 <- data.frame(
                      fold                    = character(),
                      sound                   = character(),
                      voiced                	=	double(),
                      HNR_median            	=	double(),
                      dom_median            	=	double(),
                      dom_sd                	=	double(),
                      f1_freq_sd            	=	double(),
                      f3_freq_median        	=	double(),
                      f3_freq_sd            	=	double(),
                      harmonics_median      	=	double(),
                      harmonics_sd          	=	double(),
                      peakFreqCut_median    	=	double(),
                      pitchAutocor_median   	=	double(),
                      pitchSpec_median      	=	double(),
                      pitchSpec_sd          	=	double(),
                      quartile50_median     	=	double(),
                      quartile75_median     	=	double(),
                      specSlope_sd          	=	double(),
                      fold                    = character(),
                      rpred                   = character(),
                      stringsAsFactors=FALSE
)

ufldn<-sqldf("select distinct fold from score")
fldn <- ufldn$fold
pth<- "/Users/ssadasivan/Documents/Reme/Sandvik/Rawdata/" 

#Select the no of folders you want to score in 1:x
for (i in 1:10)
{
  fname<-fldn[i]
  sd <- an(paste(pth,fldn[i],"/wav",sep=""))
  sd$fold <- fname
  sd<-subset(sd,select= c(fold,sound,pitchAutocor_median,	pitchSpec_median,	f3_freq_median,	HNR_median,	
                                quartile75_median,	harmonics_median,	dom_median,	pitchSpec_sd,	
                                specSlope_sd,	voiced,f3_freq_sd,	quartile50_median,
                                f1_freq_sd,	dom_sd,harmonics_sd,peakFreqCut_median))
  mnm1<-names(sd)
  mnm1<-mnm1[-c(1,2)]
  sd[mnm1] <- sapply(sd[mnm1],as.character)
  sd[mnm1] <- sapply(sd[mnm1],as.double)
  sd$rpred<-predict(rf3,sd)
  sd<- sqldf("select a.* ,b.Gender from sd a inner join score b on a.fold=b.fold and a.sound=b.filename")
  slist1<-rbind(slist1,sd)
 
}