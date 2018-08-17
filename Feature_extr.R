
library(tuneR)
library(seewave)
library(soundgen)
#Extracting first set of features from soundgen:analyze

an<-function (x)
{
 
  a1<-analyzeFolder(x,samplingRate = 16000, summary = TRUE)

}

#creating an emplty dataset

flist1 <- data.frame(
  sound                   = character(),
  duration              	=	double(),
  voiced                	=	double(),
  HNR_mean              	=	double(),
  HNR_median            	=	double(),
  HNR_sd                	=	double(),
  ampl_mean             	=	double(),
  ampl_median           	=	double(),
  ampl_sd               	=	double(),
  amplVoiced_mean       	=	double(),
  amplVoiced_median     	=	double(),
  amplVoiced_sd         	=	double(),
  dom_mean              	=	double(),
  dom_median            	=	double(),
  dom_sd                	=	double(),
  entropy_mean          	=	double(),
  entropy_median        	=	double(),
  entropy_sd            	=	double(),
  f1_freq_mean          	=	double(),
  f1_freq_median        	=	double(),
  f1_freq_sd            	=	double(),
  f1_width_mean         	=	double(),
  f1_width_median       	=	double(),
  f1_width_sd           	=	double(),
  f2_freq_mean          	=	double(),
  f2_freq_median        	=	double(),
  f2_freq_sd            	=	double(),
  f2_width_mean         	=	double(),
  f2_width_median       	=	double(),
  f2_width_sd           	=	double(),
  f3_freq_mean          	=	double(),
  f3_freq_median        	=	double(),
  f3_freq_sd            	=	double(),
  f3_width_mean         	=	double(),
  f3_width_median       	=	double(),
  f3_width_sd           	=	double(),
  harmonics_mean        	=	double(),
  harmonics_median      	=	double(),
  harmonics_sd          	=	double(),
  medianFreq_mean       	=	double(),
  medianFreq_median     	=	double(),
  medianFreq_sd         	=	double(),
  peakFreq_mean         	=	double(),
  peakFreq_median       	=	double(),
  peakFreq_sd           	=	double(),
  peakFreqCut_mean      	=	double(),
  peakFreqCut_median    	=	double(),
  peakFreqCut_sd        	=	double(),
  pitch_mean            	=	double(),
  pitch_median          	=	double(),
  pitch_sd              	=	double(),
  pitchAutocor_mean     	=	double(),
  pitchAutocor_median   	=	double(),
  pitchAutocor_sd       	=	double(),
  pitchCep_mean         	=	double(),
  pitchCep_median       	=	double(),
  pitchCep_sd           	=	double(),
  pitchSpec_mean        	=	double(),
  pitchSpec_median      	=	double(),
  pitchSpec_sd          	=	double(),
  quartile25_mean       	=	double(),
  quartile25_median     	=	double(),
  quartile25_sd         	=	double(),
  quartile50_mean       	=	double(),
  quartile50_median     	=	double(),
  quartile50_sd         	=	double(),
  quartile75_mean       	=	double(),
  quartile75_median     	=	double(),
  quartile75_sd         	=	double(),
  specCentroid_mean     	=	double(),
  specCentroid_median   	=	double(),
  specCentroid_sd       	=	double(),
  specCentroidCut_mean  	=	double(),
  specCentroidCut_median	=	double(),
  specCentroidCut_sd    	=	double(),
  specSlope_mean        	=	double(),
  specSlope_median      	=	double(),
  specSlope_sd          	=	double(),
  fold                    = character(),
  stringsAsFactors=FALSE
)




for (i in 1:3500)
{
  fname<-substr(fnd[i],start=50,stop=100)
  d <- an(paste(fnd[i],"/wav",sep=""))
  d$fold <- fname
  flist1<-rbind(flist1,d)
}

print(a)
print(Sys.time())
setwd("/Users/ssadasivan/Documents/Reme/Sandvik")
saveRDS(flist1,"flist1.rda")

flist1 <- readRDS(file="flist1.rda")


#====================== extracting the second set of features from TUNE R package

flist2 <- data.frame(
  fold    = character(),
  filename  = character(),
  skewf   = double(),
  kurtf   = double(),
  sdf   = double(),
  stringsAsFactors=FALSE
)

for (i in 1:3500)
{
  fln <- list.files(paste(fnd[i],"/wav",sep=""))
  fl<-length(fln)
  print(fnd[i])
  nr<-nrow(flist2)
  for (j in 1:fl)
  {
  setwd(paste(fnd[i],"/wav",sep=""))
  sndObj <- readWave(fln[j])
  print(fln[j])
  
  s1 <- sndObj@left
  s1 <- s1 / 2^(sndObj@bit -1)
  n <- length(s1)
  p <- fft(s1)
  nUniquePts <- ceiling((n+1)/2)
  p <- p[1:nUniquePts] 
  p <- abs(p) 
  p <- p / n 
  p <- p^2 
  if (n %% 2 > 0)
    {
    p[2:length(p)] <- p[2:length(p)]*2 
    } 
  else 
    {
    p[2: (length(p) -1)] <- p[2: (length(p) -1)]*2 
    }
  freqArray <- (0:(nUniquePts-1)) * (sndObj@samp.rate / n)
  r<-nr+j
  
  
  flist2[r,1] <- (substr(fnd[i],start=50,stop=100))
  flist2[r,2] <- fln[j]
  flist2[r,3] <- skewness(freqArray)
  flist2[r,4] <- kurtosis(freqArray)
  flist2[r,5] <- sd(freqArray)
remove(sndObj)
}
}
saveRDS(flist2,"flist2.rda")
setwd("/Users/ssadasivan/Documents/Reme/Sandvik/")
flist2 <- readRDS(file="flist2.rda")
