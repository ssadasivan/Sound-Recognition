library(RCurl)
library(stringr)

#Scrping the web page to get details of all the files to download
webpage = getURL("http://www.repository.voxforge1.org/downloads/SpeechCorpus/Trunk/Audio/Main/16kHz_16bit/")


tc = textConnection(webpage)
contents = readLines(tc)
close(tc)

# Extracting  each zipped folder name
RX<- regexpr('(?<=>)([A-Z0-9._-]{1,50})',text=contents, ignore.case=TRUE,perl=TRUE)
urls = unlist(regmatches(contents, RX))
head(urls)

urls<-urls[-c(1,2,3,4,5,6,7,8,9)]

head(urls)
# generate the full URLS

baseurl<- "http://www.repository.voxforge1.org/downloads/SpeechCorpus/Trunk/Audio/Main/16kHz_16bit/"

#Folder for downloading the wav files
based="/Users/ssadasivan/Documents/Reme/Sandvik/Rawdata/"


for (i in 1:6242)
{
  p<-paste0(baseurl,urls[i])
  d<-paste0(based, urls[i])
  download.file(url = p, destfile = d)

}



#unzip all files 

fn<- list.files("/Users/ssadasivan/Documents/Reme/Sandvik/Rawdata")

setwd("/Users/ssadasivan/Documents/Reme/Sandvik/Rawdata")

for (i in fn)
  untar(i)

#getting directory names
fnd<- list.dirs("/Users/ssadasivan/Documents/Reme/Sandvik/Rawdata" ,recursive = FALSE)

#getting file names 
fl1 <- list.files("/Users/ssadasivan/Documents/Reme/Sandvik/Rawdata/1snoke-20120412-hge/wav")

#Creating a R dataset for storing the folder name , wav file names and Gender for each file

fldata<-data.frame(fold=character(),filename=character(),Gender=character(), stringsAsFactors=FALSE)

for (i in 1:6186)
{

  f <- readLines(paste(fnd[i],"/etc/README",sep=""))
  fln <- list.files(paste(fnd[i],"/wav",sep=""))
  fl<-length(fln)
  cline <- grep("Gender",f,  value=TRUE)
  s<-str_extract_all(cline,boundary("word"),simplify = TRUE)
  nr<-nrow(fldata)
  print(nr)

  for (k in 1:fl)
    {
    print(k)
    print(fln[k])
    r<-nr+k
    fldata[r,1]<-substr(fnd[i],start=50,stop=100)
    fldata[r,2]<-fln[k]
    fldata[r,3]<-s[,2]

  }
}


setwd("/Users/ssadasivan/Documents/Reme/Sandvik")

saveRDS(fldata,"fldata.rda")

fldata <- readRDS(file="fldata.rda")
