
coorelation <- function(x,directory,threshold)
{
  tempData<-read.csv(paste(directory,"/",x,sep=""))
  good <- complete.cases(tempData)

  corValue <- NA
  if(threshold<nrow(tempData[good,]))
  corValue<-cor(
                x=as.numeric(tempData[good,][["sulfate"]]),
                y=as.numeric(tempData[good,][["nitrate"]])
                )
  
  corValue
#   print(corValue)

}
corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  fileList <- list.files(directory)
  cr <- lapply(fileList,coorelation,directory,threshold)
  
  cr <-as.numeric(cr)
  cr <-cr[!is.na(cr)]
}

# 
# 
# cr<-corr("specData")
# head(cr)
# summary(cr)