
complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  dataSet=data.frame()
  for(i in 1:length(id))
  {
    tempDataSet = read.csv(
      paste(directory,
            "/",
            formatC(id[i],
                    width=3, 
                    flag=0),
            ".csv",
            sep="")
    )
    y <- complete.cases(tempDataSet)
    x<-c(id[i],nrow(tempDataSet[y,]))
    dataSet <- rbind(dataSet,x)
  }
  names(dataSet)<- c("id","nobs")
  
  dataSet
}

# complete("specdata", 1)
# complete("specdata", c(2, 4, 8, 10, 12))
# complete("specdata", 30:25)
# complete("specdata", 3)
