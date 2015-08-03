pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  df = data.frame()
  for(i in id){
    if(i<10){
      file = paste(directory,"/00",i,".csv", sep='')
    }
    else if(i < 100){
      file = paste(directory,"/0",i,".csv", sep='')
    }
    else{
      file = paste(directory,"/",i,".csv", sep='')
    }
    df = rbind(df,read.csv(file))
  }
  mm = mean(df[[pollutant]], na.rm=T)
  return(mm)
}
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!


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
  data = data.frame()
  for(i in id){
    if(i<10){
      file = paste(directory,"/00",i,".csv", sep='')
    }
    else if(i < 100){
      file = paste(directory,"/0",i,".csv", sep='')
    }
    else{
      file = paste(directory,"/",i,".csv", sep='')
    }
    
    df = read.csv(file)
    data = rbind(data, data.frame(id = i, nobs = sum(complete.cases(df))))
    
  }
  return(data)
}


corr <- function(directory, id = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  df = data.frame()
  for(i in id){
    if(i < 1)
      return(0)
    if(i<10){
      file = paste(directory,"/00",i,".csv", sep='')
    }
    else if(i < 100){
      file = paste(directory,"/0",i,".csv", sep='')
    }
    else{
      file = paste(directory,"/",i,".csv", sep='')
    }
    df = rbind(df,read.csv(file))
  }
  mm = cor(df[["nitrate"]], df[["sulfate"]])
  return(mm)
}
