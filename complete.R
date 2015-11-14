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
  ## Note: Do not round the result!


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

