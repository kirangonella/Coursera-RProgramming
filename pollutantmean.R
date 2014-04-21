pollutantmean <- function(directory, pollutant, id=1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  ## Reading the current directory and appending the directory argument
  ## to move into the required processing folder
  dir <- getwd()
  dir1 <- paste(dir,"/",directory,sep="") 
  ## Initializing a variable for future use 
  x2 <- 0
  ## Loop for going through all the files given in the 'id' argument
  for(i in id) {
    if(i<10) {
      ## Appending two zeros to the variable to maintain the number format in sync
      i <- paste("00",i,sep="")
    } else if(i < 100) {
      ## Appending single zeros to the variable to maintain the number format in sync
      i <- paste("0",i,sep="")
    }
    ## Creating the pattern to identify the required files from 
    ## the available files in the directory
    patn <- paste(i ,sep="",collapse=" ")
    files <- list.files(path=dir1,pattern=patn)
    
    filename <- paste(dir1,"/",files,sep="") 
    if(pollutant == "nitrate") {
      ## Fetching one of the required files to read its contents
      data1 <- read.csv(filename,colClasses=c('NULL','NULL','numeric','NULL'))
    } else if (pollutant == "sulfate") {
      ## Fetching one of the required files to read its contents
      data1 <- read.csv(filename,colClasses=c('NULL','numeric','NULL','NULL'))
    }
    ## Removing NAs from the data frame
    x1 <- na.omit(data1)
    ## rounding off to 3 digits
    x1[1] <- signif(x1[1],3)
    ## Merging different data frames into one
    x2 <- rbind(x2,x1)    
  }
  ## Renaming the column names in the dataframe
  names(x2) <- c("value")
  ## calculating the mean of the "value" column in data frame
  m1 <- mean(x2$value)
  
  ## Printing the rounded value to the console
  print(signif(m1))
  
}
