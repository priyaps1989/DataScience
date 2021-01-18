complete<-function(directory,id=1:332){             ## 'directory' is a character vector of length 1
                                                    ## 'directory' is a character vector of length 1
                                                    ## indicating the location of the CSV files
  
  entire_files <- list.files(directory)             ##accessing all the csv files in specdata
                                                    ## indicating the location of the CSV files
 
   final_value<-data.frame();                       ##initializing empty dataframe
  
  for (value in id) {                               ## 'id' is an integer vector indicating the monitor ID numbers to be used 
                                            
    file_path=paste(directory,"/" ,entire_files[value], sep="")## joining the file list and directory
    interim<-read.csv(file_path)
    interim<-na.omit(interim)                       ## removing NA values
    final_value<-rbind(final_value,data.frame(id=value,nobs=nrow(interim)))
  }
    return(final_value)                             ##returns a dataframe where id is the monitor id
                                                    ## and nobs is the number of complete cases
}

