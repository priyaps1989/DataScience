
pollutantmean <- function(directory, pollutant, id = 1:332) { ## 'directory' is a character vector of length 1
                                                              ## indicating the location of the CSV files
  
                                                              ## 'pollutant' is a character vector of length 1 
                                                              ##indicating the name of  the pollutant for which 
                                                              ##we will calcultate the mean; either "sulfate" or "nitrate"
                                                              ## 'id' is an integer vector indicating the monitor 
                                                              ##ID numbersto be used
                                                              
                                                              
                                                              ## 'directory' is a character vector of length 1
  
                                                              ## indicating the location of the CSV files
  entire_files <- list.files(directory)                       ##accessing all the csv files in specdata
  
  interim<-data.frame()                                       ## creating empty data frame                             
  
  for (value in id) {                                          ## to search the entire csv files to find the 
    file_path=paste(directory,"/" ,entire_files[value], sep="")##corresponding monitor id and join the file list and directory
    
    interim<-rbind(interim,read.csv(file_path))                ##assigning the particular csv file and binding 
    
  }
  return(mean(interim[,pollutant],na.rm = TRUE))               ## Return the mean of the pollutant across all monitors list
                                                               ## in the 'id' vector (ignoring NA values)
                                                               ## NOTE: Do not round the result
                                                           
                                                          
}

