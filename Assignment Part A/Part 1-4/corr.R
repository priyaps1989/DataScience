

corr<-function(directory,threshold =0){
  source("complete.R")
  finalized = complete(directory, 1:332)
  finalized_above_threshold = subset(finalized, nobs > threshold )
  final_value<-vector(mode="numeric",length=0)      ## create empty vector
                                                    
  
  entire_files <- list.files(directory)             ##accessing all the csv files in specdata
  
  for(value in finalized_above_threshold$id){
    file_path=paste(directory,"/" ,entire_files[value], sep="")
    interim<-read.csv(file_path)
    
    interim<-interim[complete.cases(interim),]      ##complete.cases returns logical value for each row
                                                    ##in order to keep only the complete rows interim[complete.cases(interim),]
                                                    ##storing complete cases subset in a new data frame
    
    nobs<-nrow(interim)                             ## number of complete observation
    
    if(nobs>threshold){                             ##for such files,we find correlation between nitrate & Sulphate
      final_value<-c(final_value,cor(interim$nitrate,interim$sulfate)) ##concatenation function is used combine each 
                                                    ##correlation for each files in vector format
      
    }
  }
  
  return(final_value)
  
}