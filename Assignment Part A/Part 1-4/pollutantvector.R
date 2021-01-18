pollutantvector<-function(directory,pollutant,id=1:332, p){
                                                                 ## 'directory' is a character vector of length 1
                                                                 ## indicating the location of the CSV files
  
  entire_files <- list.files(directory)                         ##accessing all the csv files in specdata
  
  #interim<-data.frame()
  final_value<-vector(mode="numeric",length=0)           ## create empty vector
  
  for (value in id) { 
    file_path=paste(directory,"/" ,entire_files[value], sep="")
    interim<-rbind(interim,read.csv(file_path))
    interim<-interim[pollutant]
    interim<-interim[complete.cases(interim),]
    
    for(i in 1: length(interim)){
      if(interim[i]>p){
        
        #final_value<-rbind(final_value,data.frame(id=value,pollutant=interim[i]))
        final_value<-c(final_value,interim[i])
      }
    }}
  
  return(final_value)
}


