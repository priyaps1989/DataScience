source("pollutantmean.R")
source("complete.R")
source("corr.R")
source("pollutantvector.R")


pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 23)

complete("specdata", 1)
complete("specdata", 30:25)


cr <- corr("specdata", 150)
cr
head(cr)
summary(cr)

pollutantvector("specdata", "sulfate", 1:35, 0.5)