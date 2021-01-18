PowerUsage<-read.csv("Dataset.txt",header=TRUE, sep=";")
png(file="plot2.png", width = 550, height = 300)
boxplot(subset(PowerUsage, Voltage==237)$Global_active_power, col="red", main="Global Active Power",xlab = "Global Active Power (kilowatts) for Voltage 237", ylab = "Frequency")
dev.off()  
cat("plot2.png has been saved in",getwd())

