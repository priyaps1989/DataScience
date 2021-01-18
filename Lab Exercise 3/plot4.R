PowerUsage<-read.csv("Dataset.txt",header=TRUE, sep=";")
png(file="plot4.png", width = 630, height = 360)
par(mfrow = c(3,1), mar=c(4, 4, 2,1))

hist(subset(PowerUsage, Sub_metering_2==0.000)$Global_active_power, main="Global Active Power for Sub_metering_2=0.000",xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red", xlim=c(0,8), ylim=c(0, 60000))
hist(subset(PowerUsage, Sub_metering_2==1.000)$Global_active_power, main="Global Active Power for Sub_metering_2=1.000",xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red", xlim=c(0,8), ylim=c(0, 20000))
hist(subset(PowerUsage, Sub_metering_2==2.000)$Global_active_power, main="Global Active Power for Sub_metering_2=2.000",xlab="Global Active Power (kilowatts)", ylab="Frequency", col="Red", xlim=c(0,8), ylim=c(0, 8000))

dev.off()
cat("plot4.png has been saved in", getwd())

