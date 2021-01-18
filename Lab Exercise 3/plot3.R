PowerUsage<-read.csv("Dataset.txt",header=TRUE, sep=";")

png(file="plot3.png",width = 630, height = 360)

with(PowerUsage, plot(Voltage,Global_active_power), main = "Global Active Power (kilowatts) for Voltage >=249")
with(subset(PowerUsage, Voltage >= 249),plot(Voltage, Global_active_power))
with(subset(PowerUsage, Voltage >= 249 && Voltage<249),points(Voltage, Global_active_power))
with(subset(PowerUsage, Voltage>=250), points(Voltage, Global_active_power, col="red"))
legend("topright", pch = 1, col = c("black","red"), legend = c("Voltage >=249 & <250", "Voltage >= 250"))
dev.off()
cat("plot3.png has been saved in", getwd())