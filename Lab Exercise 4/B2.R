## Question 2
install.packages("e1071")
rm(list=ls())
cat("\14")
MainData<- read.csv("Data Set 3.csv")
#1 Plot the histograms of L, H, N, B in one page.
par(mfrow = c(2,2), mar=c(4, 4, 2,1))
hist( MainData$L,col = "Sky Blue", xlab = "L", main = "Histogram of L")
hist( MainData$H,col = "Sky Blue", xlab = "H", main = "Histogram of H")
hist( MainData$N,col = "Sky Blue", xlab = "N", main = "Histogram of N")
hist( MainData$B,col = "Sky Blue", xlab = "B", main = "Histogram of B")

#2 Plot the density functions of L, H, N, B in one page
par(mfrow = c(2,2), mar=c(4, 4, 2,1))
plot(density(MainData$L), xlab = "N= 150  Bandwidth= 1.726" , main= "Density function of L")
plot(density(MainData$H), xlab = "N= 150  Bandwidth= 1.632" , main= "Density function of H") 
plot(density(MainData$N), xlab = "N= 150  Bandwidth= 0.9862" , main="Density function of N") 
plot(density(MainData$B), xlab = "N= 150  Bandwidth= 1.479" , main= "Density function of B")


#3 Compare the density functions against a normal density function. Comments on the symmetry and sharpness of the density functions.   [0.4 Marks]

library(e1071)
skewness(MainData$L) 
skewness(MainData$H) 
skewness(MainData$N) 
skewness(MainData$B)

kurtosis(MainData$L) 
kurtosis(MainData$H) 
kurtosis(MainData$N) 
kurtosis(MainData$B)

#4 Create the boxplots of L, H, N and B using a similar scale. 
par(mfrow = c(1,4), mar=c(4, 4, 2,1)) 
boxplot(MainData$L,main = "L")
boxplot(MainData$H,main = "H") 
boxplot(MainData$N,main = "N") 
boxplot(MainData$B,main = "B")


#5 Calculate the mean, variance and standard deviation of L, H, N and B
mean(MainData$L)
mean(MainData$H)
mean(MainData$N)
mean(MainData$B)

var(MainData$L)
var(MainData$H)
var(MainData$N)
var(MainData$B)

sd(MainData$L)
sd(MainData$H)
sd(MainData$N)
sd(MainData$B)

#Question 2 ends

