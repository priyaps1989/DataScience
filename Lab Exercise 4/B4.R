## Question 4

rm(list=ls())
cat("\14")
Power<- read.csv("Data Set 5.csv") 
par(mfrow = c(1,3), mar=c(4, 4, 2,1))

#1. Create a scatter plot for the two variables.
#Plotting the scatter plot for Size,KW variables 
plot(Power$Size, Power$KW, xlab ="x" ,ylab = "y" , main="Observed Data")
#plotting the linear,polynomial regression models
data1<- lm(Power$KW~poly(Power$Size,1))
lines(Power$Size, data1$fitted.values, col="Red")

plot(Power$Size, Power$KW, xlab ="x" ,ylab = "y" , main="Observed Data")
data2<- lm(Power$KW~poly(Power$Size,2))
lines(Power$Size, data2$fitted.values, col="Red")

plot(Power$Size, Power$KW, xlab ="x" ,ylab = "y" , main="Observed Data")
data3<- lm(Power$KW~poly(Power$Size,3))
lines(Power$Size, data3$fitted.values, col="Red")


#Compare the accuracy of the three models   
error1 = data1$fitted.values - Power$KW
mean(error1^2)

error2 = data2$fitted.values - Power$KW
mean(error2^2)

error3 = data3$fitted.values - Power$KW
mean(error3^2)

##Question 4 ends