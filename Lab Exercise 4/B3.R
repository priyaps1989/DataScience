## Question 3

# 1. Create a scatter plot for petal length and width variables.
rm(list=ls())
cat("\14")
IrisData <- read.csv("Data Set 4.csv")
plot(IrisData$Petal.Length, IrisData$Petal.Width, xlab="Petal length", ylab="Petal width")

#2 Calculate a liner flower between petal length and width and show it in the scatter plot.
flower<-lm(Petal.Width ~ Petal.Length, data=IrisData)
lines(IrisData$Petal.Length,flower$fitted.values)

#3. Based on the Species data, subdivide the iris dataset into three separate subsets (for each species).
setosa <- subset(IrisData, Species == "setosa")
versicolor <- subset(IrisData, Species == "versicolor")
virginica <- subset(IrisData, Species == "virginica")

#4. Repeat steps 1 and 2 for each subsets
##scatter plot for species setosa
plot(setosa$Petal.Length, setosa$Petal.Width, xlab="Petal length of setosa", ylab="Petal width of setosa")
setosaflower<-lm(Petal.Width ~ Petal.Length, data=setosa) 
lines(setosa$Petal.Length,setosaflower$fitted.values, col= "black")

##scatter plot for species versicolor
plot(versicolor$Petal.Length, versicolor$Petal.Width, xlab="Petal length of versicolor", ylab="Petal width of versicolor") 
versiflower<-lm(Petal.Width ~ Petal.Length, data=versicolor) 
lines(versicolor$Petal.Length,versiflower$fitted.values,col= "blue")

##scatter plot for species virginica
plot(virginica$Petal.Length, virginica$Petal.Width, xlab="Petal length of virginica", ylab="Petal width of virginica")
virginicaflower<-lm(Petal.Width ~ Petal.Length, data=virginica) 
lines(virginica$Petal.Length,virginicaflower$fitted.values, col= "green")

#5. Plot all the results including scatter plots and linear flowers in one plot.
plot(IrisData$Petal.Length, IrisData$Petal.Width, col=c("black","blue","green")[unclass(IrisData$Species)],xlab="Petal length", ylab="Petal width")
flower<-lm(Petal.Width ~ Petal.Length, data=IrisData) 
lines(IrisData$Petal.Length,flower$fitted.values, col= "red")

setosaflower<-lm(Petal.Width ~ Petal.Length, data=setosa) 
lines(setosa$Petal.Length,setosaflower$fitted.values, col= "black")

versiflower<-lm(Petal.Width ~ Petal.Length, data=versicolor) 
lines(versicolor$Petal.Length,versiflower$fitted.values, col= "blue")

virginicaflower<-lm(Petal.Width ~ Petal.Length, data=virginica) 
lines(virginica$Petal.Length,virginicaflower$fitted.values, col= "green")


#6. Show that the linear flowers obtained for the three subsets can describe the relationship between petal length and width more accurately.
#Calculating Error

flower<-lm(Petal.Width ~ Petal.Length, data=IrisData)
error = flower$fitted.values - IrisData$Petal.Width
mean(error^2)

setosaflower<-lm(Petal.Width ~ Petal.Length, data=setosa) 
error1 = setosaflower$fitted.values - setosa$Petal.Width
mean(error1^2)

versiflower<-lm(Petal.Width ~ Petal.Length, data=versicolor)
error2 = versiflower$fitted.values - versicolor$Petal.Width
mean(error2^2) 

virginicaflower<-lm(Petal.Width ~ Petal.Length, data=virginica)
error3 = virginicaflower$fitted.values - virginica$Petal.Width
mean(error3^2) 

#7. Repeat the above steps for sepal length and sepal width data.

##sepal length and width

##scatter plot for width and length of sepal

plot(IrisData$Sepal.Length, IrisData$Sepal.Width, xlab="Sepal length", ylab="Sepal width")
flower<-lm(Sepal.Width ~ Sepal.Length, data=IrisData)
lines(IrisData$Sepal.Length,flower$fitted.values)

##scatter plot for species setosa
setosa <-  subset(IrisData, Species == "setosa")
plot(setosa$Sepal.Length, setosa$Sepal.Width,  xlab="Sepal length of setosa", ylab="Sepal width of setosa")
setosaflower<-lm(Sepal.Width ~ Sepal.Length, data=setosa)
lines(setosa$Sepal.Length,setosaflower$fitted.values, col= "black")


##scatter plot for species versicolor
versicolor <-  subset(IrisData, Species == "versicolor")
plot(versicolor$Sepal.Length, versicolor$Sepal.Width,  xlab="Sepal length of versicolor", ylab="Sepal width of versicolor")
versiflower<-lm(Sepal.Width ~ Sepal.Length, data=versicolor)
lines(versicolor$Sepal.Length,versiflower$fitted.values,col= "blue")

##scatter plot for species virginica
virginica <-  subset(IrisData, Species == "virginica")
plot(virginica$Sepal.Length, virginica$Sepal.Width,  xlab="Sepal length of virginica ", ylab="Sepal width of virginica")
virginicaflower<-lm(Sepal.Width ~ Sepal.Length, data=virginica)
lines(virginica$Sepal.Length,virginicaflower$fitted.values, col= "green")

##scatter plot for all the species
plot(IrisData$Sepal.Length, IrisData$Sepal.Width,  col=c("black","blue","green")[unclass(IrisData$Species)],xlab="Sepal length", ylab="Sepal width")
flower<-lm(Sepal.Width ~ Sepal.Length, data=IrisData)
lines(IrisData$Sepal.Length,flower$fitted.values, col= "red")
setosaflower<-lm(Sepal.Width ~ Sepal.Length, data=setosa)
lines(setosa$Sepal.Length,setosaflower$fitted.values, col= "black")
versiflower<-lm(Sepal.Width ~ Sepal.Length, data=versicolor)
lines(versicolor$Sepal.Length,versiflower$fitted.values, col= "blue")
virginicaflower<-lm(Sepal.Width ~ Sepal.Length, data=virginica)
lines(virginica$Sepal.Length,virginicaflower$fitted.values, col= "green")



flower<-lm(Sepal.Width ~ Sepal.Length, data=IrisData)
error4 = flower$fitted.values - IrisData$Sepal.Width
mean(error4^2)


setosaflower<-lm(Sepal.Width ~ Sepal.Length, data=setosa)
error5 = setosaflower$fitted.values - setosa$Sepal.Width
mean(error5^2)

versiflower<-lm(Sepal.Width ~ Sepal.Length, data=versicolor)
error6 = versiflower$fitted.values - versicolor$Sepal.Width
mean(error6^2)

virginicaflower<-lm(Sepal.Width ~ Sepal.Length, data=virginica)
error7 = virginicaflower$fitted.values - virginica$Sepal.Width
mean(error7^2)


##Question 3 ends