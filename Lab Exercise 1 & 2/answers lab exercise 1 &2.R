x <- c(4, "a", TRUE)
print(x)


x <- c(1,3, 5) 
y <- c(3, 2, 10)
print(cbind(x,y))

x <- list(2, "a", "b", TRUE)
print(x[[2]])


x <- 1:4  
y <- 2:3
print(x)
print(y)
print(x+y)
w<-x+y
print(w)
class(w)

x <- c(3, 5, 1, 10, 12, 6)
x[x == 0] < 6 ; print(x)
x <- c(3, 5, 1, 10, 12, 6)
 x[x %in% 1:5] <- 0  ; print(x)
 x <- c(3, 5, 1, 10, 12, 6)
 x[x > 0] <- 6  ; print(x)
 x <- c(3, 5, 1, 10, 12, 6)
 x[x > 6] <- 0  ; print(x)
 x <- c(3, 5, 1, 10, 12, 6)
 x[x < 6] == 0  ; print(x)
 x <- c(3, 5, 1, 10, 12, 6)
 x[x != 6] <- 0  ; print(x)
 x <- c(3, 5, 1, 10, 12, 6)
 x[x == 6] <- 0  ; print(x)
 x <- c(3, 5, 1, 10, 12, 6)
 x[x >= 6] <- 0  ; print(x)
 x <- c(3, 5, 1, 10, 12, 6)
 x[x < 6] <- 0  ; print(x)
 x <- c(3, 5, 1, 10, 12, 6)
 x[x <= 5] <- 0  ; print(x)
 x <- c(3, 5, 1, 10, 12, 6)
 x[x == 0] <- 6  ; print(x)
####################################### 
A<-download.file( 'https://moodle.unitec.ac.nz/pluginfile.php/868127/mod_folder/content/0/dataset.zip?forcedownload=1',destfile = "dataset.zip")
A<-unzip("dataset.zip")

A<-read.csv(hw1_data.csv)
 print(A[1:2,])
 
 
 getwd()
 setwd("D:/PGDip in Computing/Introduction to Data Science/ dataset")
 library("readxl")
 A<- read_excel("hw1_data.xls")
 getwd()
 setwd('G:/Data Science Speciality Track/R programming/Quiz')
 download.file(url='https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fquiz1_data.zip',destfile='quiz1.zip')
 data<-read.table(unz("quiz1.zip","hw1_data.csv"),header=T,sep='\t')
 #######################################
 download.file(url='https://moodle.unitec.ac.nz/pluginfile.php/868127/mod_folder/content/0/dataset.zip?forcedownload=1',destfile = 'dataset.zip')
 d1<-read.table(unz("dataset.zip","value.csv"), header=T, sep='\t')
 d1<-read.table(unzip("dataset.zip","value.csv"), header=T, sep='\t')
 ##########################################
 
 
 #######################################
 my_data <- read.table(file = "clipboard", 
                       sep = "\t", header=TRUE)
 print(my_data) 
 my_data[1:2,]

 ############################################################################
  getwd()
 
  dw1_data<-read.table("hw1_data.csv",header = TRUE,sep = ',')
  dw1_data[1:2,]
  nrow(dw1_data)
  dw1_data[47,"Ozone"]
  dw1_data[47,1]
   
  nrow(subset(dw1_data, is.na(Ozone), select = Ozone))
  sub = subset(dw1_data, !is.na(Ozone), select = Ozone)
  sub
  apply(sub, 2, mean) 
  sub = subset(dw1_data, Month == 5 & !is.na(Ozone), select = Ozone)
  apply(sub, 2, max)
  
 initial <- read.table("hw1_data.csv",header = TRUE,sep = ',', nrows = 10)
 initial <- read.table("hw1_data.csv",header = TRUE,sep = ',', nrows = 2)
 print(initial)
 print(nrows(dw1_data))
 colnames(dw1_data)
 
dw1_data[1:2,1:6]
 d1
 classes <- sapply(initial, class) 
 
 tabAll <- read.table("hw1_data.csv",header = TRUE,sep = ',', colClasses = classes)
 
 cube <- function(x, n) {x^3 }; print(cube(3))
 
 x <- 1:10
 if(x > 5) {
   x <- 0
 }
 
 
 
 f <- function(x) {
   g <- function(y) {
     y + z
   }
   z <- 4
   x + g(x)
 }
 
 z <- 10
 print(f(3))
 
 x <- 5
 y <- if(x < 3) {
   NA
 } else {
   10
 }
 print(y)
 
 
 h <- function(x, y = NULL, d = 3L) {
   z <- cbind(x, d)
   if(!is.null(y))
     z <- z + y
   else
     z <- z + f
   g <- x + y / z
   if(d == 3L)
     return(g)
   g <- g + 10
   g
 }
 print(x)
 
 
 library(datasets)
 data(iris)
 iris
 v<-subset(iris,Species == 'virginica')
 v
 #sl<-data.frame(v$Sepal.Length)

 # sl<-data.frame(iris$Sepal.Length[iris$Species=="virginica"])
#sl
# summary(sl) #  Mean   :6.588 
 #print(mean(sl))
 sub = subset(v, select = Sepal.Length)
 sub
 apply(sub, 2, mean) 
 colMeans(iris)
 apply(iris[, 1:4], 2, mean)
 
 y<-10
 f<-function (x) {
   y <- 2
   y^2 + g(x)
 }
 g<-function(x) {
   x*y
 }
 print(f(3))
 f(3)