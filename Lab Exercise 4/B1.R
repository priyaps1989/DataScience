## Question 1

# clean the raw data
rm(list=ls())
cat("\14")
#install.packages('readr')
library(readr)

# Read the dataset
Sales <- read.csv("Data Set 1a.csv",na.strings = "")

#function to convert word-number to int
wordtonum <- function(word){
  num <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                     six=6, seven=7, eight=8, nine=9) #print(word)
  for(i in word) {
    if(!is.null(num[[i]])) {
      word[word==i]<-num[[i]]
    } }
  
  return(word) }

#  change quantity column to numeric type
Sales$Quantity <- wordtonum(Sales$Quantity)

#  set as numeric for quantity
Sales$Quantity = as.numeric(Sales$Quantity)

#  change spelling for products 
levels(Sales$Product) <- c(levels(Sales$Product),"Galaxy Edge S8")
Sales$Product[Sales$Product == "Galaxi Edge S8"] <-"Galaxy Edge S8"

#  Correcting fonststyle of customer name
levels(Sales$Name) <- c(levels(Sales$Name), "Alice")
Sales$Name[Sales$Name == "ALice"] <- "Alice"

#omitting data with missing customer name 
Sales<-na.omit(Sales)

#install.packages('lubridate')
library(lubridate) # for date
#  date format clean up 
Sales$Date = dmy(Sales$Date)

# Removing Anomalous Data
levels(Sales$Date) <- c(levels(Sales$Date),NA)
Sales$Date[Sales$Date == "1978-09-02"] <-NA
Sales<-na.omit(Sales)

# function for forex rate calculation
ForexRate<- function(arg1){
  if (arg1=='NZD') {out<-1} 
  if (arg1=='AUD') {out<-1.2} 
  return(out)
}

#read csv for unit price for products
Products <- read.csv("Data Set 1b.csv")

#  combine Sales table and Products by product
MergeTable = merge(Sales,Products,"Product")

#  apply the forex rates.
MergeTable["Exchange.Rate"]<-sapply(MergeTable$Currency,ForexRate)
MergeTable$Unit.Price.NZD<-MergeTable$Unit.Price*MergeTable$Exchange.Rate 
MergeTable = na.omit(MergeTable)


# Step 1. Omitting all the Sales made before 1 Jan 2010
FinalSales = subset(MergeTable,MergeTable$Date >'2010-01-01')

#install.packages('zoo') 
library(zoo)
#install.packages('plyr')
library(plyr)
#  Step 2. Calculating monthly total sale
FinalSales$Month_Year=as.yearmon(FinalSales$Date)
MonthYearColumns = c('Month_Year')
QuantityPriceColumns = c('Quantity','Unit.Price.NZD') 
MonthlySales=head(ddply(FinalSales,MonthYearColumns,function(x) colSums(x[QuantityPriceColumns])))


install.packages('plotly')
library(plotly)
packageVersion('plotly')
# Step 3. Visualizing the results (monthly total sale) by using appropriate charts/plots.
VisualData=data.frame(MonthlySales) 
barplot(VisualData$Quantity, main="Monthly Total Sales", xlab="Month & Year",names.arg=c(VisualData$Month_Year), ylab = "Quantities Sold")




#Question 1 ends

