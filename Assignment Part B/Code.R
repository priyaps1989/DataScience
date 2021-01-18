#Import Libarires
library(ggplot2) #Data visulization 
library (gridExtra)# laying multiple plots on a page
library(GGally)#extends ggplot
library(readr) # To read CSV File
library(dplyr) # efficient manipulation of dataset
library(reshape2) # flexibly reshape data
library(RColorBrewer)# color palettes for graphing
install.packages("e1071")
library(e1071)#skewness and kurtosis
library(MASS)
install.packages("corrplot")
library(corrplot)

# Read the data
Wine_List<- read.csv("whitewine.csv",na.strings="")

#Data elaboration and cleaning
class(Wine_List)
names(Wine_List)
str(Wine_List)

#missing value analysis
sum(complete.cases(Wine_List))

#Remove records with NA
sum(is.na(Wine_List))# no NA values

#Check for duplicates and remove it
sum(duplicated(Wine_List))
Wine_List <- Wine_List[!duplicated(Wine_List), ]

# converting mg/dm3 to g/dm3
Wine_List$free.sulfur.dioxide<-.001*Wine_List$free.sulfur.dioxide
Wine_List$total.sulfur.dioxide<-.001*Wine_List$total.sulfur.dioxide

#Creating a new Factored Variable called 'Rating'

Wine_List$rating <- ifelse(Wine_List$quality < 5, 'bad', ifelse(
  Wine_List$quality < 7, 'average', 'good'))

Wine_List$rating <- ordered(Wine_List$rating,
                       levels = c('bad', 'average', 'good'))
#Adding label as alcohol percentage to group the alcohol concentration 
Wine_List$label <- ifelse(Wine_List$alcohol <=9, 'light', ifelse(
  Wine_List$alcohol <=12, 'medium', 'strong'))

Wine_List$label <- ordered(Wine_List$label,
                            levels = c('light', 'medium', 'strong'))


##Univariate Analysis
# 1. Frequency
table(Wine_List$rating)
table(Wine_List$quality)
table(Wine_List$label)


# 2 percentage table
100*(table(Wine_List$rating)/sum(table(Wine_List$rating)))
100*(table(Wine_List$quality)/sum(table(Wine_List$quality)))
100*(table(Wine_List$label)/sum(table(Wine_List$label)))

# 3 Location
## Mean 
mean(Wine_List$fixed.acidity)
mean(Wine_List$volatile.acidity)
mean(Wine_List$citric.acid)
mean(Wine_List$residual.sugar)
mean(Wine_List$chlorides)
mean(Wine_List$free.sulfur.dioxide)
mean(Wine_List$total.sulfur.dioxide)
mean(Wine_List$density)
mean(Wine_List$pH)
mean(Wine_List$sulphates)
mean(Wine_List$alcohol)

## Median
median(Wine_List$fixed.acidity)
median(Wine_List$volatile.acidity)
median(Wine_List$citric.acid)
median(Wine_List$residual.sugar)
median(Wine_List$chlorides)
median(Wine_List$free.sulfur.dioxide)
median(Wine_List$total.sulfur.dioxide)
median(Wine_List$density)
median(Wine_List$pH)
median(Wine_List$sulphates)
median(Wine_List$alcohol)

## 4	Spread (minimum, maximum, range, quartiles, standard deviation, etc.)

# min values
min(Wine_List$fixed.acidity)
min(Wine_List$volatile.acidity)
min(Wine_List$citric.acid)
min(Wine_List$residual.sugar)
min(Wine_List$chlorides)
min(Wine_List$free.sulfur.dioxide)
min(Wine_List$total.sulfur.dioxide)
min(Wine_List$density)
min(Wine_List$pH)
min(Wine_List$sulphates)
min(Wine_List$alcohol)

# max values
max(Wine_List$fixed.acidity)
max(Wine_List$volatile.acidity)
max(Wine_List$citric.acid)
max(Wine_List$residual.sugar)
max(Wine_List$chlorides)
max(Wine_List$free.sulfur.dioxide)
max(Wine_List$total.sulfur.dioxide)
max(Wine_List$density)
max(Wine_List$pH)
max(Wine_List$sulphates)
max(Wine_List$alcohol)

# range
range(Wine_List$fixed.acidity)
range(Wine_List$volatile.acidity)
range(Wine_List$citric.acid)
range(Wine_List$residual.sugar)
range(Wine_List$chlorides)
range(Wine_List$free.sulfur.dioxide)
range(Wine_List$total.sulfur.dioxide)
range(Wine_List$density)
range(Wine_List$sulphates)
range(Wine_List$alcohol)


#variance
var(Wine_List$fixed.acidity)
var(Wine_List$volatile.acidity)
var(Wine_List$citric.acid)
var(Wine_List$residual.sugar)
var(Wine_List$chlorides)
var(Wine_List$free.sulfur.dioxide)
var(Wine_List$total.sulfur.dioxide)
var(Wine_List$density)
var(Wine_List$pH)
var(Wine_List$sulphates)
var(Wine_List$alcohol)


#Standard Deviation
sd(Wine_List$fixed.acidity)
sd(Wine_List$volatile.acidity)
sd(Wine_List$citric.acid)
sd(Wine_List$residual.sugar)
sd(Wine_List$chlorides)
sd(Wine_List$free.sulfur.dioxide)
sd(Wine_List$total.sulfur.dioxide)
sd(Wine_List$density)
sd(Wine_List$pH)
sd(Wine_List$sulphates)
sd(Wine_List$alcohol)

#skewness and kurtosis
skewness(Wine_List$fixed.acidity)
skewness(Wine_List$volatile.acidity)
skewness(Wine_List$citric.acid)
skewness(Wine_List$residual.sugar)
skewness(Wine_List$chlorides)
skewness(Wine_List$free.sulfur.dioxide)
skewness(Wine_List$total.sulfur.dioxide)
skewness(Wine_List$density)
skewness(Wine_List$pH)
skewness(Wine_List$sulphates)
skewness(Wine_List$alcohol)

kurtosis(Wine_List$fixed.acidity)
kurtosis(Wine_List$volatile.acidity)
kurtosis(Wine_List$citric.acid)
kurtosis(Wine_List$residual.sugar)
kurtosis(Wine_List$chlorides)
kurtosis(Wine_List$free.sulfur.dioxide)
kurtosis(Wine_List$total.sulfur.dioxide)
kurtosis(Wine_List$density)
kurtosis(Wine_List$pH)
kurtosis(Wine_List$sulphates)
kurtosis(Wine_List$alcohol)

# Bar Chart
#Bar Chart of Quality, Rating and Alcohol content
grid.arrange(ggplot(data = Wine_List, aes(x = quality)) +
  geom_bar(width = 1, color = 'black',fill = I('grey')) +
    labs(y= "Count", x = "Quality")+ggtitle("White Wine Quality"),

ggplot(data = Wine_List, aes(x = rating)) +
  geom_bar(width = 1, color = 'black',fill = I('slate gray2'))+
  labs(y= "Count", x = "Rating")+ggtitle("Rating of White Wine"),

ggplot(data = Wine_List, aes(x = label)) +
  geom_bar(width = 1, color = 'black',fill = I('slate gray3'))+
  labs(y= "Count", x = "Alcohol Content")+ggtitle("Alcohol Content in White Wine"),nrow=1)


#Histogram and Boxplot
 
 #Histogram and Boxplot of Fixed Acidity
 grid.arrange(ggplot(Wine_List, aes( x = 1, y = fixed.acidity ) ) + 
                geom_jitter(alpha = 0.1 ) +
                geom_boxplot(alpha = 0.2, color = 'red' ) ,
              ggplot(data = Wine_List, aes(x = fixed.acidity)) +
                geom_histogram(binwidth = 1, color = 'black',fill = I('slate gray2')),ncol = 2)
 
 #Histogram and Boxplot of Volatile Acidity
 grid.arrange(ggplot(Wine_List, aes( x = 1, y = volatile.acidity ) ) + 
                geom_jitter(alpha = 0.1 ) +
                geom_boxplot(alpha = 0.2, color = 'red' ),
              ggplot(data = Wine_List, aes(x = volatile.acidity)) +
                geom_histogram(binwidth = 0.05, color = 'black',fill = I('slate gray2')), ncol = 2)
 
 #Histogram and Boxplot of Citric Acid
 grid.arrange(ggplot(Wine_List, aes( x = 1, y = citric.acid )) + 
                geom_jitter(alpha = 0.1 ) +
                geom_boxplot(alpha = 0.2, color = 'red' ),
              ggplot(data = Wine_List, aes(x = citric.acid)) +
                geom_histogram(binwidth = 0.08, color = 'black',fill = I('slate gray2')),ncol = 2)
 
 #Histogram and Boxplot of Residual Sugar
 grid.arrange(ggplot(Wine_List, aes( x = 1, y = residual.sugar )) + 
                geom_jitter(alpha = 0.1 ) +
                geom_boxplot(alpha = 0.2, color = 'red' ) ,
              ggplot(data = Wine_List, aes(x = residual.sugar)) +
                geom_histogram(binwidth = 4, color = 'black',fill = I('slate gray2')), ncol = 2)

  #Residual sugar distribution is skewed to the left. This actually can be fixed by changing  into log distribution form.
   ggplot(aes(x=residual.sugar),data=Wine_List) +
   geom_histogram(binwidth = .04, color = 'black',fill = I('slate gray2'))+
   scale_x_log10( breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))) +
   ggtitle("Residual Sugar") + xlab("Log 10 Residual Sugar") + ylab("Count")
  
  

 #Histogram and Boxplot of Chlorides
 grid.arrange(ggplot(Wine_List, aes( x = 1, y = chlorides )) + 
                geom_jitter(alpha = 0.1 ) +
                geom_boxplot(alpha = 0.2, color = 'red' ),
              ggplot(data = Wine_List , aes(x = chlorides)) +
                geom_histogram(binwidth = 0.02, color = 'black',fill = I('slate gray2')), ncol = 2)

 #Chlorides distribution is skewed to the left. This actually can be fixed by changing  into log distribution form.
 ggplot(aes(x=chlorides),data=Wine_List) +
   geom_histogram(binwidth = 0.02, color = 'black',fill = I('slate gray2'))+
   scale_x_log10( breaks = scales::trans_breaks("log10", function(x) 10^x),
                  labels = scales::trans_format("log10", scales::math_format(10^.x))) +
   ggtitle("Chlorides") + xlab("Log 10 Chlorides") + ylab("Count")
 
 #Histogram and Boxplot of Free Sulfur Dioxode
 grid.arrange(ggplot(Wine_List, aes( x = 1, y = free.sulfur.dioxide )) + 
                geom_jitter(alpha = 0.1 ) +
                geom_boxplot(alpha = 0.2, color = 'red' ) ,
              ggplot(data = Wine_List, aes(x = free.sulfur.dioxide)) +
                geom_histogram(binwidth = .01, color = 'black',fill = I('slate gray2')), ncol = 2)
 
  #Histogram and Boxplot of Total Sulfur Dioxide
 grid.arrange(ggplot(Wine_List, aes( x = 1, y = total.sulfur.dioxide )) + 
                geom_jitter(alpha = 0.1 ) +
                geom_boxplot(alpha = 0.2, color = 'red' ),
              ggplot(data = Wine_List, aes(x = total.sulfur.dioxide)) +
                geom_histogram(binwidth = .01, color = 'black',fill = I('slate gray2')), ncol = 2)
 
 #Histogram and Boxplot of Density
 grid.arrange(ggplot(Wine_List, aes( x = 1, y = density)) + 
                geom_jitter(alpha = 0.1 ) +
                geom_boxplot(alpha = 0.2, color = 'red' ),
              ggplot(data = Wine_List, aes(x = density)) +
                geom_histogram(binwidth = 0.003, color = 'black',fill = I('slate gray2')), ncol = 2)
 
 
 #Histogram and Boxplot of pH
 grid.arrange(ggplot(Wine_List, aes( x = 1, y = pH)) + 
                geom_jitter(alpha = 0.1 ) +
                geom_boxplot(alpha = 0.2, color = 'red' ),
              ggplot(data = Wine_List, aes(x = pH)) +
                geom_histogram(binwidth = 0.06, color = 'black',fill = I('slate gray2')), ncol = 2)
 
 #Histogram and Boxplot of Sulphates
  grid.arrange(ggplot(Wine_List, aes( x = 1, y = sulphates)) + 
                geom_jitter(alpha = 0.1 ) +
                geom_boxplot(alpha = 0.2, color = 'red' ) ,
              ggplot(data = Wine_List, aes(x = sulphates)) +
                geom_histogram(binwidth = 0.06, color = 'black',fill = I('slate gray2')) , ncol = 2)
 
  #Histogram and Boxplot of Alcohol
  grid.arrange(ggplot(Wine_List, aes( x = 1, y = alcohol)) + 
                 geom_jitter(alpha = 0.1 ) +
                 geom_boxplot(alpha = 0.2, color = 'red' ),
               ggplot(data = Wine_List, aes(x = alcohol)) +
                 geom_histogram(binwidth = 0.4, color = 'black',fill = I('slate gray2')), ncol = 2)
  
  
  ##Bivariate Analysis
  # Frequency
  table( Wine_List$rating, Wine_List$label) 
  
  #	Percentage
  100*(table(Wine_List$rating, Wine_List$label)/sum(table(Wine_List$rating, Wine_List$label)))
  
  #Correlation
  wine_quality_cor <- as.matrix(subset( Wine_List, select = -c(quality, rating, label)))
  response <- Wine_List$rating
  corrplot(cor(wine_quality_cor), method = c("circle"))
  corrplot(cor(wine_quality_cor), method = c("number"))
  new<-subset( Wine_List, select = -c( quality, rating, label))
  ggpairs(new)
  
  #Boxplot
  #boxplot between Alcohol and quality rating
   ggplot(aes(x = rating, y= alcohol), data = Wine_List) +
    geom_jitter( alpha = 1/4)  +
    geom_boxplot( alpha = .5,color = 'slate gray')+
    stat_summary(fun = "mean",geom = "point",color = "red", shape = 8,size = 4)+
    labs(y= "Alcohol", x = "Quality Rating")+ggtitle("boxplot between alcohol and quality rating")
  
  
  #boxplot between density and quality rating
  ggplot(aes(x = rating, y = density), data = Wine_List)+
    geom_jitter( alpha = 1/4)+
    geom_boxplot(alpha = .5,color = 'slate gray')+
    geom_point(stat = 'summary', fun = mean, color = 'red', pch = 4)+
    labs(y= "Density", x = "Quality Rating")+ggtitle("boxplot between density and quality rating")
  
  
  #boxplot between residual sugar and quality rating
  ggplot(aes(x = rating, y = residual.sugar), data = Wine_List)+
    geom_jitter( alpha = 1/4)+
      geom_boxplot(alpha = .5,color = 'slate gray')+
      geom_point(stat = 'summary', fun = mean, pch = 9, color = 'red')+
     labs(y= "Residual Sugar", x = "Quality Rating")+ggtitle("boxplot between residual sugar and quality rating")
  
  #boxplot between pH and quality rating
  ggplot(aes(x = rating, y = pH), data = Wine_List)+
    geom_jitter( alpha = 1/4)+
    geom_boxplot(alpha = .5,color = 'slate gray')+
    geom_point(stat = 'summary', fun = mean, pch = 9, color = 'red') +
    labs(y= "pH", x = "Quality Rating")+ggtitle("boxplot between pH and quality rating")
  
  #boxplot between chlorides and quality rating
    ggplot(aes(x = rating,y = chlorides), data = Wine_List)+
      geom_jitter( alpha = 1/4)+
    geom_boxplot(alpha = .5,color = 'slate gray') +
    geom_point(stat = 'summary', fun = mean, pch = 9, color = 'red') +
    labs(y= "Chlorides", x = "Quality Rating")+ggtitle("boxplot between chlorides and quality rating")
  
    #boxplot between Total Sulfur dioxide and quality rating
    ggplot(aes(x = rating,y = total.sulfur.dioxide), data = Wine_List)+
      geom_jitter( alpha = 1/4)+
      geom_boxplot(alpha = .5,color = 'slate gray') +
        geom_point(stat = 'summary', fun = mean, pch = 9, color = 'red') +
       labs(y= "Total Sulphar Dioxide", x = "Quality Rating")+ggtitle("boxplot between total sulfur dioxide and quality rating")
    
    #boxplot between Total fixed acidity and quality rating
    ggplot(aes(x = rating,y = fixed.acidity), data = Wine_List)+
      geom_jitter( alpha = 1/4)+
      geom_boxplot(alpha = .5,color = 'slate gray') +
      geom_point(stat = 'summary', fun = mean, pch = 9, color = 'red') +
      labs(y= "Fixed Acidity", x = "Quality Rating")+ggtitle("boxplot between fixed acidity and quality rating")
    
    
    
    
    # Scatter Plot and liniear regression
    # Scatter plot and liniear regression of density and alcohol
    ggplot(aes(x = density, y = alcohol), data = Wine_List)+
      geom_point(alpha = 1/3 , color = I('slate gray3'))+
      geom_smooth(method = 'lm' , color= 'red')
    
    # Scatter plot and liniear regression of residual sugar and density 
    ggplot(aes(x = residual.sugar, y = density), data = Wine_List)+
      geom_point(color = 'slate gray3')+
      geom_smooth(method = 'lm' , color= 'red')
    
    # Scatter plot of residual sugar and alcohol
    ggplot(aes(x = residual.sugar, y = alcohol), data = Wine_List)+
      geom_point(alpha = 1/2, color = 'Slate gray 3')+
      geom_line(stat = "summary", fun = mean, color='red')
    
    # Scatter plot and liniear regression of total sulfurdioxide and density 
        ggplot(aes(x = density , y = total.sulfur.dioxide), data = Wine_List)+
          geom_point(alpha = 1/2, color = 'slate gray3')+
          geom_smooth(method = 'lm' , color='red')

        
        
      
   # MultiVariate Analysis
       #MultiVariate Analysis between Alcohol, Quality and Density
        ggplot(aes(x = alcohol, 
                   y = density  , color = factor(quality)), 
               data = Wine_List) + 
          geom_point(alpha = 0.8, size = 1) +
          geom_smooth(method = "lm", se = FALSE,size=1)  +
          scale_color_brewer(type='seq',
                             guide=guide_legend(title='Quality'))
        
        