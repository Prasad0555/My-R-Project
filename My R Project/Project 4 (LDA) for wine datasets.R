
#set the working directory 

setwd("C:\\Users\\MyPC\\Desktop\\Project 4")

set.seed(123)

install.packages("GGally")
library(GGally)

install.packages("tidyverse")
library(tidyverse)

install.packages("ggpubr")
library(ggpubr)

install.packages("repr")
library(repr)

install.packages("scales")
library(scales)

install.packages("memisc")
library(memisc)

library(MASS)

wine <- read.csv("wine.data.txt")

View(wine)
# In wine dataset is not showing columns names


colnames(wine) <- c("Type","Alcohol","Malic","Ash","Alcalinity","Magnesium","Phenols",
                    "Flavanoids","Nonflavanoids","Proanthocyanins","Color","Hue",
                    "Dilution","Proline")
# we are assigning the heading of each of the 13 columns in the wine datasets


head(wine)
# except the first one all are numeric contineous values

table(wine$Type)

# we are checking the prior probability of the wine
table(wine$Type)/nrow(wine)


#check the structure
str(wine)
# Type is also numeric over here, it should have been factor.


#check for the missing values
colSums(is.na(wine))
# there are no missing values in the dataset


library(ggplot2)

options(repr.plot.width = 4,repr.plot.height = 3)  # here we are setting the plot size
# this is simply giving the size of the canvas on which we want to drow the plot


#plotting the histogram
ggplot(aes(x=Type),data = wine)+geom_bar(fill="#00AFBB",colour= "#FC4E07")
# class 2 observation are higher than the other 2 class 
dev.off()


#creating the function to plot histogram, boxplot and scatter plot together to analysis variables

wine_attribute <- function(attribute, varName = '', bins = 30) {
  ## Building the histogram:
  histogram <- ggplot(data = wine) +
    geom_histogram(aes(x=attribute), bins = bins,
                   fill = 'steelblue', colour='darkgrey', alpha= 0.8) +
    labs(x = varName)
  ## Histogram scaling y_log10:
  histYlog <- histogram + scale_y_log10() +
    labs(y = 'log10(count)', x= varName)
  
  ## Histogram scaling x_log10:
  histXlog <- histogram + scale_x_log10() + 
    labs(x = paste('log10(', varName,')'))
  
  ## Building the boxplot highlighting the outliers:
  outliers <- ggplot(wine, aes(x = 1, y = attribute)) + 
    geom_jitter(alpha = 0.1 ) +
    geom_boxplot(alpha = 0.2, color = 'red') + 
    labs(x ='distance from mean', y= varName)
  
  ## Arranging all the plots:
  histPlots <- ggarrange(histogram, histXlog, histYlog, ncol=1, nrow=3)
  ggarrange(outliers, histPlots,ncol=2, widths = c(1,1.5))
}

# we are using the ggplot over here. I told you a ggplot command has got 3 components, in any code 
# of any  plot with ggplot you will have basically 3 components, first you will have a data, second
# you will have the asthetics, asthetics will control what you want to plot on x aes and y aes and
# then it will have the geometric shape, geometric shape will decide whether you want a bar or box
# or whether you want a dot.


# We can notice from the boxplot the data points are highly distributed and From the histogram,
# we can see the distribution of the data points for each class in the "Alcohol" attribute.
  

#malic acid
## plot size to 7.5 x 3 
options(repr.plot.width=7.5, repr.plot.height=3)

wine_attribute(wine$Malic, varName = 'Malic (mg/L)')


# Alcohol

# This attributes refers to the percent alcohol content of the wine (% of values).

## Plot size to 7.5 * 3
options(repr.plot.width = 7.5, repr.plot.height = 3)
 ## How the "alcohol" attribute is distributed?
wine_attribute(wine$Alcohol, varName = 'Alcohol (% of vol)')


#Ash

##plot size to 7.5 * 3
options(repr.plot.width = 7.5, repr.plot.height = 3)
## How the "Ash" attribute is distributed?
wine_attribute(wine$Ash, varName = 'Ash')

 
#Alcalinity
## plot size to 7.5 x 3 
options(repr.plot.width=7.5, repr.plot.height=3)

wine_attribute(wine$Alcalinity, varName = 'Alcalinity')



#Flavanoids vs Type
## plot sizd to 6 * 4
options(repr.plot.width=6, repr.plot.height=4)  #Setting the plot size
ggplot(aes(x= factor(Type), y= Nonflavanoids), data = wine) +
  geom_jitter( alpha = .2) +     # its controls the intensity of dot
  geom_boxplot( alpha = .5,color = 'blue')+
  stat_summary(fun.y = "mean", geom = "point", color = "darkblue", 
               shape = 4, size = 4) +
  labs(x= 'Type',
       y= 'Nonflavanoids',
       title= 'Nonflavanoids Vs. Type')


# In this: nonflavanoids in group no. 1 or wine label 1 is very close to point 3. In wine class 2
#          the nonflavanoids is something point 3.7. And  3rd class of wines where it is slitly 
#          below .5
#          So Nonflavanoids vs type for each category you have the boxplot showing you the content
#          of the nonflavanoids in this three diff. type of wines.


# IN boxplot cross sign is representing 'mean' and the thick blue line representing 'median'
# In first class the cross sign is super impose on boxplot, because of the "Normal dis." And 
# class 2 the skewness is "left".



# If you look at any code you will find 3 things common what is the data, what is that you want to 
# have on the x aes and y aes and geometric shape. here 'geom_jitter' alpha .2 controls the
# the intensity of dot which has on the boxplot.



#Bivariate analysis
## plot size to 6 x 4 
options(repr.plot.width=6, repr.plot.height=4)  

ggcorr(wine[,1:14], geom = "blank", label = TRUE, 
       hjust = 0.9, layout.exp = 2) +
  geom_point(size = 8, aes(color = coefficient > 0, 
                           alpha = abs(coefficient) > 0.35)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE)

# This is the alternate of the pairs plot. 'abs' mean absolute.



# check for normality of dis.

hist(wine$Alcalinity,col = "maroon")
boxplot(wine$Alcalinity,col = "maroon")

hist(wine$Malic,col="orange") 
hist(log(wine$Malic),col = "orange") 
# This is the log transform plot. This is looking more normal as compare to above original plot.
# So whenever you have variable which is looking skewed you can try this trick of doing the 
# log transformation plot.

boxplot(wine$Malic,col="orange")


hist(wine$Ash,col = "grey")
boxplot(wine$Ash,col="grey")


hist(wine$Alcalinity) #very normal


hist(wine$Magnesium) #not very normal
hist(wine$Phenols) # looking bimodel
hist(wine$Flavanoids) # lookig bimodel


#pairs plot
library(psych)

pairs.panels(wine[2:4],gap=0,bg=c("red","green","blue")[wine$Type],pch=21)
# In this we are just using pairs dot panel which is of the psych package, I am using the variable
# for column no. 2 to 4. We are not giving any gab in each of this boxes.

pairs.panels(wine[5:7],gap=0,bg=c("red","green","blue")[wine$Type],pch=21)
pairs.panels(wine[8:10],gap=0,bg=c("red","green","blue")[wine$Type],pch=21)
pairs.panels(wine[11:14],gap=0,bg=c("red","green","blue")[wine$Type],pch=21)


#split the dataset

set.seed(123)

wine$Type <- as.factor(wine$Type)
# here we are converting the dependent variable from int. to factor

library(caTools)
spl = sample.split(wine$Type,SplitRatio = 0.8)

train <- wine[spl==TRUE,]
test <- wine[spl==FALSE,]


# Applying the lda model
lda_mod <- lda(Type~ .,data = train)
# Here we just using the lda function of the MASS package to predict the type of the  wine
# using all the 14 variables in the dataset train

lda_mod
 
plot(lda_mod,col = "Red",pch=2) # IN this you can clearly see the classification

attributes(lda_mod)

lda_mod$prior
# percentage of type 1, type 2, type 3 in the training data

lda_mod$counts
# In this class 1 there is 46 obs., class 2 has 57 obs. and class 3 has 38 observations

lda_mod$means

lda_mod$scaling
# This will give you coefficients of LDA

lda_mod$lev
# This will showing you levels

pred <- predict(lda_mod, newdata = train)

pred # $x means linear discriminant score

table(train$Type,pred$class)
# there is no any misclassification


# Histogram using the values given y LDA column 1 values
ldahist(data = pred$x[,1],g=train$Type)

ldahist(data = pred$x[,2],g=train$Type)


# BIo-Plots
install.packages("usethis")
library(usethis)

install.packages("devtools")
library(devtools)

install_github('fawda123/ggord')
library(ggord)

ggord(lda_mod,train$Type,ylim=c(-7,5))


# partition plot
install.packages("klaR")
library(klaR)


# Prediction on the test datasets
pred_test <- predict(lda_mod,newdata = test)

pred_test

pred_test$class

tab <- table("Actual"=test$Type,"Predicted"=pred_test$class)

tab  # no difference in actual and predicted it means that out predicted model is 100% accurate

accuracy=sum(diag(tab))/sum(tab)

accuracy # accuracy is 100% or 1


# https://rpubs.com./ifn1411LDA
   #this project taken from this site.

