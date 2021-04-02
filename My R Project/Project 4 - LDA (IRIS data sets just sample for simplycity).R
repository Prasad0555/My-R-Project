
set.seed(555)

#linear discriminant analysis will help figure out that combination of the 4 variables
# sepal length, sepal width, petal length, petal width which will  gives us the best separation
# between different species.

View(iris)
 # In this first four columns is numeric contineous nature and species is the dependent variable
 # which has three categories or levels and each level has same observations, it is 50 for 
 # three levels. so each has same portion. 

str(iris)
# load the psch package to create the pairs panel plot

install.packages("psych")
library(psych)
  
# we are doing the pairs plot to show the interaction of different variable in the iris data
# set and the background color, and we want these different colors to be given to the three
# different species and the pch is the style of the dot which is 21 over here.

pairs.panels(iris,gap=.5,bg=c("orange","green","purple")[iris$Species],pch = 21)
  
# If you remove the outliers in the petal lenght and petal width then you can see the data 
# which is normally distributed.


# split the data set
dim(iris)

library(caTools)
# now this package is use to splitting the datasets into training and  testing. With ratio
# .8 and .2, in this we are just using the .8 observation to learn the parameters. .2 having 
# the actual values but it is not being use in order to learn the parameters.


spl=sample.split(iris$Species,SplitRatio = .8)

spl # In it .8 is the True and .20 is the False

table(spl)


# Now are going to create training dataset and we are going to include all Ture in this data.
# train is to be used for creating the model
train <- iris[spl==T,]
dim(train)

# In test we are including all False in test dataset.
# to perform the prediction we are going to use the test data.
test <- iris[spl==F,] 
dim(test)

# It simply doing the random sampling

head(iris)

table(iris$Species)


install.packages("MASS")
library(MASS)
# In this package has the function lda

names(train)


# create the LDA model and print model details 

lda_model <- lda(Species~.,data = train)
# using the lda function to predict the species by considering all the 4 variables from the 
# dataset train

lda_model

# In the output:
# 1. we have Prior probabilities of groups, all the three classes are present in equal numbers
#    we have the proportion of each obs. equal to 33.33.
# 2. we have Group means so, it is centroid of setosa, centroid of versicolor, and centroid
#    of virginica.
# 3. Coefficients of linear discriminants: LD1 gives us coefficents of each of 4 variables
#    it means LD1= 1.19*Sepal.Length+1.34*Sepal.Width-2.46*Petal.Length-2.65*Petal.Width
#    this is the first Linear discriminant function.

lda_model$prior # this is the prior probability

lda_model$counts

lda_model$means

lda_model$scaling # gives the coefficients fo LDA1 and LDA2


# prediction on the testing data

pred_training <- predict(lda_model,newdata = test)

pred_training # this is my predicted output of the testing data sets which has 30 obs.

pred_training$class

table(test$Species)

table(test$Species,pred_training$class)
# there is showing misclassification, there is error because of overlab.


# histogram using the values given Y LDA column 1 values
ldahist(data = pred_training$x[,1],g=train$Species)
# g: controls diff levels of the dependent variable

# histogram using the values given Y LDA column 2 values
ldahist(data = pred_training$x[,2],g=train$Species)


# prediction on the test data and accuracy
pred_on_test<- predict(lda_model,newdata = test)$class

pred_on_test
tab1<- table(test$Species,pred_on_test)

tab1

sum(diag(tab1))/sum(tab1)

