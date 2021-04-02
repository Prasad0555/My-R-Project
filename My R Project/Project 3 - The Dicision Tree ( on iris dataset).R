
# classification tree using iris 

# Regression tree using Auto from ISLR package

# classification tree

# use the dataset iris

set.seed(123)
df <- iris

str(df)

summary(df)

#check the head of the df
tail(df,15)

#check the dimension of the data
dim(df)

# split the data using the caTools
library(caTools)

spl <- sample.split(df$Species,SplitRatio = .8)
# we have spplitted the data into 80 and 20 ratio

table(df)

df_train <- df[spl==T,] # we are putting the 80 samples in df_train consider as True 
df_train

df_test <- df[spl==F,] # 20 samples in df_test consider as False
df_test

dim(df_train)
dim(df_test)

table(df_train$Species)
table(df_test$Species)

dim(df)


# plot the species using ggplot2
install.packages("ggplot2")
library(ggplot2)

names(iris)

species_plot <- ggplot(data = iris,aes(x=Petal.Length,y=Petal.Width,color=Species))

species_plot<- ggplot(data = iris,aes(y=Petal.Length, x=Petal.Width,color=Species))
species_plot+geom_point()

species_plot+ geom_point()+ geom_hline(yintercept = 2, linetype="dashed",color="red")

species_plot +geom_point()+ geom_hline(yintercept = 2, linetype="dashed",color="red")+
  geom_vline(xintercept = 0.9, linetype="dashed",color="red") # hline means horizontal line and vline 
                                                              # means vertical line

species_plot +geom_point()+ geom_hline(yintercept = 2, linetype="dashed", color="red")+
  geom_vline(xintercept = 0.9, linetype="dashed",color="red")+
  geom_hline(yintercept = 4.8, linetype="dashed",color="black")+
  geom_vline(xintercept = 1.75, linetype="dashed",color="purple")

# (geom_hline) is for the horizontal line 
# (geom_vline) is for the vertical line
# (yintercept =2) is drawing the line, since y aes.


# In this plot: class 1 has no impurity so there is 
# 1. Entropy is 0 of class 1, because there is no impurity.
# 2. class 2 has 1 misclassification.
# 3. class 3 has no impurity.


# what is limit i can split on the dataset ? 
#  Ans: As many number of obs. as we have that many subset we can have 


table(df_train$Species) # well balanced bifurcation

table(df_test$Species)


# Fitting the decision tree model using the package "rpart"
install.packages("rpart")
library(rpart)

names(df_train)


#create the tree with minbucket (parameter) value as 10
dec_tree_model<- rpart(Species~ .,data = df_train,method = "class", minbucket=10)

# we are creating the decision tree model, we are wanting to predict the species that is the
# class of species and we are using all the input variables all 4 then we are using the
# training data, method is class because we want to do a classification.

# minbucket means minimum number of obs. for a split to take place is 10
# Manually putting the value of minbucket you can control the size of tree, If you put very
# small value of the minbucket you are going to have a tree which is very very large. on the
# other end if you have a minbucket value which is high you are going to have a tree which 
# is very very simple.


dec_tree_model

?rpart

dev.off()

# rpart plot pakacge will help to plotting the decision tree.
install.packages("rpart.plot")
library(rpart.plot)

rpart.plot(dec_tree_model,type = 3) # type 3 is better to consider because sir explained.

#In this plot:
# 1. Petal Length is first attribute on which the split is made. If petal length is less than
#    2.5 then all the obs. belonging to setosa category.
# 2. If petal length is greater than 2.5 we ask another question is the petal length less 
#    than 4.9, our obs. belonging to vesicolor and there is also .03 obs. is coming from
#    verginica so there is misclassification.


# alternate way of creating the plot
prp(dec_tree_model)


# Prediction using the test data
dec_tree_model<- predict(dec_tree_model,newdata = df_test,type = "class")
dec_tree_model
# df_test has just 30 obs. so prediction came with 30 obs.


# confusion  matrix
table(df_test$Species,dec_tree_model)
# There are 2 misclassification


#check the accuracy
acc<- (10+8+8)/(10+8+8+2+2)
acc


# Now we dont want to manually put the value of minbucket so what we do is called the cross
# validation.

# Advanced cp implementation

# cp parameter is the complexity parameter
library(caret)

install.packages("e1071")
library(e1071) # e1071 is algorithms

# cp process we are written in notebook.
numfold<- trainControl(method = "cv",number = 10)
# We are using 10 folds over here. we are going to create 10 folds of the dataset

cpGrid<- expand.grid(.cp=seq(0.01,0.5,0.01))
# The complexity parameter i am taking all the values starting 0.01 to 0.5 incrementing by
# a value of 0.01. here we are using different cp values now If the value of cp is 0.01
# it will create a very complex tree if the value of cp is 0.5 it will create a less 
# complecated tree.

# So we are going to create 1 tree for each cp value for each fold.


#checking the cross validation accuracy for cp parameters
train(Species~ .,data = df_train,method="rpart",trControl=numfold,tuneGrid=cpGrid)
# We are using the train function, species the dependent variable the data is df_train, 
# method (rpart), number of folds and Grid value of cp.

?expand.grid


# Final model
Final_tree<- rpart(Species~ .,data = df_train,method = "class", cp=0.46)
# best cp value is 0.45 but we are accordingly take up 0.43.
# we are creating the final model by using the package (rpart) to predict the species.
plotcp(Final_tree)

# Prediction using the final model
Final_pred<- predict(Final_tree,newdata = df_test,type = "class")
Final_pred
# we are predicted the Species of the flower of the df_test dataset

dim(df_test)
length(Final_pred)
table(df_test$Species,Final_pred)

# We need to use Final_tree model on the df_test dataset to predict the class this will 
# give you the Final model.

# If you want to check which 2 obs. have been misclassified we didn't do anything to them 
# because the model that we created we could not improve it even by choosing by the optimum
# cp value


# Final prediction also there is a error (misclassification) we have not been able to improve
# the model even after doing the cross validation. But you will see the impact of the 
# cross validation on some other dataset. so the idea was just to show you how you can do
# cross validation. some other project you will find useful.

# Random Forest concept of the bagging/boostrapped
install.packages("randomForest")

library(randomForest)
