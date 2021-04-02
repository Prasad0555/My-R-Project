
# setting the working directory

setwd("C:\\Users\\MyPC\\Desktop\\Project 3")

set.seed(123)


# Load the package
install.packages("plyr")
library(plyr)

library(ggplot2)

library(caret)

library(MASS)

install.packages("party")
library(party)

install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("ROCR")
library(ROCR)

library(rpart)

install.packages("rattle")
library(rattle)

install.packages("rpart.plot")
library(rpart.plot)


#load the dataset after setting working directory
df_churn<- read.csv("Churn.csv")

head(df_churn)

view(df_churn)

str(df_churn)

#checking missing values
colSums(is.na(df_churn))
# There is no missing values

length(df_churn$customerID) # count of customers

count(df_churn$gender)


# We are going to see there is any impact on gender and churn
plot(table(df_churn$Churn,df_churn$gender),color=c("red","green"),xlab="Churned Yes or No",
     ylab="Gender",main="Gender vs Churn")
# There is no impact of gender on the Churn


table(df_churn$SeniorCitizen==TRUE)
# There are 1566 of sinior citizen

# The seniorCitizen plot is not working because there other values apart from 0 and 1 also.
view(df_churn)


table(df_churn$Partner) # almost 50% who have a partners

plot(table(df_churn$Churn,df_churn$Partner),color=c("red","green"),ylab="churned Y N",
     xlab="Partner")
# In this plot:
# The people who have a partner their churn is lower, people who dont have a partner their
# churn is higher.


table(df_churn$Dependents)
df_churn$Dependents[1:10] # This will show you 1st 10 elements
plot(table(df_churn$Churn,df_churn$Dependents),color=c("red","green"),
     ylab= "Churned Yes or No",xlab= "Dependents")
# Dependents does not seem to have any impact on the Churn because no. of Dependents also 
# doesnt seem to have any impact on the Churn.


summary(df_churn$tenure)
str(df_churn$tenure) # tenure is num. so we have to plot hist.
hist(df_churn$tenure,col = "orange",xlab = "Tenure of the connection",
     main = "Dist of the Tenure")
# Mejority of connections seem to be very recent times. Most of the connections are in the
# range of 0 to 20 months.


table(df_churn$CallService) # 10933 is opted for call services
plot(table(df_churn$Churn,df_churn$CallService),xlab= "churned Yes or No", 
     ylab= "call service Yes or No",col=c("orange","green"))
# It is not very significant relationship


table(df_churn$MultipleConnections) # there aree 1425 No phone service
table(df_churn$Churn,df_churn$MultipleConnections)
plot(table(df_churn$MultipleConnections,df_churn$Churn),xlab="Type of connection",
     ylab="Churned",col=c("red","green"))
# In this plot showing error because there are other value apart from yes or No.


table(df_churn$OnlineSecurity) # its a significant variable
plot(table(df_churn$OnlineSecurity,df_churn$Churn),xlab= "Availability of online security",
     ylab= "churned",col=c("blue","yellow"))
# whenever who  people opted for online security their churned is least.
# we are assigning the NO internet service with No.


table(df_churn$OnlineBackup) # No internet services 3227
plot(table(df_churn$OnlineBackup,df_churn$Churn),col=c("green","orange"))
# there is relationship a significant relationship between onlineBackup and Churned.
# wherever people opted for onlineBackup their churned is least


table(df_churn$DeviceProtectionService) # no internet services 3241
plot(table(df_churn$DeviceProtectionService,df_churn$Churn),col=c("green","orange"))
# showing significant relationship, so all the above 3-4 plot showing same plot.


table(df_churn$TechnicalHelp) # no int. service 3214
plot(table(df_churn$TechnicalHelp,df_churn$Churn),col=c("green","orange"))
# significant relationship


table(df_churn$OnlineTV) # N.I.S 3245
plot(table(df_churn$OnlineTV,df_churn$Churn),col=c("green","orange"))
# does not have much impact on the churn


table(df_churn$OnlineMovies) # N.I.S 3222
plot(table(df_churn$OnlineMovies,df_churn$Churn),col= c("green","orange"))
# showing not much impact on churn


table(df_churn$Agreement)
plot(table(df_churn$Agreement,df_churn$Churn),col= c("green","red"))
# showing very significant impact
# whenever the agreement is two year the churn is least.


table(df_churn$BillingMethod)
df_churn$BillingMethod[1:15]
plot(table(df_churn$BillingMethod,df_churn$Churn),col=c("green","red"))


table(df_churn$PaymentMethod)
plot(table(df_churn$PaymentMethod,df_churn$Churn),xlab= "Payment modes",
     ylab= "churned Yes or No",col=c("black","red"))
# showing important variable


str(df_churn$MonthlyServiceCharges) # numeric variable so we have to make hist
hist(df_churn$MonthlyServiceCharges,col="green")
# In this plot there is nothing to tell you about the churn, it is just telling you the 
# distribution of the monthly charges data.


summary(df_churn$TotalAmount) # numeric
hist(df_churn$TotalAmount,col = "orange",xlab = "Bill amount",ylim = c(0,5000))


# Based on the result of the each column change "No internet servcie" to "NO" for six columns,
# they are: OnlineSecurity","OnlineBackup","DeviceProtectionService","TechnicalHelp",    
#           "OnlineTV","OnlineMovies"  


cols_name<- c(10:16)
for (i in 1:ncol(df_churn[,cols_name])) {
        df_churn[,cols_name][,i] <- as.factor(mapvalues
                 (df_churn[,cols_name][,i],from= c("No internet service"),
                         to=c("No")))}
# In this column no. 10:16 we are doing for loop over here in each column from column no.
# 10 to 15, we are accessing each row and then we are changing "No internet service" to "No"
# mapvalues function doing is to change all the values in the rows wherever it is "NO internet
# service" to "No". (ncol) is number of columns.
# as.factor which means convert whatever is there content within the braket convert that 
# into the factor.

df_churn[,cols_name]
str(df_churn)


table(df_churn$MultipleConnections)

df_churn$MultipleConnections<- as.factor(mapvalues(df_churn$MultipleConnections,
                                                   from = c("No phone service"),
                                                   to=c("No")))
# multipleconnection column had some rows of "No phone service" so just convert into "No".
# we are converting that into a factor.

table(df_churn$SeniorCitizen)

# Remove the columns we do not need or are not significant for the analysis:
df_churn$SeniorCitizen<- NULL
df_churn$customerID<- NULL


# we also saw that the gender column was not have any impact, we are dropping this.
df_churn$gender<- NULL
names(df_churn)


# Model Development
# Split the data into training and testing sets
library(caTools)

spl=sample.split(df_churn$Churn,SplitRatio=.70)

training<- df_churn[spl==TRUE,]

testing<- df_churn[spl==FALSE,]



# Full grown tree model concept
full_model_tree<- rpart(Churn~ .,training,method = "class",minsplit=0,cp=0)
# We are doing fully grown tree and we are using: minsplit=0 and cp=0 

plot(full_model_tree)
# this tree performance of this dataset is very good, but this will not able to get a equal
# good performance on a new dataset with this kind of a structure because this is overfitting.


# Prediction using the fully grown tree
Full_model_pred<- predict(full_model_tree,newdata = testing,type = "class")
# we are doing the prediction on testing dataset

Full_model_pred
# To look at the actual prediction by the fully grown tree we just created.
# In prediction: "No" is not the churn and "yes" is churn. This is the prediction.


# Accuracy and confusion Matrix
conf_matrix_full_model<- table(testing$Churn,Full_model_pred)
# we are just comparing the actual output with the Prediction.
# We want to figure out how many misclassifications are there and how efficient the model
# that we have created.

conf_matrix_full_model
# so there were 1797 which where No and we also said it is No and there were 1340 yeses and 
# We also said Yes. We are gone wrong in 342+221 cases.


# Efficiency of the model/print out the accuracy 
sum(diag(conf_matrix_full_model)) / sum(conf_matrix_full_model)
# diag is diagonal, when we write diag with  this confusion matrix we are simply taking out
# 1797 and 1340 this two values.
sum(diag(conf_matrix_full_model)) # 1797+1340


# If you dont understand the code then break it and run it each line.



# cp implementation
# we will apply the cross validation technique and we will create a new model using 10 folds
# with different cp values
library(caret)
library(e1071)
numfolds<- trainControl(method = "cv",number = 10)
# Smaller cp values will lead to bigger trees/complexity increases
# higher cp values will lead to smaller trees

cpGrid<- expand.grid(.cp=seq(0.01,0.05,0.01)) # complexity parameter



# checking the cross validation accuracy for cp parameters
train(Churn~ .,data = training,method= "rpart",trControl=numfolds,tuneGrid= cpGrid)
# The algorithm is creating a model and calculating a performance, calcuting the accuracy 
# and the kappa values both.


# pruning(reducing the size of tree) the full_model_tree(fully grown model)
pruned_tree<- prune(full_model_tree,cp=0.01)
# We are using the cp value to prune the fully grown tree. We are reducing the tree size 
# by using the cp value 0.01
prp(pruned_tree)
# pruning means cutting your tree.


#Prediction using the fully grown tree
pruned_model_pred<- predict(pruned_tree,newdata = testing,type = "class")
pruned_model_pred


#accuracy and confusion matrix
conf_matrix_pruned_model<- table(testing$Churn,pruned_model_pred) # we are calculating the confusion matrix
                                                                  # on the prediction
conf_matrix_pruned_model


# print out the accuracy
sum(diag(conf_matrix_pruned_model)) / sum(conf_matrix_pruned_model)
# There is a decline in the accuracy, the accuracy has gone down to 76%


library(party)
# https://www.rdocumentation.org/packages/partykit/versions/1.2-9/topics/ctree_control
#   This link will showing you details.


# The third way which we can create the decision tree with (ctree2)
model<- train(Churn~ .,data = training,method="ctree2",
              trControl= trainControl("cv",number = 10),
              tuneGrid= expand.grid(maxdepth= 10,mincriterion = 0.95))
# here the method is called ctree2 this is another technique this is called classification
# tree. We are continue to use our 10 fold cross validation and we are use this new 
# parameter which is (maxdepth) now maxdepth controls 'the number of levels at which the 
# split is happening'. And (mincriterion) is basically there is some testing going on and
# whenever the value of the test is less than 0.95 then only this split takes place.

plot(model$finalModel,cex=0.05) #cex is controls the size


# prediction using the fully grown tree
model_pred<- predict(model,newdata = testing,type = "raw")
model_pred
# type takes two thing "class" and "raw" you can also put the type of "prob" this will given
# you the probabilities. but "raw and class" will give you the class.

model_pred<- predict(model,newdata = testing,type = "prob")
model_pred


# accuracy and confusion matrix
conf_matrix_model <- table(testing$Churn,model_pred)
conf_matrix_model


# print out the accuracy
sum(diag(conf_matrix_model)) / sum(conf_matrix_model)


# Random Forest
install.packages("randomForest")
library(randomForest)

churn_rforest<- randomForest(Churn~ .,data = training,nodesize=5,ntree=200)
# we will have to convert into factor variable.
training$Churn<- as.factor(training$Churn)

churn_rforest<- randomForest(Churn~ .,data = training,nodesize=5,ntree=200)
# we are using the randomforest command to predict the churn using all the variables
# data is training, nodesize is same thing which minbucket was there and ntree is lets 
# you control of tree
predict_forest<- predict(churn_rforest,newdata = testing)


# accuracy and confusion matrix
conf_matrix_rf<- table(testing$Churn,predict_forest)
conf_matrix_rf


# print out the accuracy
sum(diag(conf_matrix_rf)) / sum(conf_matrix_rf)
# the accuracy is jumped 86%


# In dicision tree the accuracy the final model was 76 then after pruning it had gone down 77
# With the random forest the accuracy has gone upto 86%