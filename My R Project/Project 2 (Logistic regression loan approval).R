
#set working directory
setwd("C:\\Users\\MyPC\\Desktop\\Project 2")
set.seed(123) #if we are set the seed then we are not going to get a different output so it is good that 
             #we all are on the same page we all get the same output we can put any value but sir put 
             # 123 so we set 123, if sir dont set 123 then we are going to get a different data set
             #  or output if sir not use this function, whatever sir's R studio get output, it is
              # not possible we getting  the same output.

rm(list = ls())
gc() #gc is garbage collection

#read in the training dataset and save it to object named cr_train
cr_train<- read.csv("Credit_Risk_Train_data.csv",na.strings = "") # we want the blank spaces to be 
  # read as na's,it means that wherever it blanks please that as na (na value). missing value means
  # not the outliers it is a 'blank'.

#view of the dataset
View(cr_train)

#checking the structure of dataset
str(cr_train)

#check the summary of the dataset
summary(cr_train)


#first 10 rows of the dataset
head(cr_train,10)
head(cr_train) #if you dont specified rows then it will give you first 6 rows

#last 10 rows of the dataset
tail(cr_train,10)
tail(cr_train)


#read in the validation dataset and save it to object named cr_valid
cr_valid <-read.csv("Credit_Risk_validate_data.csv",na.strings = "")

#read in the test dataset and save it to objeft named cr_test
cr_test <- read.csv("Credit_Risk_Test_data.csv",na.strings = "")
summary(cr_test)


#carry out the imputation using the correct central value
#visualizing the missing value
install.packages("Amelia")
library(Amelia)

missmap(cr_train,col = c("blue","orange"))
missmap(cr_test,col = c("black","orange"))
dev.off()


table(colSums(is.na.data.frame(cr_train))>0)

b= barplot(colSums(is.na.data.frame(cr_train)),cex.names = .7,col = "orange",las=2)
text(b, 0,colSums(is.na(cr_train)),cex = .6,pos = 3,col = "red")
  #this barplot showing you number of values and missing value very clear. (cex= .7) this arguement 
  #control the size of names or labels. (las=2) control vertical display of the
  # variable names. (colsums) it is going to sum up the missing values in the data frame cr_train
  # each column.



#in case of categorical variable we shall be using the which central tendency measure

#for gender
dev.off()
table(cr_train$Gender) #male is the model class
  #table is given you frequency of the categorical variables

summary(cr_train$Gender)
which(is.na(cr_train$Gender)) #it is tellig me the row numbers where the values are missing

cr_train[which(is.na(cr_train$Gender)),"Gender"]<-"Male" #in the training dataset we are going to the 
    #column gender wherever it is na we are finding out the row numbers which is.na and then the column
    #gender we are replacing value with male.

barplot(table(cr_train$Gender),col = c("pink","blue"))


#for marital status
table(cr_train$Married)
table(is.na(cr_train$Married))
cr_train[which(is.na(cr_train$Married)),"Married"]<-"Yes"
barplot(table(cr_train$Married),col = c("red","green"),ylim=c(0,600))

?barplot  #this fuction showing all details about the barplot please read it.


#for dependents
table(cr_train$Dependents)
summary(cr_train$Dependents)
table(is.na(cr_train$Dependents))
cr_train[which(is.na(cr_train$Dependents)),"Dependents"]<-"0" #earlier 0 is not character variable now
                                                            #it is a becoming character variable "0"
                                                            #0 was the model class.
                                                            
barplot(table(cr_train$Dependents),col = c("green","yellow","orange","red"))
dev.off()


#for education
dev.off()
table(cr_train$Education) #no missing value
table(is.na(cr_train$Education))
missmap(cr_train)
barplot(table(cr_train$Educationl),col = c("orange","brown"))


#for selfemployed
table(cr_train$Self_Employed)
table(is.na(cr_train$Self_Employed))
cr_train[which(is.na(cr_train$Self_Employed)),"Self_Employed"]<-"No"
barplot(table(cr_train$Self_Employed),col = c("yellow","green"))


#for credit history
summary(cr_train$Credit_History)
table(cr_train$Credit_History)
table(is.na(cr_train$Credit_History))
cr_train[which(is.na(cr_train$Credit_History)),"Credit_History"]<-"1"
is.na(cr_train$Credit_History)
barplot(table(cr_train$Credit_History), col = c("orange","blue"))
dev.off()
missmap(cr_train)

#for numeric variable how do we check the distribution as data? numerically and graphically
install.packages("moments")
library(moments)
skewness(cr_train$ApplicantIncome) #showing positive

hist(cr_train$ApplicantIncome, main = "Histogram of loan applicant income",xlab = "Applicant income",
     ylab = "Frequency",col = "orange")

#If skewness is negative, the data are negatively skewed or skewed left, i.e left tail is longer
#If skewness is positive, the data are positively  skewed or skewed right, i.e right tail is longer


hist(cr_train$LoanAmount,col = "green") #skewness present in data
cr_train[which(is.na(cr_train$LoanAmount)),"LoanAmount"]<-median(cr_train$LoanAmount,na.rm = T)
  #we are replacing the missing value in case of loan amount


#for loan amount term
hist(cr_train$Loan_Amount_Term)
cr_train[which(is.na(cr_train$Loan_Amount_Term)),"Loan_Amount_Term"]<-
  median(cr_train$Loan_Amount_Term, na.rm = T) #na missing value and rm is remove
dev.off()
missmap(cr_train)


#for catigorical variable in test data set
View(cr_test)
summary(cr_test)
str(cr_test)
cr_test[which(is.na(cr_test$Gender)),"Gender"]<-"Male"
cr_test[which(is.na(cr_test$Married)),"Married"]<-"Yes"
cr_test[which(is.na(cr_test$Dependents)),"Dependents"]<-"0"
cr_test[which(is.na(cr_test$Education)),"Education"]<-"Graduate"
cr_test[which(is.na(cr_test$Self_Employed)),"Self_Employed"]<- "No"
cr_test[which(is.na(cr_test$Credit_History)),"Credit_History"]<- "1"
table(is.na(cr_test$Credit_History))
cr_test[which(is.na(cr_test$Credit_History)),"Credit_History"]<- "1"

#for numerical variables in test dataset
cr_test[which(is.na(cr_test$LoanAmount)),"LoanAmount"]<-median(is.na(cr_test$LoanAmount),na.rm = T)
cr_test[which(is.na(cr_test$Loan_Amount_Term)),"Loan_Amount_Term"]<-
  median(is.na(cr_test$Loan_Amount_Term),na.rm = T)

missmap(cr_test)


#for categorical variable in validation data set
summary(cr_valid)
str(cr_valid)
cr_valid[which(is.na(cr_valid$Gender)),"Gender"]<-"Male"
cr_valid[which(is.na(cr_valid$Married)),"Married"]<-"Yes"
cr_valid[which(is.na(cr_valid$Dependents)),"Dependents"]<-"0"
cr_valid[which(is.na(cr_valid$Education)),"Education"]<-"Graduate"
cr_valid[which(is.na(cr_valid$Self_Employed)),"Self_Employed"]<-"No"
cr_valid[which(is.na(cr_valid$Credit_History)),"Credit_History"]<-"1"


#for numerical variables in validation dataset
cr_valid[which(is.na(cr_valid$LoanAmount)),"LoanAmount"]<-median(is.na(cr_valid$LoanAmount),na.rm = T)
cr_valid[which(is.na(cr_valid$Loan_Amount_Term)),"Loan_Amount_Term"]<-
  median(is.na(cr_valid$Loan_Amount_Term),na.rm = T)

str(cr_valid)


#all missing values should have been imputed by now and no NA's should be present
summary(cr_train)
summary(cr_test)
summary(cr_valid)


#for converting the credit history into numerical variable in train, test and validation set
summary(cr_train$Credit_History)
str(cr_train)
cr_train$Credit_History<- as.numeric(cr_train$Credit_History)
summary(cr_train$Credit_History)

summary(cr_test$Credit_History)
cr_test$Credit_History<- as.numeric(cr_test$Credit_History)
summary(cr_test$Credit_History)

summary(cr_valid$Credit_History)
cr_valid$Credit_History<- as.numeric(cr_valid$Credit_History)
summary(cr_valid$Credit_History)


#check summary of all three datasets
#there should be no NA's now in any of the dataset
summary(cr_train)
summary(cr_test)
summary(cr_valid)

missmap(cr_train)
missmap(cr_test)
missmap(cr_valid)
dev.off()


#plotting the relationship between the input and the output variables
str(cr_train)
names(cr_train)
     #laon status is a dependent variable
plot(table(cr_train$Gender,cr_train$Loan_Status),main="Gender vs Loan status",
     col = c("yellow","green"))

plot(table(cr_train$Married,cr_train$Loan_Status),main="Marital status vs Loan status",
     col = c("yellow","green"))

dev.off()
plot(table(cr_train$Dependents,cr_train$Loan_Status),main="No of independents vs Loan status",
     col=c("orange","blue"))

plot(table(cr_train$Education,cr_train$Loan_Status),main="Education vs Loan status",
     col=c("red","green"))

dev.off()
plot(table(cr_train$Self_Employed,cr_train$Loan_Status),main="Self_Employed vs Loan status",
     col=c("blue","orange"))

plot(table(as.factor(cr_train$Loan_Amount_Term),cr_train$Loan_Status),
           main="loan_Amount_Term vs loan status",col=c("Red","Green"))


str(cr_train)
table(cr_train$Loan_Amount_Term)
range(cr_train$CoapplicantIncome)
fivenum(cr_train$CoapplicantIncome) #fivenum is not different than summary,just summary giving you a
                                    #mean and fivenum does not giving you a mean
summary(cr_train$CoapplicantIncome)

boxplot(cr_train$CoapplicantIncome,col = "orange")
median(cr_train$CoapplicantIncome)


#showing the relationship with the help of using boxplot
boxplot(cr_train$CoapplicantIncome~cr_train$Loan_Status,main="Applicant income vs Loan status",
        col=c("Red","blue"),ylim=c(0,15000))

boxplot(cr_train$CoapplicantIncome ~cr_train$Loan_Status,main="Co-applicant Income vs Loan status",
        col=c("Red","Green"),ylim=c(0,4000))


plot(table(as.factor(cr_train$Loan_Amount_Term),cr_train$Loan_Status),xlab = "Loan Amount Term",
     ylab = "Loan status",col=c("Red","Green")) 
class(cr_train$Loan_Amount_Term)

plot(table(as.factor(cr_train$Property_Area),cr_train$Loan_Status),xlab = "Rural to Urban",
     main="Property area vs Loan approval",col=c("Red","Green","Blue"))

plot(table(as.factor(cr_train$Credit_History),cr_train$Loan_Status),xlab = "Credit History",
     main="Loan status",col=c("Red","Green"))


str(cr_train)


#changing the type of variable in training dataset for the purpose fo glm
class(cr_train$Gender)
cr_train$Gender<-as.numeric(ifelse(cr_train$Gender=="Male",1,0)) #this step is called dummy coding
#it means wherever it is Male, we are putting 1 otherwise it is going to be 0.

cr_train$Married<-as.numeric(ifelse(cr_train$Married=="Yes",1,0))
#wherever marital status is Yes we are putting 1 otherwise it is going to 0.

cr_train$Education<-as.numeric(ifelse(cr_train$Education=="Graduate",1,0))

cr_train$Self_Employed<-as.numeric(ifelse(cr_train$Self_Employed=="Yes",1,0))

View(cr_train)

#when there are three levels like(Rural,urban and semiurban) of any catigorical variable we just 
#need two dummy variables. Because if my property area rural is 0 and if my property area urban is 
#0 then it clearly means that the property area is of the third type.

cr_train$Property_Area_Rural<-as.numeric(ifelse(cr_train$Property_Area=="Rural",1,0))
 #we are created Property area rural

cr_train$Property_Area_Urban<-as.numeric(ifelse(cr_train$Property_Area=="Urban",1,0))
 #we are created Property area Urban

View(cr_train)

cr_train$Loan_Status<-as.factor(ifelse(cr_train$Loan_Status=="Y",1,0))
  #Now having this created these two column we are going to  drop 'Property Area' column


cr_train$Property_Area<- NULL
cr_train$Loan_ID<-NULL


#
table(cr_train$Dependents)
class("3+")
str(cr_train)
class(cr_train$Dependents)
cr_train$Dependents=="3+"  #there is no "3+" in dataset so its showing False
cr_train$Dependents==3   
table(cr_train$Dependents==3) #so there are 51 people who have got 3 dependents

cr_train$Dependents<-as.numeric(ifelse(cr_train$Dependents=="3+",3,cr_train$Dependents))
   #these "3+" is a character but do you want that to remain as character, so we just putting this
   #"3+" as 3. we are changing this "3+" into 3 that's it. we could have put 4 over here. But we dont 
   # want to change the center of the number of dependents that's why we put 3 over here. if we put 4  
   #over here there might be change in the central value of the dependents variable.

#again explanation
cr_train$Dependents<-as.numeric(ifelse(cr_train$Dependents=="3+",3,cr_train$Dependents))
    #we are say if cr_train$dependents is equal to 3+,put 3 and then we are saying convert this 
    #into numeric and then we are saying put those values in the column dependents.

x=3  #here we assigning the value 
x==3 #here we just confirming the x is really 3
x!=3


#if you want to more info. about the plot and etc. just use the commant like (?barplot,?boston)



#changing the type of variable in validation dataset for the purpose of glm
head(cr_valid)

cr_valid$Gender<-as.numeric(ifelse(cr_valid$Gender=="Male",1,0))
cr_valid$Married<-as.numeric(ifelse(cr_valid$Married=="Yes",1,0))
cr_valid$Education<-as.numeric(ifelse(cr_valid$Education=="Graduate",1,0))
cr_valid$Self_Employed<-as.numeric(ifelse(cr_valid$Self_Employed=="Yes",1,0))

cr_valid$Property_Area_Rural<-as.numeric(ifelse(cr_valid$Property_Area=="Rural",1,0))
cr_valid$Property_Area_Urban<-as.numeric(ifelse(cr_valid$Property_Area=="Urban",1,0))


cr_valid$Dependents<-as.numeric(ifelse(cr_valid$Dependents=="3+",3,cr_valid$Dependents))

names(cr_valid)

cr_valid$Loan_ID<-NULL
cr_valid$Property_Area<-NULL



#changing the type of variable in testing dataset for the purpose of glm

cr_test$Gender<-as.numeric(ifelse(cr_test$Gender=="Male",1,0))
cr_test$Married<-as.numeric(ifelse(cr_test$Married=="Yes",1,0))
cr_test$Education<-as.numeric(ifelse(cr_test$Education=="Graduate",1,0))
cr_test$Self_Employed<-as.numeric(ifelse(cr_test$Self_Employed=="Yes",1,0))

cr_test$Property_Area_Rural<-as.numeric(ifelse(cr_test$Property_Area=="Rural",1,0))
cr_test$Property_Area_Urban<-as.numeric(ifelse(cr_test$Property_Area=="Urban",1,0))

cr_test$Dependents<-as.numeric(ifelse(cr_test$Dependents=="3+",3,cr_test$Dependents))

cr_test$Loan_ID<-NULL
cr_test$Property_Area<-NULL

names(cr_valid)

names(cr_train)


#lets apply the glm in our training dataset
names(cr_train)
str(cr_train)

table(cr_train$Education)

log_model <- glm(Loan_Status ~ .,data = cr_train,family = "binomial")
#now we are using the (glm) command it use to create a generlize linear model. so we use this 
# command to create the logistic regression model. And loan_status is the dependent variable we
# want to predict the dependent variable be taking into account all the input variables.
# there is the two class classification so that is the reason the family = binomial.

summary(log_model)
# In summary explaination:
# 1. the intercept is the beta 0
# 2. Gender showing high P value is 0.98 so we would say beta for Gender is =0(means we have to
#     drop it.) 
# 2. self employed also high value it is not important
# 3. Now 'ApplicantIncome' and 'CoapplicantIncome' there is some multicollinearity happening between
#     these two. that is why this is now showing important over here. So we have to create the
#     model by excluding 'CoapplicantIncome' and by considering the 'ApplicantIncome' then you
#     will see that 'ApplicantIncome' will have 'P value' which is much lesser.
# 4. Null deviance is a result of a model which only consider 'beta 0' not 'beta'n(B1,B2...)
# 5. Residual deviance is consider 'beta 0' and 'beta'n. So 'NUll deviance' is higher and 
#    'Residual deviance' is lower means you beta's working good.
# 6. Number of Fisher Scoring iterations, So this model are able to converge 5 iterations.


# 1. Null deviance: A low null deviance implies that the data can be modeled well merely using the intercept.
#    If the null deviance is low, you should consider using few features for modeling the data.
# 2. Residual deviance: A low residual deviance implies that the model you have trained is appropriate.



# when you create a model you dont have any dearth of data So we can do is we can split our datasets
# into three part The training part which we use to learn our parameters, The validation part on  
# which we will validate our model, If we find the model is good we will gohead and do the actual 
# prediction on the testing data. If you dont find the good model then we recreate the model by 
# looking at the 'P'value. And if you create the multiple model we find the better model by looking
# at the AIC value.



# Question: why we do the prediction on the validation dataset to check the performance why not on 
# the training dataset itself?
# Ans: we are created the model over here using the training data. Now you know what is there in 
#      training data so if you are ask to prediction on the training data itself, it is very easy 
#      you to do the prediction on the training data, because we have already seen these data
#      but the validation dataset which one you not seen that is why to check the efficacy of the 
#      model we are the validation dataset.



# checking the accuracy of the model using validation set

predic_on_valid<-predict(log_model,newdata = cr_valid,type = "response")

predic_on_valid[1:10]
cr_valid$outcome[1:10]


#accuracy calculation for threshold of 0.5

table(cr_valid$outcome,predic_on_valid>0.5)
   # we are taking the predictions which are greater than 0.5 then we are comparing with the actual
   # outcome. And there are 20 errors. (19+1)

acc_log_model<-(58+289)/(58+19+1+289)
print(acc_log_model) # The accuracy is showing 0.94


#accuracy calculation for threshold of 0.75
     
    # Now we are increase our Thereshold value Manually.
table(cr_valid$outcome,predic_on_valid>0.75)
acc_log_model_1<-(70+208)/(70+208+82+7)

print(acc_log_model_1) # The accuracy is showing .75


#accuracy calculation for threshold of 0.65

table(cr_valid$outcome,predic_on_valid>0.65)
acc_log_model_2<-(60+274)/(60+274+17+16)

print(acc_log_model_2) # the accuracy of .65 is 0.91. it is close to 1.0, so it is better.


# we use the model to predict on the test data

predicttest<-predict(log_model,newdata = cr_test,type = "response")

predicttest #showing the probability of each customer to getting the loan.
 # Now we have to decide about the Threshold which Threshold we consider in order to approve the loan
 # or reject the loan.

#for checking this we shall calculate  the AUC value 

cr_valid$outcome
table(cr_valid$outcome)

install.packages("ROCR")
install.packages("gplots")
  # ROCR and gplots we are going to use for ROC plot.

library(ROCR)
library(gplots)


ROCRpred<-prediction(predicttest,cr_valid$outcome) # Here we are taking the outcome only because we
    # want to study the impact of the Threshold value on the TPR and FPR.
ROCRpred

ROCRpref<-performance(ROCRpred,"tpr","fpr")

plot(ROCRpref)
plot(ROCRpref,print.cutoffs.at=seq(0,1,.1),colorize=T)


#to calculate the area under the curve
as.numeric(performance(ROCRpred,"auc")@y.values) # In this we are just Measuring the plot.
      # we are get 0.94% accuracy.


#for adding an additional column in the test dataset classifying Approval is Yes or No
#We have been able to get the class (Approved yes or No)for each client/observation

cr_test$Approval<-ifelse(predicttest>0.5,"Y","N") # We are adding the column name is Approval in the
                     # cr_test datasets.In this ifelse your probability predicted is 
                     # greater than 0.5 it is Yes otherwise it is NO. so it depends on you whether 
                     # you want 0.5,0.6 etc., depending on your industry your preference of TPR and
                     # FPR.

table(cr_test$Approval)

View(cr_test) # so in our last column have Approval which is have Yes or No.


# So we have created the model a logistic regression model, to predict whether the bank should be 
# giving the loan to the customer or Not on the basis of the input variables such as his Income, 
# CoapplicantIncome,Gender,Marrital Status, self employed Yes or No, credit history available Yes or
# No, so we have created this model.


#Sensitivity is as same as TPR
#Specificty is same as TNR
#FPR=(1-Specificity Ratio)

#Hosmer L test

#Null hypothesis states, the model fits data well. 
#Alt  hypothesis states , the model doesnot fit the data well

install.packages("ResourceSelection")
library(ResourceSelection)

#alpha level 5% or 10%
log_model$y
hl<-hoslem.test(log_model$y,fitted(log_model),g=10)

hl

#cbind(hl$expected,hl$observed)
#
#checking if the model converged 

log_model$converged

#F>N

names(cr_train)

log_model_new<- glm(Loan_Status ~ Married+Education+ApplicantIncome+LoanAmount+Loan_Amount_Term+
                      Credit_History+Property_Area_Rural+Property_Area_Urban,data = cr_train,
                    family = "binomial")

summary(log_model)

summary(log_model_new)


#The null deviance shows how well the response is predicted by the model with nothing but an intercept.

#The residual deviance shows how well the response is predicted by the model when the predictors are included

#Fisher Scoring Iterations. This is the number of iterations to fit the model.
# The logistic regression uses an iterative maximum likelihood algorithm to fit the data

