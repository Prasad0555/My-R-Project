
##At the time correlation will give you value which is as high as .8 or .9 but
#then it does not mean that the relationship that exist between these variable
#are linear. like below the example

seq(1,200,1)
x<- seq(1,200,1)
y<- x**2
cor(x,y)
plot(x,y)

x<- seq(-200,200,1)
y<- x**2
plot(x,y)
cor(x,y)


#set the working directory

setwd("C:\\Users\\MyPC\\Desktop\\Project 1")

#read the training and the testing data

train<- read.csv("Property_Price_Train.csv")
test<- read.csv("Property_Price_Test.csv")
View(train)
str(train)

#run the head, str and summary commands

str(train)
summary(train)
dim(train) # showing rows and columns number
dim(test)
colnames(train) # showing all names in our datasets

df<- train [,c(2,5,6,8,12,13,14,16,17,19,20,22
               ,30,31,33,34,39,40,41,42,43,44,45,
               54,57,63,68,72,77,78,79,80,81)]
      #it is created by sir judgement

dim(df)
head(df) #these will show you what you pick up columns
View(df)


#install amelia
install.packages("Amelia") # with the help of Amelia package we will use the function missmap
                            #function
library(Amelia)
missmap(train) #this will showing you missing value wherever these white line coming it means, that 
                #particular column and no. has missing value
missmap(df)
colnames(df) #this will give you the names of the columns

colSums(is.na.data.frame(df)) #column wise it is going to give us a no. of missing values

sort(colSums(is.na(df)),decreasing = T) #is.na means you are confirming is there is any 
                                        #na(missing value)


#Handling the missing values
is.na(df$Exposure_Level) #every row of this column you will get true/false of the column

table(is.na(df$Exposure_Level)) #this will give you the frequency of how many true/false

  #table command lets you do a frequency or calculate the frequency of the categorical variable

table(df$Exposure_Level) # this will showing you four categorical variable. in categorical 
                          #mode is the central value.

   #now all the missing value in a variable will be replace in a central value(we will assign the 
                                   #missing value into NO because NO is highest)


#if there is 50 percent missing values then we will delete it but when it is important values
# then we will have to retain it.

df$Exposure_Level[is.na(df$Exposure_Level)]<-"NO"


table(is.na(df$Basement_Height))
table(df$Basement_Height) #model class is "TA" [highest frequency (mode)]   
df$Basement_Height[is.na(df$Basement_Height)]<-"TA"


table(is.na(df$BsmtFinType1))
table(df$BsmtFinType1) #Unf is model class
df$BsmtFinType1[is.na(df$BsmtFinType1)]<-"Unf"


table(is.na(df$Electrical_System))
table(df$Electrical_System) #model class is "SBrkr"
df$Electrical_System[is.na(df$Electrical_System)]<-"SBrkr"


#inspect the attribute lot size
library(moments) #it will showing you skewness
skewness(df$Lot_Size) #showing positive skewed
summary(df$Lot_Size)

dev.off() # dev.off for resetting the plotting parameters
hist(df$Lot_Size,col = "red", breaks = 200)
hist(df$Lot_Size,col = "blue",breaks = 200, xlim = c(0,40000))

dev.off()

colSums(is.na(df))

#road type
table(df$Road_Type)
table(is.na(df$Road_Type))
barplot(table(df$Road_Type),col = c("red","green")) #majority having paved roads.


#lets check the attributre of exposure level
summary(df$Exposure_Level)
table(is.na(df$Exposure_Level))
df$Exposure_Level[1:5]


#replace the missing value with NO as it is the modal class and also if there is no info we
              #assume that there is no Exposure

df$Exposure_Level[is.na(df$Exposure_Level)]<- "No"
barplot(table(df$Exposure_Level),col = "orange", xlab = "Exposure Level", ylab = "count")


#property shape
barplot(table(df$Property_Shape), col = c("red","green","blue","yellow"))

#property slope
table(df$Property_Slope)

#Neighborhood
table(df$Neighborhood)
sort(table(df$Neighborhood),decreasing = T)
table(is.na(df$Neighborhood))
pie(table(df$Neighborhood))
barplot(table(df$Neighborhood))


#house condition
table(df$House_Condition)

#construction year
table(df$Construction_Year)
range(df$Construction_Year) #its give you first year and last year(oldest property and latest pro.)


#lets check the correlation between the inpute and output variable
cor(df$Construction_Year,df$Sale_Price)
plot(table(df$Construction_Year),col="orange",lwd = 4)

cor(df$Lot_Size,df$Sale_Price) #showing very low correlation
plot(df$Lot_Size,df$Sale_Price,col="red")

cor(df$Total_Basement_Area,df$Sale_Price)
plot(df$Total_Basement_Area,df$Sale_Price, xlim = c(0,3000),xlab = "Basement area",
     ylab = "Price of the property",col="red")
    #looking very clear linear trend


boxplot(df$Lot_Size,ylim=c(0,50000)) #showing too many outliers
IQR(df$Lot_Size) #IQR is difference between Q3 and Q1
summary(df$Lot_Size)
table(df$Lot_Size>11603) # showing outliers which is further than 11603 rows


cor(df$First_Floor_Area,df$Sale_Price)
plot(df$First_Floor_Area,df$Sale_Price,xlim=c(0,3000),col="blue",xlab= "Floor area",ylab="Price")

plot(df$Second_Floor_Area,df$Sale_Price,xlab="Floor area", ylab="Price",col="orange")
     #there are lot of houses which dont have second floor so there showing vertical line

cor(df$Garage_Area,df$Sale_Price) #showing very low correlation
plot(df$Garage_Area,df$Sale_Price,col="blue",pch=1,xlab ="Garage area", ylab = "Price")
     #looking uncorrelated data


#Test data
df_test<-test[,c(2,5,6,8,12,13,14,16,17,19,20,22,30,31,33,34,39,40,41,42,43,44,45,54,
                 57,63,68,72,77,78,79,80)]
missmap(df_test)

#Handling the missing data in the test data
colSums(is.na(df_test))
sort(colSums (is.na(df_test)),decreasing = T)        

table(df_test$Basement_Height)
df_test$Basement_Height[is.na(df_test$Basement_Height)]<-"TA"

table(df_test$Exposure_Level)
df_test$Exposure_Level[is.na(df_test$Exposure_Level)]<-"No"

table(df_test$BsmtFinType1)
df_test$BsmtFinType1[is.na(df_test$BsmtFinType1)]<-"Unf"
 
table(df_test$Total_Basement_Area) #in df_test data 'Total_Basement_Area' has not categorical variable
                                  #so we take median value. median value is perfect value.
median(df_test$Total_Basement_Area,na.rm = T) #na.rm its remove the missing value and calculate.
                                              #we take median because its a perfect value
df_test$Total_Basement_Area[is.na(df_test$Total_Basement_Area)]<-988

table(df_test$Kitchen_Quality)
df_test$Kitchen_Quality[is.na(df_test$Kitchen_Quality)]<-"TA"

median(df_test$Garage_Area,na.rm = T)

df_test$Garage_Area[is.na(df_test$Garage_Area)]<-480

#if the +(plus) sign coming then its a message the line is incomplete
table(df_test$Sale_Type)
df_test$Sale_Type[is.na(df_test$Sale_Type)]<-"WD"


#final check of the missing values on the datasets
missmap(df)
missmap(df_test)


#inspect the attribute building class
table(df$Building_Class)
cor(df$Building_Class,df$Sale_Price)
df$Building_Class<- NULL #this will drop the variable
missmap(df)

df_test$Building_Class<-NULL

colnames(df) #this will show you all the name of columns
names(df)


pnorm(.107,0,1,lower.tail = F)
pnorm(.107,0,1,lower.tail = F)*2
pnorm(-1.64,0,1)
pnorm(-1.64,0,1,lower.tail = T)
pnorm(1.64,0,1,lower.tail = F)


1.364e+5 #means = 1.364*10^+5


dim(df)
dim(df_test)
colnames(df)
colnames(df_test)
colnames(df_test)==colnames(df)


df_test$Building_Class<-NULL


#lets remove them
df_train<-df[-c(524,1183,1299,186,589,1271,899,692,1325,524,1183,1299,804,1170,582,689,441,1047),] 
  #we drop the outliers 


#lets again create a linear model and see if the multiple r square improves
df_train$Condition1<-NULL
df_test$Condition1<-NULL

df_train$Sale_Condition<-NULL
df_test$Sale_Condition<-NULL


levels(df_test$Road_Type)
levels(df_train$Road_Type)
levels(df_train$Road_Type)<-levels(df_test$Road_Type) # Road_type is categorical variable, training data
#has different levels of the categories which is not matching with the levels of Road_type in
#testing data, so we are taking levels of the test data and we are matching it with the levels
#of the training data.

levels(df_train$Property_Shape)<-levels(df_test$Property_Shape)

levels(df_train$Property_Slope)<-levels(df_test$Property_Slope)
levels(df_train$Condition1)<-levels(df_test$Condition1)

levels(df_train$House_Type)<-levels(df_test$House_Type)

levels(df_train$House_Design)->levels(df_test$House_Design)


lin_mod_1<-lm(Sale_Price~ .,data =df_train)
summary(lin_mod_1)   #our 'P' value should be minimum and Adjusted R squared should be maximum then 
                     #your model is perfect model.

#sum of all residuals as equal to  zero
sum(lin_mod_1$residuals)


#lets carry out the final prediction
predict(lin_mod_1,newdata = df_test)

tail(predx,100) #this will show you last 100 predict observations


#multiple packages for VIF
install.packages("car")
library(car) #for vif

vif(lin_mod_1) #the some value is greater than 10 so we need to drop it cause they having the 
  #problem of multicollinearity

install.packages("DAAG")
library(DAAG)
vif(lin_mod_1)
table(vif(lin_mod_1)>10) #this will showing the values which is greater than 10
sort(vif(lin_mod_1),decreasing = T)

#residuals versus fitted plot 
plot(lin_mod_1,1)

#Quantile quantile plot
plot(lin_mod_1,2)

#Scale location plot
plot(lin_mod_1,3)

#cooks distance plot
plot(lin_mod_1,4)

#residuals leverage plot
plot(lin_mod_1,5)


boxplot(df_train$Sale_Price)
fivenum(df_train$Sale_Price)
summary(df_train$Sale_Price)

IQR(df_train$Sale_Price)
IQR(df_train$Sale_Price)*1.5
new_df<-df_train[df_train$Sale_Price>124800,] #this new data is outliers

dim(new_df) #so 1145 is rows and 30 columns




