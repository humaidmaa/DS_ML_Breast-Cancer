# Title: Breast Cancer study
# Name: Mansour Alhumaid
# Date: 02 Nov 2020
#The data contain Clinical features were observed or measured for 64 patients with breast cancer and 52 healthy controls.
#The dataset is downloaded from The UCI Machine Learning Repository
#There are 10 predictors, all quantitative, and a binary dependent variable, indicating the presence or absence of breast cancer.
#The predictors are anthropometric data and parameters which can be gathered in routine blood analysis.
#Prediction models based on these predictors, if accurate, can potentially be used as a biomarker of breast cancer.


# Data From:
# https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra

# load packages
library(tidyverse)
library(rio)
library(ggstatsplot)
library(ggplot2)
#library(scales)
#library(GGally)
#library(recipes)
#library(glmnet)
library(caret)
library(rsample)
library(vip)
library(corrplot)
library(caTools)
library(car)
library(FSelector)
library(data.tree)
library(rpart)
library(raprt.plot)

df <- read.csv("Data/dataR2.csv")

#EDA
nrow(df)
ncol(df)
head(df)
tail(df)
str(df)


#Conversion
df$Classification=factor(df$Classification)
df$Age=factor(df$Age)
df=subset(df,select = -c(1,10))

#finding null values

which(is.na(df))
any(is.na(df))


##Visualization
boxplot(df$MCP.1)
#Lines
a <- ggplot(data=df, aes(x=BMI, y=MCP.1, colour=Classification))+geom_point()
a
#add  size
ggplot(data=df, aes(x=Insulin, y=MCP.1, colour=Classification, Size=BMI))+ geom_point()
#plotting with layers
p<-ggplot(data=df, aes(x=Insulin, y=MCP.1, colour=Classification))
# point
a+geom_point(aes(size=BMI))
#multiple layer
p+geom_point()+geom_line()

##If the insulin is in the range of 0-20 ,
#MCP-1 is ranging in between 230 -1050 then there is a chance that the person can get breast cancer

# mapping vs setting
r<-ggplot(data=df, aes(x=Glucose, y=MCP.1))
# add color
# mapping
r+geom_point(aes(colour=Classification))
# setting
# mapping
r+geom_point(aes(size=Classification))
#If your glucose and MCP level are high then there is a change that you may get cancer
# setting
r+geom_point(size=10)

#----Denisty chart
s+geom_density(aes(fill=Classification))

#If MCP count is between 0-250 then there is a chance to get cancer

#Finding correlation
#when we have various variables,Correlation is an important factor to check the dependencies within themselves
#its gives us an insight between mutual relationship among variables
#to get correlation among  different variables for a data set use following code


plot(df)
#below codes gives graphical representation of correlation

cr=cor(df)

df.cor = cor(df)
corrplot(df.cor)
#corrplot(cr,type='lower')
corrplot(cr,method='number')
#from the above code we are getting HOMA AND Insulin are multicollinear varilables

#splitting the data into train and test

set.seed(101)
sample=sample.split(df$MCP.1,SplitRatio = .7)
train=subset(df,sample==T)
test=subset(df,sample==F)

#Multicollinearity
#Multicollinearity makes it hard to interpret your coefficients,
#and it reduces the power of your model to identify independent variables that are statistically significant.
#These are definitely serious problems


df=subset(df,select = -c(MCP.1))
numericData=df[sapply(df,is.numeric)]
descrCor=cor(numericData)

#vif

model=lm(MCP.1~.,data=train)
vif(model)
#from the above code we will get multicollinearity varibles Insulin and Homa,we need remove those coulmns

#Now create the model
model=lm(MCP.1~.,data=train)
summary(model)

#from summary  we came to know there is less correaltion bw all columns related to target column

#model creation after removing Insulin and Adiponectin
model=lm(MCP.1~BMI+Resistin+Glucose+HOMA+Insulin+Leptin,data=train)
summary(model)

pred=predict(model,test)
pred

#Comparing Predicted vs Actual
plot(test$MCP.1,type = "l",lyt=1.8,col="red")
lines(pred,type="l",col="blue")

plot(pred,type='l',lyt=1.8,col='blue')#from this data is not seems to be good

#Finding Accuracy
accuracy=sqrt(mean(pred-df$MCP.1)^2)

#converting 1 and 2 into 0 and 1
df$Classification=factor(df$Classification,labels=c(0,1))

#Model split
set.seed(101)
sample=sample.split(df$Classification,SplitRatio = 0.7)
train=subset(df,sample==TRUE)
test=subset(df,sample==FALSE)
#model building
model=glm(Classification~.,data=train,family='binomial')
summary(model)

#we need remove this one because its has less significance levels
Adiponectin

#predictions
res=predict(model,test,type='response')
res
#confusion matrix
table(ActualValue=test$Classification ,PredictedValue=res>0.5)

#Decision Tree

#Model Building
tree=rpart(Classification~.,data=train)
tree

#we can plot
plot(tree,margin=0.1)

#margin is used to adjust the plot ,for viewing labels
text(tree,use.n = TRUE,pretty = TRUE,cex=0.8)
#Predictions
pred=predict(tree,test,type='class')
pred
#Confusion matrix for evaluating the model
confusionMatrix(pred,test$Classification)


