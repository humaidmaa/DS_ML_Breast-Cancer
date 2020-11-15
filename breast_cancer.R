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
library(scales)
library(GGally)
library(recipes)
library(glmnet)
library(caret)
library(rsample)
library(vip)



df=read.csv("C:/Users/mansour/Desktop/git hub/Portfolio_Builder_Exercises/Data/dataR2.csv")
nrow(df)
ncol(df)
head(df)
tail(df)
str(df)


#Conversion
df$Classification=factor(df$Classification)
df$Age=factor(df$Age)
str(df)
summary(df)

#finding null values

which(is.na(df))
any(is.na(df))

#Finding Outliers

# Create a boxplot of the dataset.
boxplot(df,horizontal = T)
colnames(df)

#for individual column
boxplot(df$BMI)#no Outliers
boxplot(df$Glucose)
boxplot(df$Insulin)
boxplot(df$HOMA)
boxplot(df$Leptin)
boxplot(df$Adiponectin)
boxplot(df$Resistin)
boxplot(df$MCP.1)#no outliers

# To get the actual values of the outliers with this

boxplot(df$MCP.1)$out

# To assign the outlier values into a vector

outliers=boxplot(df$MCP.1, plot=FALSE)$out
print(outliers)

#Removing the outliers

# First need find in which rows the outliers are

df[which(df$MCP.1 %in% outliers),]

# Now remove the rows containing the outliers, one possible option is:

df = df[-which(df$MCP.1 %in% outliers),]

# check now with boxplot,  will notice that those pesky outliers are gone

boxplot(df$MCP.1)

outliers=boxplot(df$Resistin, plot=FALSE)$out
df[which(df$Resistin %in% outliers),]
df = df[-which(df$Resistin %in% outliers),]
boxplot(df$Resistin)

outliers=boxplot(df$Adiponectin, plot=FALSE)$out
df[which(df$Adiponectin %in% outliers),]
df = df[-which(df$Adiponectin %in% outliers),]
boxplot(df$Adiponectin)

##Visualization
# plain gragh is seen
ggplot(data=df, aes(x=BMI, y=MCP.1))
#add  geometry
ggplot(data=df, aes(x=BMI, y=MCP.1))+ geom_point()
#add  color
#Lines
a=ggplot(data=df, aes(x=BMI, y=MCP.1, colour=Classification))+geom_point()
a
#add  size
ggplot(data=df, aes(x=Insulin, y=MCP.1, colour=Classification, Size=BMI))+ geom_point()
#ploting with layers
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
#IF your MCP count is between 0-250 then there is a chance that to get cancer

#model building
library(caTools)
set.seed(101)
sample=sample.split(df,SplitRatio = .7)
train=subset(df,sample==T)
test=subset(df,sample==F)

model=lm(MCP.1~.,data=df)
summary(model)

pred=predict(model,test)
pred

library(Metrics)

rmse(test$MCP.1, pred)


# Create training feature matrices

X <- model.matrix(MCP.1 ~ ., train)[, -1]
