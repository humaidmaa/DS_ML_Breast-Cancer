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




install.packages('rsample')
install.packages('earth')
install.packages('vip')
install.packages('pdp')
install.packages('class')
library(rsample)   # data splitting
library(ggplot2)   # plotting
library(earth)     # fit MARS models
library(caret)     # automating the tuning process
library(vip)       # variable importance
library(pdp)       # variable relationships
# load packages
library(tidyverse)
library(rio)
library(ggstatsplot)
#library(scales)
#library(GGally)
#library(recipes)
#library(glmnet)
library( kableExtra)
library(corrplot)
library(caTools)
library(car)
library(FSelector)
library(data.tree)
library(rpart)
library(raprt.plot)
library(randomForest)#its works in regression and classification
library(class)

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

#converting categorical into binary features
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
#Sensitivity says actual positive  cases
#specialty says negative values


#Random Forest,  works in regression and classification
#before Build a model we need to check the significance level of each variable for that we will use tunaRE function

bestmtry=tuneRF(train,train$Classification,stepFactor = 1.2,improve = 0.01,trace=T,plot=T)

#OOB nothing but prediction error
#so our actual consider columns are max 3 enough for build a model


model=randomForest(Classification~.,data=train)
model

#No. of variables tried at each split: 3
#Every node will split into 3 daughter nodes
#  OOB estimate of  error rate: 32.1% its depends upon confusion matrix

#we need find the gini information for tree splitting

importance(model)

#visualizing the gini index
varImpPlot(model)

#predicting the model
pred=predict(model,test,type = 'class')
pred


#Confusion Matrix
confusionMatrix(table(pred,test$Classification))



#KNN
#Using Of KNN data Should be Labeled (structured format)
#Data Should be Noise free
#data should be small because KNN is lazy learner
#According to Ecludian distance we can calculate distance then we can say
#how many observations are similar based on their distance

#K-Nearest Neighbor also known as KNN is a supervised learning algorithm that can be used for regression as well as classification problems.
#But KNN is widely used for classification problems in machine learning.
#KNN works on a principle assuming every data point falling near to each other is falling in the same class.
#That means similar things are near to each other.

#KNN algorithms decide a number k which is the nearest Neighbor to that data point which is to be classified.
#If the value of k is 5 it will look for 5 nearest Neighbors to that data point.
#lets the example, k=4. KNN finds out 4 nearest Neighbors.
#It is seen that because that data point is close to these Neighbors so it will belong to this class only.

#What is the significance of k?

#Value of k - bigger the value of k increases confidence in the prediction.

#Load the desired data.

#Choose the value of k.

#For getting the class which is to be predicted, repeat starting from 1 to the total number of training points we have.

#The next step is to calculate the distance between the data point whose class is to be predicted and all the training data points. Euclidean distance can be used here.

#Arrange the distances in non-decreasing order.

#Assume the positive value of k and filtering k lowest values from the sorted list.

#We have top k top distances.


#Normalize the data because by doing this we can get all observations similar format
normalize=function(x){
 return((x-min(x)) / (max(x)-min(x)))}
nor=as.data.frame(lapply(df[,1:9],normalize))#By running this will get all normalized values
head(nor)

#Model splitting

set.seed(101)

build=sample(10:nrow(nor),size=nrow(nor)*0.7 ,replace=FALSE)

train.class=df[build,]#70% training data
test.class=df[-build,]# remaining 30% test data

#Now creating a separate data frame for 'Classification' feature which is our target.
train.class_labels=df[build,10]
test.class_labels=df[-build,10]



NROW(train.class_labels)# To find number of observations in our training data in target


#Optimization of K value

i=1
k.optm=1
for (i in 1:28) {
 knn.mod=knn(train=train.class ,test=test.class,cl=train.class_labels ,k=i)
 k.optm[i]=100*sum(test.class_labels==knn.mod)/NROW(test.class_labels)
 k=i
 cat(k,'=',k.optm[i],'\n')  # to print accuracy
}

plot(k.optm, type='b',xlab = 'k-value' ,ylab='Accuracy level') # To plot % accuracy wrt to k=value


knn.2=knn(train=train.class,test.class,cl=train.class_labels,k=2)
ACC.2=100*sum(test.class_labels==knn.2)/NROW(test.class_labels)  #k=2
ACC.2

# To check predictions against actual value in tabular form
table(knn.2,test.class_labels)


confusionMatrix(table(knn.2,test.class_labels))


#MARS
# Many of these models can be adapted to nonlinear patterns in the data by manually adding model terms (i.e. squared terms, interaction effects);
#however, to do so you must know the specific nature of the nonlinearity a priori.
#Alternatively, there are numerous algorithms that are inherently nonlinear.
#When using these models, the exact form of the nonlinearity does not need to be known explicitly or specified prior to model training.
#Rather, these algorithms will search for, and discover, nonlinearities in the data that help maximize predictive accuracy.



# Fit a basic MARS model
mars1=earth(MCP.1~ .,data =df)
mars1
# Print model summary
print(mars1)

#It also shows us that 6 of 16 terms were used from 5 of the 7 original predictors.
#But what does this mean? If we were to look at all the coefficients,
#we would see that there are 6 terms in our model (including the intercept).
#These terms include hinge functions produced
#from the original 7 predictors (7 predictors because the model automatically dummy encodes our categorical variables).

summary(mars1) %>% .$coefficients %>% head(10)

#The plot method for MARS model objects provide convenient performance and residual plots.

plot(mars1, which = 1)

# the model selection plot that graphs the
#GCV R2( generalized cross validation criterion. GCV is a form of regularization: it trades off goodness-of-fit against model complexity.)
#(left-hand y-axis and solid black line) based on the number of terms retained in the model
#(x-axis) which are constructed from a certain number of original predictors (right-hand y-axis).
#The vertical dashed lined at 37 tells us the optimal number of non-intercept terms retained where marginal increases in

#Tuning
#Since there are two tuning parameters associated with our MARS model:
#the degree of interactions and the number of retained terms,
#we need to perform a grid search to identify the optimal combination of these hyperparameters that minimize prediction error

# create a tuning grid
hyper_grid=expand.grid(
 degree = 1:3,
 nprune = seq(2, 100, length.out = 10) %>% floor()
)

head(hyper_grid)

#We can use caret to perform a grid search using 10-fold cross-validation
# for reproducibiity
set.seed(123)

# cross validated model
tuned_mars <- train(
 x = subset(df, select = -MCP.1),
 y = df$MCP.1,
 method = "earth",
 metric = "RMSE",
 trControl = trainControl(method = "cv", number = 10),
 tuneGrid = hyper_grid
)

# best model
tuned_mars$bestTune
##    nprune degree
## 14     34      2

# plot results
ggplot(tuned_mars)

#The following table compares the cross-validated RMSE for our tuned MARS model
#to a regular multiple regression model along with tuned principal component regression (PCR),
#partial least squares (PLS), and regularized regression (elastic net) models.
#By incorporating non-linear relationships and interaction effects,
#the MARS model provides a substantial improvement over the previous linear models that we have explored.

#Notice
#Notice that we standardize the features for the linear models but we did not for the MARS model.
#Whereas linear models tend to be sensitive to the scale of the features,
#MARS models are not.

# multiple regression
set.seed(123)
cv_model1 <- train(
 MCP.1 ~ .,
 data = df,
 method = "lm",
 metric = "RMSE",
 trControl = trainControl(method = "cv", number = 10),
 preProcess = c("zv", "center", "scale")
)

# principal component regression
set.seed(123)
cv_model2 <- train(
 MCP.1 ~ .,
 data = df,
 method = "pcr",
 trControl = trainControl(method = "cv", number = 10),
 metric = "RMSE",
 preProcess = c("zv", "center", "scale"),
 tuneLength = 20
)

# partial least squares regression
set.seed(123)
cv_model3 <- train(
 MCP.1 ~ .,
 data =df,
 method = "pls",
 trControl = trainControl(method = "cv", number = 10),
 metric = "RMSE",
 preProcess = c("zv", "center", "scale"),
 tuneLength = 20
)

# regularized regression
set.seed(123)
cv_model4 <- train(
 MCP.1 ~ .,
 data =df,
 method = "glmnet",
 trControl = trainControl(method = "cv", number = 10),
 metric = "RMSE",
 preProcess = c("zv", "center", "scale"),
 tuneLength = 10
)



# extract out of sample performance measures
summary(resamples(list(
 Multiple_regression = cv_model1,
 PCR = cv_model2,
 PLS = cv_model3,
 Elastic_net = cv_model4,
 MARS = tuned_mars
)))$statistics$RMSE %>%
 kableExtra::kable() %>%
 kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))

#Feature interpretation
#MARS models via earth::earth() include a backwards elimination feature selection routine
#that looks at reductions in the GCV estimate of error as each predictor is added to the model.
#This total reduction is used as the variable importance measure (value = "gcv").
#Since MARS will automatically include and exclude terms during the pruning process,
#it essentially performs automated feature selection.
#If a predictor was never used in any of the MARS basis functions in the final model (after pruning),
#it has an importance value of zero.

#while the rest of the features have an importance value of zero since they were not included in the final model.
#Alternatively, you can also monitor the change in the residual sums of squares (RSS) as terms are added (value = "rss");
#however, you will see very little difference between these methods.

# variable importance plots
p1 <- vip(tuned_mars, num_features = 40, bar = FALSE, value = "gcv") + ggtitle("GCV")
p2 <- vip(tuned_mars, num_features = 40, bar = FALSE, value = "rss") + ggtitle("RSS")

gridExtra::grid.arrange(p1, p2, ncol = 2)

#Its important to realize that variable importance will only measure the impact of the prediction error
#as features are included; however, it does not measure the impact for particular hinge functions created for a given feature.
#For example, in Figure  we see that Resistin and Glucose are the two most influential variables;
#however, variable importance does not tell us how our model is treating the non-linear patterns for each feature.
#Also, if we look at the interaction terms our model retained,
#we see interactions between different hinge functions for  Resistin and Glucose.


#MARS provides a great stepping stone into nonlinear modeling and tends to be fairly intuitive due to being closely related to multiple regression techniques.
#They are also easy to train and tune.
#From this illustrated how incorporating non-linear relationships via MARS modeling greatly
#improved predictive accuracy on our Brest Cancer data.

