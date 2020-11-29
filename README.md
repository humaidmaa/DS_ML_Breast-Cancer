# Portfolio_Builder_Exercises
ML Portfolio Builder Exercises

# Title: Breast Cancer study

#The data contain Clinical features were observed or measured for 64 patients with breast cancer and 52 healthy controls.
#The dataset is downloaded from The UCI Machine Learning Repository
#There are 10 predictors, all quantitative, and a binary dependent variable, indicating the presence or absence of breast cancer.
#The predictors are anthropometric data and parameters which can be gathered in routine blood analysis.
#Prediction models based on these predictors, if accurate, can potentially be used as a biomarker of breast cancer.


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
