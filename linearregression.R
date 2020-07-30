# Import test data
data=read.csv("LR1.csv")

head(data)  # display the first 6 observations
print(summary(data))
scatter.smooth(x=data$experience, y=data$salary, main="Experince ~ Salary")  # scatterplot
linearMod <- lm(salary ~ experience, data=data)  # build linear regression model on full data
print(linearMod)
print(summary(linearMod))
plot(linearMod$residuals, pch = 16, col = "red")
modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["experience", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["experience", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
# For model comparison, the model with the lowest AIC and BIC score is preferred.
AIC(linearMod)  
BIC(linearMod)

# Build the model on training data -
trainingData=read.csv("LR1_Train.csv")
testData=read.csv("LR1_Test.csv")
lmMod <- lm(salary ~ experience, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance
summary (lmMod)
summary (distPred)
# calculate prediction accuracy and error rates
#actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))  # make actuals_predicteds dataframe.
#correlation_accuracy <- cor(actuals_preds)  # 
#print(head(actuals_preds))

# second dataset, many variables, but all numeric
diabetes=read.csv("diabetes.csv")
lmdiabetes = lm(Outcome~., data = diabetes) #Create the linear regression
summary(lmdiabetes) #Review the results
plot(lmdiabetes$residuals, pch = 16, col = "red")
modelSummary <- summary(lmdiabetes)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["Pregnancies", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["Pregnancies", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- lmdiabetes$fstatistic[1]  # fstatistic
f <- summary(lmdiabetes)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
# For model comparison, the model with the lowest AIC and BIC score is preferred.
AIC(lmdiabetes)  
BIC(lmdiabetes)

# Build the model on training data -

diabetes1=read.csv("diabetes.csv")
library(dplyr)
train<-sample_frac(diabetes1, 0.5)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-diabetes1[-sid,]
lmMod <- lm(Outcome ~ ., data=train)  # build the model
plot(lmMod$residuals, pch = 16, col = "red")
distPred <- predict(lmMod, test)  # predict distance
summary (lmMod)
