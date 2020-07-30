# Import test data
data=read.csv("sample1.csv")
head(data)  # display the first 6 observations
print(summary(data))
#We are going to look at the relationship between fear of violent crime (`tcviolent`), 
#high scores represent high fear, with a variable measuring perceptions on antisocial behaviour in the local area (`tcarea`), high scores represent the respondents perceive a good deal of antisocial behaviour[^1]:
reg <- lm(tcviolent ~ sex, data=data)
reg1 <- lm(tcviolent ~ tcarea + sex, data=data)
reg2 <- lm(tcviolent ~ . , data=data)
print(reg)
print(summary(reg))
print(summary(reg$coefficients))
plot(reg$residuals, pch = 16, col = "red")

library(ggplot2)
qplot(x = tcviolent, data = data)
qplot(x = tcarea, data = data)

attributes(reg)
print(summary(reg$model))
print(summary(reg$coefficients))

modelSummary <- summary(reg)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["experience", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["experience", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- reg$fstatistic[1]  # fstatistic
f <- summary(reg)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)
# For model comparison, the model with the lowest AIC and BIC score is preferred.
AIC(reg)  
BIC(reg)
