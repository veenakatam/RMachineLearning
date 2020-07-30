# Import test data
data=read.csv("Admission.csv")
head(data)  # display the first 6 observations
print(summary(data))
# Check distribution between the 2 binary classes
xtabs(~admit + rank, data = data)
xtabs(~admit + gpa, data = data)

#First, we convert rank to a factor to indicate that rank should be treated as a categorical variable.

data$rank <- factor(data$rank)
logit <- glm(admit ~ gre + gpa + rank, data = data, family = "binomial")
summary(logit)
# We can use the confint function to obtain confidence intervals for the coefficient estimates. 
## CIs using profiled log-likelihood
confint(logit)
## CIs using standard errors
confint.default(logit)
#To get the exponentiated coefficients, you tell R that you want to exponentiate (exp)
## odds ratios only
exp(coef(logit))
## odds ratios and 95% CI
exp(cbind(OR = coef(logit), confint(logit)))
#Now for example, we can say that for a one unit increase in gpa, the odds of 
#being admitted to graduate school (versus not being admitted) increase by a factor of 2.17. 
with(logit, null.deviance - deviance)
#The chi-square of 40.53 with 5 degrees of freedom and an associated p-value of less than 0.001 tells us that our model 
#as a whole fits significantly better than an empty model.

data=read.csv("smarket.csv")
head(data)  # display the first 6 observations
print(summary(data))
names(data)
# Histogram of variables
par(mfrow=c(1,8))
for(i in 1:8) {
  hist(data[,i], main=names(data)[i])
}
install.packages("Amelia")
library(Amelia)
library(mlbench)
# Check missing data
missmap(data, col=c("blue", "red"), legend=FALSE)

#featutes/target distribution
pairs(data, col=data$Direction)

# Logistics Regression
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = data, family = binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit,type = "response")
glm.probs[1:5]

train.raw <- read.csv('train.csv',header=T,na.strings=c(""))

#Now we need to check for missing values and look how many unique values there are for each variable using the sapply() function which applies the 
#function passed as argument to each column of the dataframe.
sapply(train.raw,function(x) mean(is.na(x)))

missmap(train.raw, main = "Missing values vs observed")
