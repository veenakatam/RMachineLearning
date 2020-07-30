if(!require(arules)) install.packages("arules")
if(!require(arulesViz)) install.packages("arulesViz")
if(!require(dplyr)) install.packages("dplyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(knitr)) install.packages("knitr")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
library(arules)
library(arulesViz)
library(dplyr)
library(plyr)
library(lubridate)
library(ggplot2)
library(knitr)
library(RColorBrewer)


# First example from : http://www.rpubs.com/thirus83/445115
#Other refs: https://rstudio-pubs-static.s3.amazonaws.com/252505_4931623e7ab14851b1002d1bc61e94d9.html
#https://towardsdatascience.com/association-rule-mining-in-r-ddf2d044ae50
#https://www.datacamp.com/community/tutorials/market-basket-analysis-r
#https://blog.exploratory.io/introduction-to-association-rules-market-basket-analysis-in-r-7a0dd900a3e0
#https://www.r-bloggers.com/association-rule-learning-and-the-apriori-algorithm/
#http://www.rdatamining.com/examples/association-rules
#https://github.com/natarajan1993/Market-Basket-Analysis-with-R
# https://github.com/Deepaknatural/Training/blob/master/MarketBasket_Latest.R
#https://rstudio-pubs-static.s3.amazonaws.com/267119_9a033b870b9641198b19134b7e61fe56.html
# First lets use the AdultUCI dataset that comes bundled with the arules package.

data()
data("Groceries")
summary(Groceries)

rules <- apriori(Groceries,parameter=list(support=0.002, confidence = 0.5))
print(summary(rules))
print(rules)

inspect(head(sort(rules, by = "lift")))
plot(rules)
head(quality(rules))
plot(rules, method = "grouped")

plot(rules,method = "scatterplot")

plot(rules,method = "graph")

# Import test data
df <- read.csv("OnlineRetailSmall.csv")
head(df)

df <- df[complete.cases(df), ] # Drop missing values
# Change Description and Country columns to factors
# Factors are the data objects which are used to categorize the data and store it as levels.
df %>% mutate(Description = as.factor(Description),
              Country = as.factor(Country)) 

# Change InvoiceDate to Date datatype

df$Date <- as.Date(df$InvoiceDate) 
df$InvoiceDate <- as.Date(df$InvoiceDate)
# Extract time from the InvoiceDate column
TransTime<- format(as.POSIXct(df$InvoiceDate),"%H:%M:%S") 

# Convert InvoiceNo into numeric
InvoiceNo <- as.numeric(as.character(df$InvoiceNo)) 
# Add new columns to original dataframe
cbind(df, TransTime, InvoiceNo) 
glimpse(df)

# Group by invoice number and combine order item strings with a comma
transactionData <- ddply(df,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,collapse = ","))
transactionData$InvoiceNo <- NULL # Don't need these columns
transactionData$Date <- NULL

colnames(transactionData) <- c("items")
head(transactionData)

write.csv(transactionData,"market_basket_transactionsSmall1.csv", quote = FALSE, row.names = TRUE)


# MBA analysis 
# From package arules
tr <- read.transactions('market_basket_transactionsSmall.csv', format = 'basket', sep=',')

summary(tr)

# plot the frequency of items
itemFrequencyPlot(tr)

itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
arules::itemFrequencyPlot(tr,
                          topN=20,
                          col=brewer.pal(8,'Pastel2'),
                          main='Relative Item Frequency Plot',
                          type="relative",
                          ylab="Item Frequency (Relative)") 

# Generate the a priori rules
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
summary(association.rules)
inspect(association.rules[1:10]) # Top 10 association rules

# Select rules which are subsets of larger rules -> Remove rows where the sums of the subsets are > 1
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector

# What did customers buy before buying "METAL"
metal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(default="lhs",rhs="METAL"))
inspect(head(metal.association.rules))

# What did customers buy after buying "METAL"
metal.association.rules2 <- apriori(tr, parameter = list(supp=0.001, conf=0.8),appearance = list(lhs="METAL",default="rhs"))
inspect(head(metal.association.rules2))

# Plotting
# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.4]
#Plot SubRules
plot(subRules)

# Top 10 rules viz
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

# Filter top 20 rules with highest lift
# Paralell Coordinates plot - visualize which products along with which items cause what kind of sales.
# Closer arrows re bought together
subRules2<-head(subRules, n=20, by="lift")
plot(subRules2, method="paracoord")
