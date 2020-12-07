#Lance Miles Data Science 450 - Assignment 1    Date: 20170415

#####################
#Some code adapted from the following resources:
#Tok, W. H. (2017, April 12). WineQualityPrediction.ipynb. Retrieved April 15, 2017, from https://notebooks.azure.com/weehyong/libraries/uwdatasci450spring2017/html/WineQualityPrediction.ipynb
#
#####################


setwd("C:/Users/Lance Miles/Desktop/UW Datascience/DataScience 450/Assignment/Assignment 1")


if(!require(foreign)){
  install.packages("foreign")
  library(foreign)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(rpart)){
  install.packages("rpart")
  library(rpart)
}
if(!require(rpart.plot)){
  install.packages("rpart.plot")
  library(rpart.plot)
}
if(!require(caret)){
  install.packages("caret")
  library(caret)
}
if(!require(reshape2)){
  install.packages("reshape2")
  library(reshape2)
}


wine = read.arff(file = "RedWhiteWine.arff")
wine$quality = NULL

head(wine)
summary(wine)


ggplot(data = melt(wine), mapping = aes(x = value)) + 
  geom_histogram(bins = 32) + facet_wrap(~variable, scales = 'free_x')


set.seed(123)

sample = sample(nrow(wine), 0.7 * nrow(wine))
train.set = wine[sample,]
test.set = wine[-sample,]

#summary(wine$`R/W`)
#summary(train.set$`R/W`)
#summary(test.set$`R/W`)

trained.equation = rpart(`R/W` ~ pH + `volatile acidity`, data = train.set, method = "class")

rpart.plot(trained.equation)

prediction.RW = predict(trained.equation, test.set[,colnames(wine)], type = "class")
head(prediction.RW)

c.matrix = table(prediction.RW, test.set$`R/W`)
c.matrix

confusionMatrix(c.matrix)
