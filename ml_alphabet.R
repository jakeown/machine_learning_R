#install.packages("randomForest")
library(caret)
library(fields)
library(randomForest)
#setwd("/home/jared/Desktop/")
trainSet <- read.table("alphabet.csv", sep = ",", header = TRUE)
trainSet$Letter <- gsub('A', 1, trainSet$Letter)
trainSet$Letter <- gsub('B', 2, trainSet$Letter)
trainSet$Letter <- gsub('C', 3, trainSet$Letter)
trainSet$Letter <- gsub('D', 4, trainSet$Letter)
trainSet$Letter <- gsub('E', 5, trainSet$Letter)
trainSet$Letter <- gsub('F', 6, trainSet$Letter)
trainSet$Letter <- gsub('G', 7, trainSet$Letter)
trainSet$Letter <- gsub('H', 8, trainSet$Letter)
trainSet$Letter <- gsub('I', 9, trainSet$Letter)
trainSet$Letter <- gsub('J', 10, trainSet$Letter)
trainSet$Letter <- gsub('K', 11, trainSet$Letter)
trainSet$Letter <- gsub('L', 12, trainSet$Letter)
trainSet$Letter <- gsub('M', 13, trainSet$Letter)
trainSet$Letter <- gsub('N', 14, trainSet$Letter)
trainSet$Letter <- gsub('O', 15, trainSet$Letter)
trainSet$Letter <- gsub('P', 16, trainSet$Letter)
trainSet$Letter <- gsub('Q', 17, trainSet$Letter)
trainSet$Letter <- gsub('R', 18, trainSet$Letter)
trainSet$Letter <- gsub('S', 19, trainSet$Letter)
trainSet$Letter <- gsub('T', 20, trainSet$Letter)
trainSet$Letter <- gsub('U', 21, trainSet$Letter)
trainSet$Letter <- gsub('V', 22, trainSet$Letter)
trainSet$Letter <- gsub('W', 23, trainSet$Letter)
trainSet$Letter <- gsub('X', 24, trainSet$Letter)
trainSet$Letter <- gsub('Y', 25, trainSet$Letter)
trainSet$Letter <- gsub('Z', 26, trainSet$Letter)
#head(trainSet)
#summary(trainSet)
trainSet$Letter = as.numeric(as.character(trainSet$Letter))
bplot.xy(trainSet$Letter, trainSet$f0)
bplot.xy(trainSet$Letter, trainSet$f1)
bplot.xy(trainSet$Letter, trainSet$f2)
bplot.xy(trainSet$Letter, trainSet$f3)
bplot.xy(trainSet$Letter, trainSet$f4)
bplot.xy(trainSet$Letter, trainSet$f5)
bplot.xy(trainSet$Letter, trainSet$f6)
bplot.xy(trainSet$Letter, trainSet$f7)
bplot.xy(trainSet$Letter, trainSet$f8)
bplot.xy(trainSet$Letter, trainSet$f9)
bplot.xy(trainSet$Letter, trainSet$f10)
bplot.xy(trainSet$Letter, trainSet$f12)
bplot.xy(trainSet$Letter, trainSet$f12)
bplot.xy(trainSet$Letter, trainSet$f13)
bplot.xy(trainSet$Letter, trainSet$f14)
bplot.xy(trainSet$Letter, trainSet$f15)
trainSet2 <- trainSet[1:10000,]
set.seed(42)

trainSet2$Letter <- factor(trainSet2$Letter)

model <- train(Letter ~ f5+f6+f7+f8+f9+f10+f12+f14, data=trainSet2, method="rf", trControl=trainControl(method="cv",number=5))
model
testSet <- trainSet[10000:20000,]
testSet$Letter <- predict(model, newdata = testSet)
#summary(testSet)

submission <- testSet[,c("Letter")]

testSet2 <- trainSet[10000:20000,]
sub2 <- testSet2[,c("Letter")]

sub3 <- (as.numeric(submission) - sub2)

100 - (sum(sub3!=0) / 10000)*100

