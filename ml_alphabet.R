### This code uses random forests to classify distorted, black and white pixel displays
### as one of the 26 letters in the English alphabet.  Random distortions were applied to a sample 
### of 20,000 characters from 20 different font styles.  The attributes (e.g., height, total pixels, etc.) 
### for the 20,000 distorted characters are provided along with the corresponding letter from which
### they were distorted.  We will develop our model with a training set based on the first 10,000 characters.
### After, we will use our model to classify the remaining 10,000 characters and see how our predictions
### stack up with the actual values.

# Install the required packages if you don't already have them
# e.g., install.packages("randomForest")

# Load the packages
library(caret)
library(fields)
library(randomForest)

# Set the working directory where the data is located
#setwd("/home/Desktop/")

# Load the data in alphabet.csv
trainSet <- read.table("alphabet.csv", sep = ",", header = TRUE)

# Assign a number to each letter rather than a character
# This will make it easier to create the box plots that we 
# will need later to determine which parameters to include 
# in our model.
### ***There has to be a better way to do this!***
### For my first R code, however, I am just going to run with it
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

# Display the header and view the dataframe attributes
# if you want to see them.
#head(trainSet)
#summary(trainSet)

# Convert the Letter column to numbers rather than characters
# We want to make box plots, so we will need numbers.
trainSet$Letter = as.numeric(as.character(trainSet$Letter))

# Create box plots for each parameter and letter
# If the box plots for each of the letters are fairly 
# uniform for a certain parameter, exclude that 
# parameter from the model.  If the box plots are
# quite unique for a given parameter, it indicates that 
# parameter may be a useful classification tool. Include those
# parameters in the model.
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
# To me, 5-10, 12 and 14 look the best.
# We will include them in the model below.

# Grab the first 10,000 entries for our training set 
trainSet2 <- trainSet[1:10000,]

# Create a seed to get reproducable results
set.seed(42)

# Convert the training set to a factor so we can perform 
# classification rather than regression.
trainSet2$Letter <- factor(trainSet2$Letter)

# Train our model using random forests and cross-validation
model <- train(Letter ~ f5+f6+f7+f8+f9+f10+f12+f14, data=trainSet2, method="rf", trControl=trainControl(method="cv",number=5))

# Check out the model parameters if you like
model

# Grab the second 10,000 entries for our test set
testSet <- trainSet[10000:20000,]

# Predict the associated character for the test set based on our model  
testSet$Letter <- predict(model, newdata = testSet)
#summary(testSet)

prediction <- testSet[,c("Letter")]

testSet2 <- trainSet[10000:20000,]
sub2 <- testSet2[,c("Letter")]

sub3 <- (as.numeric(prediction) - sub2)

# This is the percentage that we got correct (should be ~90 %)
100 - (sum(sub3!=0) / 10000)*100

# Would be interesting to make a plot showing which letters 
# were most commonly mistaken as another letter.

