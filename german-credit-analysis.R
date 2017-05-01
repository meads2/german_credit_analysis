###############################
##                           ##
##          LIBRARIES        ## 
##                           ##
###############################
library(caret)
library(ggplot)
library(caTools)

# Attach german dataset from caret package
data("GermanCredit")
attach(GermanCredit)

# Set Random Seed for reporducability
set.seed(999)

###############################
##                           ##
##   1. Data Exploration     ## 
##                           ##
###############################
# Tells yoy how many rows of data you have
str(GermanCredit)

# Runs summary statistics on Dataset
summary(GermanCredit)

# View Count Of Class
qplot(Class, 
      data = GermanCredit, 
      main = 'Count of Customers By Credit',
      xlab = 'Class Of Credit',
      ylab = 'Count Of People',
      fill = 'red'
      )

# Class By Credit Rating
# Distribution Of Duration Variable
hist(GermanCredit$Duration, 
     main = 'Distribution Of Duration',
     xlab = 'Duration Length in Months',
     ylab = 'Frequency', 
     col = 'red'
     )

# Hist of Credit Amount
hist(Amount, 
     main = 'Distrbution of Credit Amount', 
     xlab = 'Amount in 1000s', 
     ylab = 'Frequency',
     col = 'blue'
     )

# ANOVA Duration By Class
# Run an anova to see if class is differnce by Duration amount on average
aov1 <- aov(Duration ~ Class, data = GermanCredit)
summary(aov1)
plot(Duration ~ Class, 
     data = GermanCredit, 
     main = 'Duration By Class',
     xlab = 'Class',
     ylab = 'Duration In Months',
     col = c('red', 'blue')
     )

# ANOVA Credit Amount By Class
# Run an anova to see if class is different by Amount on average
aov2 <- aov(Amount ~ Class, data = GermanCredit)
summary(aov2)
plot(Amount ~ Class, 
     data = GermanCredit, 
     main = 'Credit Amount By Class', 
     col = c('Pink','Green'),
     xlab = 'Class',
     ylab = 'Amount Of Credit 1000s'
     )

###############################
##                           ##
##  2. Logistic Regression   ## 
##                           ##
###############################
# Create train and test splits
split <- round(nrow(GermanCredit) * .80)
endOfSplit <- split + 1
train <- GermanCredit[1:split, ]
test <- GermanCredit[endOfSplit:nrow(GermanCredit), ]

# Model to predict $Class
model <- glm(Class ~ ., family = 'binomial', data=train)
summary(model)

# Create A Subset of Variables Summaries that stores only coefficients
# the coefficients of the variables
model.stats <- summary(model)$coefficients
# Convert model.stats into a data frame
model.stats <- data.frame(model.stats)
# Round all the records in the data to 4 places
model.stats <- round(model.stats, 4)
# Select only significant variables



# Model Predictions
# uses the model 
predictions <- predict(model, test, type = 'response')

# Find Best Cutoff AUC
# AUC = Area under the curve it minimizes errors and maximizes
# true predictions
cutoff <- colAUC(predictions, test$Class, plotROC = T)
# add cutoff line from curve
abline(h = .797, col = 'green')


# View Confusion Matrix
# Convert Predictions to 1/0 
classes <- ifelse(predictions > .797, 1, 0)
# Convert Test class to 1/0, good = 1, bad = 0
test$Class <- ifelse(test$Class == 'Good', 1, 0)
table(classes, test$Class)

###############################
##                           ##
##         3. PVCLUST        ## 
##                           ##
###############################
# PVCLUST - builds clusters within your data in hierarchal manner
library(pvclust)
# Creates a cluster object that stores the analysis and
# allows you to plot the data in the next step
# since this is a 'wide' data set containing many predictors,
# I only included the first 9 columns to conserve computing
pvclust.model <- pvclust(GermanCredit[,1:9])

# Plot the cluster object that was juse created and customize
# The headings
plot(pvclust.model, main = 'German Credit Cluster')
