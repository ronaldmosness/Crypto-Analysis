library(tidyverse) #loading the Tidyverse package
library(broom)
library(psych)
library(Metrics)
library(glmnet)



##Importing the 'stock price' dataset
stock_price <- read.csv("stock price.csv", header = TRUE, sep = ",")
str(stock_price)
View(stock_price)
nrow(stock_price)


##Importing the 'bitcoin' dataset
bitcoin <- read.csv("bitcoin.csv", header = TRUE, sep = ",")
str(bitcoin)
View(bitcoin)
nrow(bitcoin)

##Checking for missing values
sum(is.na(stock_price))
sum(is.na(bitcoin))

##Checking for duplicates
sum(duplicated(bitcoin))
sum(duplicated(stock_price))

##Computing a new variable: avg_price which is the mean of Open and Closing price for each day
bitcoin <- bitcoin %>% mutate(avg_price = (Open + Close)/2)
head(bitcoin)
head(stock_price)

##merging the two tables using the 'Date' column
final_table <- bitcoin %>% inner_join(stock_price, by = "Date")
head(final_table)
View(final_table)
nrow(final_table)

##Summary statistics of the numeric variables
summary(final_table$Volume)
summary(final_table$Marketcap)
summary(final_table$avg_price)
summary(final_table$S.P500)


##Exploratory visualization of the variables of interest
ggplot(final_table, aes(S.P500)) + geom_histogram()
ggplot(final_table, aes(Volume)) + geom_density()
ggplot(final_table, aes(Marketcap)) + geom_histogram(bins = 10)
ggplot(final_table, aes(avg_price)) + geom_density()

boxplot(final_table$S.P500)
boxplot(final_table$avg_price)
boxplot(final_table$Marketcap)
boxplot(final_table$Volume)

##Bivariate visualization with logarithmic transformation for linear plot
ggplot(final_table, aes(avg_price, S.P500)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(final_table, aes(log(avg_price), S.P500)) + geom_point() + geom_smooth(method = "lm", se = FALSE) 
ggplot(final_table, aes(Close, S.P500)) + geom_point()
ggplot(final_table, aes(log(Close), S.P500)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

## Splitting the data into training, validation and testing using 70%, 15%, 15%
set.seed(200)
sample_train <- sample(nrow(final_table), size = floor(0.70*nrow(final_table)), replace = FALSE)
sample_valid <- sample(nrow(final_table), size = floor(0.15*nrow(final_table)), replace = FALSE)
sample_test <- sample(nrow(final_table), size = floor(0.15*nrow(final_table)), replace = FALSE)

train_set <- final_table[sample_train, ]
valid_set <- final_table[sample_valid, ]
test_set <- final_table[sample_test, ]

nrow(train_set)
nrow(valid_set)
nrow(test_set)

##Correlation matrix to know the variables to put in the model
cor(final_table[c("Close", "Open", "Volume", "Marketcap", "avg_price", "S.P500")])

##Visualizing the relationship among features - scatter plot matrix
pairs.panels(final_table[c("Close", "Open", "Volume", "Marketcap", "avg_price", "S.P500")])


##fitting the regression model
model <- lm(log(Close) ~ S.P500 + Volume + log(Marketcap), train_set)
model
summary(model)


##Making predictions with the model on the training set
pred_train <- predict(model, train_set)
##Adding the column to the data set while also reverting the logarithmic transformation
train_set <-  train_set %>% mutate(pred_bitcoin_close = exp(pred_train))
head(train_set)

##Making predictions with the model on the testing set
pred_test <- predict(model, test_set)
##Adding the column to the data set while also reverting the logarithmic transformation
test_set <-  test_set %>% mutate(pred_bitcoin_close = exp(pred_test))
head(test_set)


###Evaluation metrics for prediction on the test_set and the train_set
mae(test_set$Close, test_set$pred_bitcoin_close)
mse(test_set$Close, test_set$pred_bitcoin_close)
rmse(test_set$Close, test_set$pred_bitcoin_close)


mae(train_set$Close, train_set$pred_bitcoin_close)
mse(train_set$Close, train_set$pred_bitcoin_close)
rmse(train_set$Close, train_set$pred_bitcoin_close)


##Ridge regression
y <- final_table$Close
x <- data.matrix(final_table[, c("Volume", "Marketcap", "S.P500")])

##model fitting
model_ridge <- glmnet(x, y, alpha = 0)
summary(model_ridge)

##cross validation
cv_model_ridge <- cv.glmnet(x, y, alpha = 0)

best_lambda <- cv_model_ridge$lambda.min
best_lambda
plot(cv_model_ridge)

best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)

y_pred <- predict(model_ridge, s = best_lambda, x)
mae(y, y_pred)
mse(y, y_pred)
rmse(y, y_pred)


##Lasso regression
model_ridge <- glmnet(x, y, alpha = 1)
summary(model_ridge)

cv_model_ridge <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model_ridge$lambda.min
best_lambda
plot(cv_model_ridge)

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

y_pred <- predict(model_ridge, s = best_lambda, x)
mae(y, y_pred)
mse(y, y_pred)
rmse(y, y_pred)




