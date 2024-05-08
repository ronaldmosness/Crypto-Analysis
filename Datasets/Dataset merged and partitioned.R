library(tidyverse) #loading the Tidyverse package


##Importing the 'stock price' dataset
stock_price <- read.csv("stock price.csv", header = TRUE, sep = ",")
str(stock_price)

##Importing the 'bitcoin' dataset
bitcoin <- read.csv("bitcoin.csv", header = TRUE, sep = ",")
str(bitcoin)

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
nrow(final_table)


##Converting the column 'Date' from character to date data type 
final_table <- mutate(final_table, Date = as.Date(Date, format = "%m/%d/%y"))
str(final_table)
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


## Splitting the data into training, validation and testing using 70%, 15%, 15%
set.seed(200)
sample_train <- sample(nrow(final_table), size = floor(0.70*nrow(final_table)), replace = FALSE)
sample_valid <- sample(nrow(final_table), size = floor(0.15*nrow(final_table)), replace = FALSE)
sample_test <- sample(nrow(final_table), size = floor(0.15*nrow(final_table)), replace = FALSE)

train_set <- final_table[sample_train, ]
valid_set <- final_table[sample_valid, ]
test_set <- final_table[sample_test, ]

nrow(train)
nrow(valid)
nrow(test)



