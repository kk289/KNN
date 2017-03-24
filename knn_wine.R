library(ISLR)
#load the data
df <- read.csv('winequality-red.csv', sep = ';')
print(head(df))

#Scaling the attributes
scaled_df <- scale(df[1:11])
print(head(scaled_df))


#Checking for missing values
missing.values <- any(is.na(df))
print(missing.values)

#Creating a data frame with the scaled attributes and class attribute
df_wine <- cbind(scaled_df, df[12])

library(caTools)
set.seed(101)

sample <- sample.split(df_wine$quality, SplitRatio = 0.6)
train <- subset(df_wine, sample == T)
test <- subset(df_wine, sample == F)
library(class)

#Applying knn to the data and storing the predicted value
predicted.quality <- knn(train[1:11], test[1:11], train[,12], k = 1)
print(predicted.quality)

#Checking the prediction error rate
print(mean(test$quality != predicted.quality))

#Choosing the correct value for k

predicted.value <- NULL
error.rate <- NULL

for (i in 1:10){
  set.seed(101)
  predicted.value <- knn(train[1:4], test[1:4], train$quality, k = i)
  error.rate[i] <- mean(predicted.value != test$quality) 
}
print(error.rate)

#Plotting the values of k vs error rates (elbow method)
library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate, k.values)
pl <- ggplot(error.df, aes(x = k.values, y = error.rate)) + geom_point()+ geom_line(lty = 1, color = 'red')
print(pl)
