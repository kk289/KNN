library(ISLR)
#Creating a data frame of the Iris Dataset
df <- iris

#Prinint the variance of each column
print(var(df[,1]))
print(var(df[,2]))
print(var(df[,3]))
print(var(df[,4]))

#Scaling all the 4 attributes
scaled.df <- scale(df[1:4])

#Printing the variance after scaling 
print(var(scaled.df[,1]))
print(var(scaled.df[,2]))
print(var(scaled.df[,3]))
print(var(scaled.df[,4]))

#Binding the class attribute to the scaled data frame
iris_df <- cbind(scaled.df, df[5])

#Importing caTools to split the data and setting random seed
library(caTools)
set.seed(101)

#Spliting the data on the class variable with a 0.7 ratio
#and storing it in train and test 
sample <- sample.split(iris_df$Species, SplitRatio = 0.7)
train <- subset(iris_df, sample == T)
test <- subset(iris_df, sample == F)

library(class)

#Applying knn to the data and storing the predicted value
predicted.species <- knn(train[1:4], test[1:4], train[,5], k = 1)
print(predicted.species)

#Checking the prediction error rate
print(mean(test$Species != predicted.species))

#Choosing the correct value for k

predicted.value <- NULL
error.rate <- NULL

for (i in 1:10){
  set.seed(101)
  predicted.value <- knn(train[1:4], test[1:4], train$Species, k = i)
  error.rate[i] <- mean(predicted.value != test$Species) 
}
print(error.rate)

#Plotting the values of k vs error rates (elbow method)
library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate, k.values)
pl <- ggplot(error.df, aes(x = k.values, y = error.rate)) + geom_point()+ geom_line(lty = 2, color = 'green')
print(pl)