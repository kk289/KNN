library(ISLR)
df <- Caravan

purchase <- df[ ,86]

standardized.df <- scale(df[ ,-86])

#print(var(standardized.df[,1]))
#print(var(standardized.df[,2]))

index <- 1:1000

test.data <- standardized.df[index, ]
test.purchase <- purchase[index]

train.data <- standardized.df[-index, ]
train.purchase <- purchase[-index]

library(class)
set.seed(101)

predicted.purchase <- knn(train.data, test.data, train.purchase, k = 3)
print(head(predicted.purchase))

misclass.error <- mean(test.purchase != predicted.purchase)
print(misclass.error)


pred.purchase <- NULL
error.rate <- NULL

for (i in 1:20){
  set.seed(101)
  pred.purchase <- knn(train.data, test.data, train.purchase, k = i)
  error.rate[i] <- mean(test.purchase != pred.purchase)
}
print(error.rate)

library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate, k.values)
pl <- ggplot(error.df, aes(x = k.values, y = error.rate)) + geom_point()+ geom_line(lty = 1, color = 'blue')
print(pl)

#The right value would be k = 9

