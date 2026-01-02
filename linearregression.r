#Multiple Linear Regression
#Dataset_name: data_8
#Student Number ID:x24207438
#installed and using all libraries
library(readr)
library(caret) 
library(Metrics)
library(psych)
library(lmtest)
library(car)

# loading my dataset
data_8 <- read_csv("C:/Users/admin/Desktop/mlr_data/mlr8.csv")
View(data_8)

#Fixing  types
data_8$y  <- as.numeric(data_8$y)
data_8$x1 <- as.numeric(data_8$x1)
data_8$x2 <- as.numeric(data_8$x2)
data_8$x3 <- as.factor(data_8$x3)
# returns min, max, mean, median, 1st and 3rd quartiles
summary(data_8)
# std,mean, median, skewness, kurtosis, variance, min, max(describing more about data8)
describe(data_8)

# checking if data has any missing values
sum(is.na(data_8))

#checking missing values column wise
colSums(is.na(data_8))

# seed makes the train test split same everytime
set.seed(24207438)

# tells us how many rows are there in my dataset(data8)
n <- nrow(data_8)

# spliting data into 80 and 20. 80 for training and 20 for testing
training_size <- floor(0.8 * n)

# random picking rows for training data
index <- sample(seq_len(n), size = training_size)

train <- data_8[index, ]
test  <- data_8[-index, ]

#quick check sizes for train n test data
nrow(train)
nrow(test)

View(train)
View(test)



# plotting Histogram for y, x1 and x2
par(mfrow=c(1,3))
hist(data_8$y, col = "orange", main="Histogram of y", ylab="y")
hist(data_8$x1, col = "red", main="Histogram of x1", ylab="x1")
hist(data_8$x2, col = "skyblue", main="Histogram of x2", ylab="x2")

# plotting boxplot for y, x1 and x2
par(mfrow=c(1,4))
boxplot(data_8$y,  col="orange",  main="Boxplot of y",  ylab="y")
boxplot(data_8$x1, col="red",     main="Boxplot of x1", ylab="x1")
boxplot(data_8$x2, col="skyblue", main="Boxplot of x2", ylab="x2")
boxplot(data_8$y ~ data_8$x3, col="yellow", main="Boxplot of x3", ylab="y", xlab = "x3")


# plotting scatterplot
par(mfrow=c(1,2))
plot(data_8$x1, data_8$y, main="y vs x1", xlab="x1", ylab="y", col="green")
plot(data_8$x2, data_8$y, main="y vs x2", xlab="x2", ylab="y", col="purple")
par(mfrow=c(1,1))

#plotting pairplot
pairs(data_8[, c("y", "x1", "x2")],
      main = "Pair Plot of y, x1, and x2",
      col = "blue")

# Correlation between y, x1 and x2
cor(data_8[,c("y","x1","x2")])


model <- lm(y ~ x1 + x2 + x3, data = train)

# returns summary of model
summary(model)

# Predictions
pred <- predict(model, newdata = test)



# calculating prediction error 
rmse <- rmse(test$y, pred)
rmse

# calculating average error
mae  <- mae(test$y, pred)
mae

# r^2 on test data
res_sumsq <- sum((test$y - pred)^2)
total_sumsq <- sum((test$y - mean(test$y))^2)        
r2 <- 1 - res_sumsq / total_sumsq 
r2

#plot predicted vs actual
plot(test$y, pred, main = "Predicted vs Actual (test)", 
     xlab = "Actual y", 
     ylab = "Predicted y")
abline(a=0, b=1, col="red")

#vif
vif(model)


#dw test
dwtest(model)


# Normal Q–Q Plot for Model 
qqnorm(rstandard(model),
       main = "Normal Q-Q Plot of Standardized Residuals")
qqline(rstandard(model), col = "blue", lwd = 2)

# Cook's Distance Plot for Model
plot(cooks.distance(model), type = "h",
     main = "Cook's Distance for Model",
     ylab = "Cook's Distance")

abline(h = 1, col = "red", lwd = 2)   # threshold line

# Residuals vs Leverage
plot(model, which = 5)

# Residuals vs Fitted values should show random scatter.
plot(model$fitted.values,
     rstandard(model),
     main="Residuals vs Fitted",
     xlab="Fitted Values",
     ylab="Standardized Residuals")
abline(h=0, col="red")
