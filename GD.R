source('Downloads/other.R')
library(ISLR)
attach(Auto)

#TASK 2 
#Create a new variable high 
high = rep(0,nrow(Auto))
high <- ifelse(mpg>=23,1,0)

#Create appropriate dummy variables
dummy_origin1 <- rep(0,nrow(Auto))
dummy_origin2 <- rep(0,nrow(Auto))
dummy_origin1[origin==1] <- 1
dummy_origin2[origin==2] <- 2
Auto_new <- data.frame(horsepower, weight, year, dummy_origin1, dummy_origin2)

#normalize the attributes
Auto_new <- scale(Auto_new)

#TASK 3
#set seed as birthday - 3rd January
set.seed(103)

#split the dataset in test and training dataset
sample <- sample(1:nrow(Auto_new), nrow(Auto_new)/2)
train.X <- Auto_new[-sample,]
test.X <- Auto_new[sample,]
train.Y <- high[-sample]
test.Y <- high[sample]

#TASK 4
#Try different values of the learning rate η and of the number of training steps
# Case 1, η = 0.0001, training steps : 1500
w_and_b <- gradientDescent(train.X,train.Y,test.X,test.Y,0.0001,1500)

#prediction for train and test samples
y_pred_train <- pred(w_and_b$weight,train.X,w_and_b$bias)
y_pred_test <- pred(w_and_b$weight,test.X,w_and_b$bias)

#calculating MSE for train and test data
MSE_train <- MSE(y_pred_train,train.Y)
MSE_test <- MSE(y_pred_test,test.Y)

print("CASE 1")
output_train <- sprintf("Train MSE for 0.0001 η and 1500 training steps: %f", MSE_train)
print(output_train)
output_test <- sprintf("Test MSE for 0.0001 η and 1500 training steps: %f", MSE_test)
print(output_test)

# Case 2, η = 0.001, training steps : 1000
w_and_b <- gradientDescent(train.X,train.Y,test.X,test.Y,0.001,1000)

#prediction for train and test samples
y_pred_train <- pred(w_and_b$weight,train.X,w_and_b$bias)
y_pred_test <- pred(w_and_b$weight,test.X,w_and_b$bias)

#calculating MSE for train and test data
MSE_train <- MSE(y_pred_train,train.Y)
MSE_test <- MSE(y_pred_test,test.Y)

print("CASE 2")
output_train <- sprintf("Train MSE for 0.001 η and 1000 training steps: %f", MSE_train)
print(output_train)
output_test <- sprintf("Test MSE for 0.001 η and 1000 training steps: %f", MSE_test)
print(output_test)

# Case 3, η = 0.01, training steps : 500
w_and_b <- gradientDescent(train.X,train.Y,test.X,test.Y,0.01,500)

#prediction for train and test samples
y_pred_train <- pred(w_and_b$weight,train.X,w_and_b$bias)
y_pred_test <- pred(w_and_b$weight,test.X,w_and_b$bias)

#calculating MSE for train and test data
MSE_train <- MSE(y_pred_train,train.Y)
MSE_test <- MSE(y_pred_test,test.Y)

print("CASE 3")
output_train <- sprintf("Train MSE for 0.01 η and 500 training steps: %f", MSE_train)
print(output_train)
output_test <- sprintf("Test MSE for 0.01 η and 500 training steps: %f", MSE_test)
print(output_test)

#TASK 6
# Case 4, η = 0.1, training steps : 100
w_and_b <- gradientDescent(train.X,train.Y,test.X,test.Y,0.1,100)

#prediction for train and test samples
y_pred_train <- pred(w_and_b$weight,train.X,w_and_b$bias)
y_pred_test <- pred(w_and_b$weight,test.X,w_and_b$bias)

#calculating MSE for train and test data
MSE_train <- MSE(y_pred_train,train.Y)
MSE_test <- MSE(y_pred_test,test.Y)

print("CASE 4")
output_train <- sprintf("Train MSE for 0.1 η and 100 training steps: %f", MSE_train)
print(output_train)
output_test <- sprintf("Test MSE for 0.1 η and 100 training steps: %f", MSE_test)
print(output_test)



