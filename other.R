#creating sigmoid function
sigmoid <- function(x){
  return (1/(1+exp(-x)))
}

#creating MSE function
MSE <- function(y_pred,y){
  return(mean((y_pred-y)^2))
}

forwardPropogation <- function(w,x,b){
  addition_wx = rowSums(x%*%w)
  with_bias = addition_wx+b
  y_pred = sigmoid(with_bias)
  return (y_pred)
}

#creating gradient descent function
gradientDescent <- function(x_train,y_train,x_test,y_test,learning_rate,no_of_steps){
  #TASK 4 
  #independent random numbers in the range [−0.7, 0.7] as the initial weights
  w = runif(ncol(x_train), min=-0.7, max=0.7)
  b = runif(1,min=-0.7, max=0.7)
  each_MSE_train = rep(0,no_of_steps)
  each_MSE_test = rep(0,no_of_steps)
  for(i in 1:no_of_steps){
    y_pred <- forwardPropogation(w,x_train,b)
    delta_w <- -2*colMeans(x_train*(y_pred-y_train))
    delta_b <- -2*mean(y_pred-y_train)
    w <- w + learning_rate*delta_w
    b <- b + learning_rate*delta_b
    #Find the MSE on the test set
    y_pred_test <- pred(w,x_test,b)
    each_MSE_test[i] <- MSE(y_pred_test,y_test)
  }
  plot(each_MSE_test,type='l',xlab ="Training Steps", ylab = "Test MSE", main = "Progression of Test MSE")
  return (list('weight'=w,'bias'=b))
}

#predicting the label
pred <- function(w,x,b){
  y_pred <- forwardPropogation(w,x,b)
  for(i in 1:length(y_pred)){
    if(y_pred[i]<0.5){
      y_pred[i] <- 0
    }
    else{
      y_pred[i] <- 1
    }
  }
  return (y_pred)
}



