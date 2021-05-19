library(MASS)
attach(Boston)


#set seed as birthday - 3rd January
set.seed(103)

Boston_lstat <- data.frame(lstat)
#split the lstat dataset column in test and training dataset
sample_lstat <- sample(1:nrow(Boston_lstat), nrow(Boston_lstat)/2)
test_lstat.X <- Boston_lstat[-sample_lstat,]
train_lstat.X <- Boston_lstat[sample_lstat,]
test.Y <- medv[-sample_lstat]
train_lstat.Y <- medv[sample_lstat]

Boston_rm <- data.frame(rm)
#split the rm dataset column in test and training dataset
set.seed(103)
sample_rm <- sample(1:nrow(Boston_rm), nrow(Boston_rm)/2)
test_rm.X <- Boston_rm[-sample_rm,]
train_rm.X <- Boston_rm[sample_rm,]
train_rm.Y <- medv[sample_rm]

#creating Decision tree function
decisionTree <- function(x,y){
  s <- seq(min(x), max(x), by=0.1)
  count_rss <- 1
  rss_for_all_s <- rep(0,length(s))
  y_cap_lower <-0
  y_cap_upper <-0
  for(s_each in s){
    y_cap_lower <- mean(y[x<s_each])
    y_cap_upper <- mean(y[x>=s_each])
    y_cap_lower <- ifelse(is.nan(y_cap_lower), 0, y_cap_lower)
    y_cap_upper <- ifelse(is.nan(y_cap_upper), 0, y_cap_upper)
    
    lower_rss <- sum((y[x<s_each]-y_cap_lower)^2)
    upper_rss <- sum((y[x>=s_each]-y_cap_upper)^2)
    
    rss_for_all_s[count_rss] <- lower_rss + upper_rss
    count_rss <- count_rss+1
  }
  least_rss <- min(rss_for_all_s)
  index_least_rss <- rss_for_all_s == min(rss_for_all_s)
  s_for_least_rss <- s[index_least_rss][1]
  return(list('s_for_least_rss'=s_for_least_rss,'least_rss'=least_rss,'y_cap_lower'=y_cap_lower,'y_cap_upper'=y_cap_upper))
}

#calculating the Test RSS 
getTestRSS <- function(x_test,y_test,s,y_cap_lower,y_cap_upper){
  #print(x_test)
  lower_rss <- sum((y_test[x_test<s]-y_cap_lower)^2)
  upper_rss <- sum((y_test[x_test>=s]-y_cap_upper)^2)
  rss_for_s <- lower_rss + upper_rss
  return(rss_for_s)
}

#calling decisionTree() function over training data for lstat
s_and_rss_lstat <- decisionTree(train_lstat.X,train_lstat.Y)
s_value_lstat <- s_and_rss_lstat$s_for_least_rss
least_rss_lstat <- s_and_rss_lstat$least_rss
y_cap_lower_lstat <- mean(train_lstat.Y[train_lstat.X<s_value_lstat])
y_cap_upper_lstat <- mean(train_lstat.Y[train_lstat.X>=s_value_lstat])


#calling decisionTree() function over training data for rm
s_and_rss_rm <- decisionTree(train_rm.X,train_rm.Y)
s_value_rm <- s_and_rss_rm$s_for_least_rss
least_rss_rm <- s_and_rss_rm$least_rss
y_cap_lower_rm <- mean(train_rm.Y[train_rm.X<s_value_rm])
y_cap_upper_rm <- mean(train_rm.Y[train_rm.X>=s_value_rm])

#calling getTestRSS() function over test data for lstat
rss_for_test_lstat = getTestRSS(test_lstat.X,test.Y,s_value_lstat,y_cap_lower_lstat,y_cap_upper_lstat)

#calling getTestRSS() function over test data for rm
rss_for_test_rm = getTestRSS(test_rm.X,test.Y,s_value_rm,y_cap_lower_rm,y_cap_upper_rm)

#finding the min RSS among lstat and rm and calculating the Test MSE from there
if(rss_for_test_lstat>rss_for_test_rm){
  MSE = rss_for_test_rm/length(test.Y)
}else {
  MSE = rss_for_test_lstat/length(test.Y)
}


#calling getTestRSS() function over test data for lstat
rss_for_test_lstat = getTestRSS(test_lstat.X,test.Y,s_value_lstat,y_cap_lower_lstat,y_cap_upper_lstat)

#calling getTestRSS() function over test data for rm
rss_for_test_rm = getTestRSS(test_rm.X,test.Y,s_value_rm,y_cap_lower_rm,y_cap_upper_rm)

MSE_print <- sprintf("Task 1 : Test MSE for decision stumps on the test set: %f", MSE)
print(MSE_print)

#creating Boosting Decision Stump (BDS) function
boosting_decision_tree <- function(x_train_lstat,y_train,x_train_rm,B){
  r <- y_train
  s <- rep(0,B)
  all_values <- list()
  for(i in 1:B){
    #calling decisionTree function for lstat data
    s_and_rss_lstat <- decisionTree(x_train_lstat,r)
    s_for_least_rss_lstat <- s_and_rss_lstat$s_for_least_rss
    least_rss_lstat <- s_and_rss_lstat$least_rss
    y_cap_lower_lstat <-mean(y_train[x_train_lstat<s_for_least_rss_lstat])
    y_cap_upper_lstat <-mean(y_train[x_train_lstat>=s_for_least_rss_lstat])
    
    #calling decisionTree function for rm data
    s_and_rss_rm <- decisionTree(x_train_rm,r)
    least_rss_rm <-s_and_rss_rm$least_rss
    s_for_least_rss_rm <- s_and_rss_rm$s_for_least_rss
    y_cap_lower_rm <-mean(y_train[x_train_rm<s_for_least_rss_rm])
    y_cap_upper_rm <-mean(y_train[x_train_rm>=s_for_least_rss_rm])

    x_train <- rep(0,length(y_train))
    #finding the least RSS and coressponding values between lstat and rm
    if(least_rss_lstat>least_rss_rm){
      l <- list('label'='rm','s'=s_for_least_rss_rm,'y_low'=y_cap_lower_rm,'y_up'=y_cap_upper_rm)
      all_values <- cbind(all_values,l)
      x_train <- x_train_rm
      s <-s_for_least_rss_rm
    }
    else{
      l <- list('label'='lstat','s'=s_for_least_rss_lstat,'y_low'=y_cap_lower_lstat,'y_up'=y_cap_upper_lstat)
      all_values <- cbind(all_values,l)
      x_train <- x_train_lstat
      s <-s_for_least_rss_lstat
    }
    y_cap <- rep(0,length(y_train))
    y_cap[x_train>=s] <- mean(y_train[x_train>=s])
    y_cap[x_train<s] <- mean(y_train[x_train<s])
    r <- r-0.1*y_cap
  }
  return(all_values)
}

#calling boosting_decision_tree function to get a list of lists with label - lstat/rm, s, y_lower,y_upper for the selected attribute
all_values = boosting_decision_tree(train_lstat.X,train_lstat.Y,train_rm.X,1000)
#print(all_values)

#getTestRSSForBDS function to calculate Test RSS based on BDS returns all y_cap for each B tree
getTestRSSForBDS <- function(test_lstat.X,test_rm.X,y_test,all_values,B){
  m = matrix(0,nrow = B, ncol = length(y_test))
  for(i in 1:B){
    if(all_values[,i]$label=='rm'){
     s <- unlist(all_values[,i]$s)
     y_cap <- rep(0,length(y_test))
     y_cap[test_rm.X>=s] <- mean(train_rm.Y[train_rm.X>=s])
     y_cap[test_rm.X<s] <- mean(train_rm.Y[train_rm.X<s])
    }
    else{
      s <- unlist(all_values[,i]$s)
      y_cap <- rep(0,length(y_test))
      y_cap[test_lstat.X>=s] <- mean(train_lstat.Y[train_lstat.X>=s])
      y_cap[test_lstat.X<s] <- mean(train_lstat.Y[train_lstat.X<s])
    }
    m[i,] <- y_cap
  }
  m = m* 0.1
  return(m)
}
#calling getTestRSSForBDS to get the y_cap values
m <- getTestRSSForBDS(test_lstat.X,test_rm.X,test.Y,all_values,1000)

#calculating y_pred
for(i in 1:ncol(m)){
avg[i] <- mean(m[,i])
}
y_pred <- avg

#MSE for y_pred
MSE_BDS = (sum((y_pred - test.Y)^2))/length(test.Y)
MSE_BDS_print <- sprintf("Task 2 : Test MSE for boosted decision stumps (BDS) on the test set: %f", MSE_BDS)
print(MSE_BDS_print)

#plotMSE fuction to get an array of MSEs for a range of Stumps (B)
plotMSE <- function(train_lstat.Y,test_lstat.X,B){
  each_MSE_BDS <- rep(0,length(B))
  count <- 1
  for(i in 1:B){
    all_values = boosting_decision_tree(train_lstat.X,train_lstat.Y,train_rm.X,i)
    m_each = getTestRSSForBDS(test_lstat.X,test_rm.X,test.Y,all_values,i)
    for(j in 1:ncol(m_each)){
      avg[j] <- mean(m_each[,j])
    }
    y_pred <- avg
    each_MSE_BDS[count] = (sum((y_pred - test.Y)^2))/length(test.Y)
    count <- count+1
  }
  return(each_MSE_BDS)
}

all_MSE_BDS <- plotMSE(train_lstat.Y,test_lstat.X,50)

#Task 3: plotting all BDS Test MSEs
plot(all_MSE_BDS,type='l',xlab ="B", ylab = "Test MSE", main = "Progression of Test MSE with number of Stumps")