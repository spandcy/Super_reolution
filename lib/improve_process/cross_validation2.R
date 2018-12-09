########################
### Cross Validation ###
########################

### Author: Chengliang Tang
### Project 3

cv.function <- function(X.train, y.train, d, K){
  
  n <- dim(y.train)[1]
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X.train[s != i, ,]
    train.label <- y.train[s != i, ,]
    test.data <- X.train[s == i, ,]
    test.label <- y.train[s == i, ,]
    
    par <- list(depth=d)
    fit <- train(train.data, train.label, par)
    pred <- test(fit, test.data)  
    cv.error[i] <- mean((pred - test.label)^2)  
    
  }			
  return(c(mean(cv.error),sd(cv.error)))
}

######XGB########
xgb_cv <- function(data.train, label.train, max_depth = 6, eta = 0.3, 
                   nrounds = 100, gamma = 0, nthread = 2, subsample = 0.5,
                   objective = "multi:softprob", num_class = 3, nfold=5){
  K <- nfold
  n <- length(label.train)
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- data.train[s != i,]
    train.label <- label.train[s != i]
    test.data <- data.train[s == i,]
    test.label <- label.train[s == i]
    
    par <- list(max_depth = max_depth, eta = eta)
    dtrain = xgb.DMatrix(data=data.matrix(train.data),label=train.label)
    fit <- xgb.train(data = dtrain, params = par, nrounds = nrounds, 
                     gamma = gamma, nthread = nthread, subsample = subsample,
                     objective = objective, num_class = num_class)
    pred <- predict(fit, as.matrix(test.data))
    pred <- matrix(pred, ncol=3, byrow=TRUE)
    pred_labels <- max.col(pred) - 1
    cv.error[i] <- mean(pred_labels != test.label)  
    print(cv.error[i])
  }			
  return(cv.error)
}