#########################################################
### Train a classification model with training features ###
#########################################################

### Author: Chengliang Tang
### Project 3

######GBM########
train_gbm <- function(dat_train, label_train, par=NULL){
  
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  features from LR images 
  ###  -  responses from HR images
  ### Output: a list for trained models
  
  ### load libraries
  library("gbm")
  
  ### creat model list
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 11
  } else {
    depth <- par$depth
  }
  
  ### the dimension of response arrat is * x 4 x 3, which requires 12 classifiers
  ### this part can be parallelized
  for (i in 1:12){
    ## calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-1) %/% 4 + 1
    featMat <- dat_train[, , c2]
    labMat <- label_train[, c1, c2]
    fit_gbm <- gbm.fit(x=featMat, y=labMat,
                       n.trees = 200, # 200
                       shrinkage = 0.1,
                       distribution="gaussian",
                       interaction.depth=depth, 
                       bag.fraction = 0.5,
                       verbose=FALSE)
    best_iter <- gbm.perf(fit_gbm, method="OOB", plot.it = FALSE)
    modelList[[i]] <- list(fit=fit_gbm, iter=best_iter)
  }
  
  return(modelList)
}


########XGB########
xgb_train <- function(dat_train, label_train, tune = FALSE) {
  library(xgboost)
  
  if (tune) {
    params <- xgb_para(dat_train = dat_train, label_train = label_train, K = 5, nround = 200)
    
    param.round <- xgb.set.M(dat_train = dat_train, label_train = label_train, M.range = c(80, 200), 
                             max_depth = params[[2]]$max_depth, eta = params[[2]]$eta, 
                             step = 10, K = 5)
    
    best_para<-list(max_depth = params[[2]]$max_depth, eta = params[[2]]$eta, nrounds = param.round[[1]], 
                    gamma = 0, nthread = 2, subsample = 0.5,
                    objective = "multi:softprob", num_class = 3)
  } else {
    best_para<-list(booster = 'gblinear',
                    objective = "reg:linear", eval_metric = 'RMSE', nrounds = 100)
    
  }
  
  modelList <- list()
  
  for (i in 1:12){
    ## calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_train[, , c2]
    labMat <- label_train[, c1, c2]
    
    # xgbst.train <- xgb.DMatrix(data = data.matrix(featMat), label = labMat)
    # fit_xgb <- xgboost(data = xgbst.train, params = best_para, nrounds = best_para$nrounds)
    xgbFit = xgboost(data = as.matrix(featMat), nfold = 5, label = as.matrix(labMat), 
                     nrounds = 100, objective = "reg:linear", eval_metric = "rmse", 
                      eta = 0.15, gamma = 0.1, max_depth = 4, min_child_weight = 1.7817, 
                     subsample = 0.6, colsample_bytree = 0.4603, lambda=0.5, alpha=0.5, early_stopping_rounds = 30,min_child_weight=6.5)
    
    modelList[[i]] <- list(fit=xgbFit)
  }
  
  return(modelList)
}
