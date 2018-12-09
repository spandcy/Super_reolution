#########################################################
### Train a classification model with training features ###
#########################################################

### Author: Chengliang Tang
### Project 3

######GBM########
train <- function(dat_train, label_train, par=NULL){
  
  library("gbm")
  
  ### creat model list
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 1
    
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
                       n.trees = 100, 
                       distribution="gaussian",
                       n.minobsinnode = 14,
                       interaction.depth=depth,
                       keep.data = TRUE,
                       bag.fraction = 0.5,
                       shrinkage = .09,
                       verbose=FALSE)
    best_iter <- gbm.perf(fit_gbm, method="OOB", plot.it = FALSE)
    modelList[[i]] <- list(fit=fit_gbm, iter=best_iter)
  }
  
  return(modelList)
}


########XGB########
xgb_train <- function(dat_train, label_train, tune = FALSE) {
  library(xgboost)
  
  modelList <- list()
  
  for (i in 1:12){
    ## calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_train[, , c2]
    labMat <- label_train[, c1, c2]
   
    ############
    xgbFit = xgboost(data = data.matrix(featMat),
            label = as.numeric(labMat) ,
            booster = "gblinear",
            nrounds = 150,
            max_depth = 7,
            lambda = 0.5,
            eta = .5,
            gamma = .1,
            colsample_bytree = 0.4603,
            early_stopping_rounds = 30,
            min_child_weight = 6.5,
            subsample = .6,
            eval_metric = "rmse",
            alpha = 0.5,
            objective = "reg:linear")
    
    # paras<-list(objective="reg:linear",
    #             eta=0.5, 
    #             nthread = 2,
    #             max.depth=8)
    # xgbFit <- xgboost(data = data.matrix(featMat), label = as.numeric(labMat),    
    #                    nround = 14, 
    #                    params=paras)

    modelList[[i]] <- list(fit=xgbFit)
  }
 
  return(modelList)
}

