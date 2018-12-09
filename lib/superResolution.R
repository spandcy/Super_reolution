########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3

superResolution_gbm <- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  PSNR <- NULL
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    LR_nrow <- nrow(imgLR)
    LR_ncol <- ncol(imgLR)
    num_ele <- LR_nrow * LR_ncol
    featMat <- array(NA, c(num_ele, 8, 3))
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    sample_row <- (1 : num_ele - 1) %% LR_nrow + 1
    sample_col <- (1 : num_ele - 1) %/% LR_nrow + 1
    
    for (k in c(1:3)) {
      # supplementary image matrix
      supp_imgLR <- cbind(imgLR[, 1, k], imgLR[, , k], imgLR[, LR_ncol, k])
      supp_imgLR <- rbind(cbind(imgLR[1, 1, k], imgLR[1, , k],imgLR[1, LR_ncol, k]), 
                          supp_imgLR, cbind(imgLR[LR_nrow, 1, k], imgLR[LR_nrow, , k],imgLR[LR_nrow, LR_ncol, k]))
      center=supp_imgLR[cbind(sample_row+1, sample_col+1)]
     
      ### fill the featM 
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row,sample_col)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row,sample_col + 1)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row,sample_col + 2)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row + 1,sample_col)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row + 1,sample_col + 2)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row + 2,sample_col)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row + 2,sample_col + 1)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row + 2,sample_col + 2)] - center
    }
    
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)
    
    ### step 3. recover high-resolution from predMat and save in HR_dir
    predMat[, , 1] <- predMat[, , 1] + imgLR[, ,1][cbind(sample_row,sample_col)]
    predMat[, , 2] <- predMat[, , 2] + imgLR[, ,2][cbind(sample_row,sample_col)]
    predMat[, , 3] <- predMat[, , 3] + imgLR[, ,3][cbind(sample_row,sample_col)]
    
    imgHR_fit <- array(0, c(LR_nrow*2, LR_ncol*2, 3))
    imgHR_fit <- Image(imgHR_fit, colormode = Color)
    
    base_row <- seq(1, 2 * LR_nrow, 2)
    base_col <- seq(1, 2 * LR_ncol, 2)
    imgHR_fit[base_row, base_col, ] <- predMat[, 1, ]
    imgHR_fit[base_row, base_col + 1, ] <- predMat[, 2, ]
    imgHR_fit[base_row + 1, base_col, ] <- predMat[, 3, ]
    imgHR_fit[base_row + 1, base_col + 1, ] <- predMat[, 4, ]
    # calculate MSE and PSNR
    mse <- sum((imgHR - imgHR_fit)^2)/(3*num_ele)
    psnr <- 20*log10(range(imgHR)[2]) - 10*log10(mse)
    PSNR <- append(PSNR, psnr)
    #setwd(save_path)
    writeImage(imgHR_fit, paste0("../data/test_set/SR/","img_gbm_fit_", sprintf("%04d", i), ".jpeg"))
  }
  PSNR <- sum(PSNR)/n_files
  return(PSNR)
}



superResolution_xgboost <- function(LR_dir,HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  PSNR <- NULL
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    #pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    LR_nrow <- nrow(imgLR)
    LR_ncol <- ncol(imgLR)
    num_ele <- LR_nrow * LR_ncol
    featMat <- array(NA, c(num_ele, 8, 3))
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    sample_row <- (1 : num_ele - 1) %% LR_nrow + 1
    sample_col <- (1 : num_ele - 1) %/% LR_nrow + 1
    
    for (k in c(1:3)) {
      # supplementary image matrix
      supp_imgLR <- cbind(imgLR[, 1, k], imgLR[, , k], imgLR[, LR_ncol, k])
      supp_imgLR <- rbind(cbind(imgLR[1, 1, k], imgLR[1, , k],imgLR[1, LR_ncol, k]), 
                          supp_imgLR, cbind(imgLR[LR_nrow, 1, k], imgLR[LR_nrow, , k],imgLR[LR_nrow, LR_ncol, k]))
      center=supp_imgLR[cbind(sample_row+1, sample_col+1)]
      
      ### fill the featM 
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row,sample_col)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row,sample_col + 1)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row,sample_col + 2)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row + 1,sample_col)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row + 1,sample_col + 2)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row + 2,sample_col)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row + 2,sample_col + 1)] - center
      featMat[,  1, k] <- supp_imgLR[cbind(sample_row + 2,sample_col + 2)] - center
    }
    
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)
    
    ### step 3. recover high-resolution from predMat and save in HR_dir
    predMat[, , 1] <- predMat[, , 1] + imgLR[, ,1][cbind(sample_row,sample_col)]
    predMat[, , 2] <- predMat[, , 2] + imgLR[, ,2][cbind(sample_row,sample_col)]
    predMat[, , 3] <- predMat[, , 3] + imgLR[, ,3][cbind(sample_row,sample_col)]
    
    imgHR_fit <- array(0, c(LR_nrow*2, LR_ncol*2, 3))
    imgHR_fit <- Image(imgHR_fit, colormode = Color)
    
    base_row <- seq(1, 2 * LR_nrow, 2)
    base_col <- seq(1, 2 * LR_ncol, 2)
    imgHR_fit[base_row, base_col, ] <- predMat[, 1, ]
    imgHR_fit[base_row, base_col + 1, ] <- predMat[, 2, ]
    imgHR_fit[base_row + 1, base_col, ] <- predMat[, 3, ]
    imgHR_fit[base_row + 1, base_col + 1, ] <- predMat[, 4, ]
    #calculate MSE and PSNR
   #mse <- sum((imgHR - imgHR_fit)^2)/(3*num_ele)
    mse <- mean((imgHR - imgHR_fit)^2)
    psnr <- 20*log10(1) - 10*log10(mse)
    PSNR <- append(PSNR, psnr)
    #setwd(save_path)
    writeImage(imgHR_fit, paste0("../data/test_set/SR/","img_xgboost_fit_", sprintf("%04d", i), ".jpeg"))
  }
  PSNR <- sum(PSNR)/n_files
 return(PSNR)
}










