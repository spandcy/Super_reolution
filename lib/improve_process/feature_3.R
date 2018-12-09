###Default###
feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  colnames(featMat) <- c("v1","v2","v3","v4","v5","v6","v7","v8")
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  colnames(labMat) <- c("v1","v2","v3","v4")
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgLR <- as.array(imgLR)
    imgHR = as.array(imgHR)
    ### step 1. sample n_points from imgLR
    dimLR=dim(imgLR)
    select=sample(dimLR[1]*dimLR[2],n_points)
    select_row=(select-1)%%dimLR[1]+1
    select_col=(select-1)%/%dimLR[1]+1
    #summary(selectLR)
    ### step 2. for each sampled point in imgLR,
    ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    for(j in 1:3){
      pad=cbind(0,imgLR[,,j],0)
      pad=rbind(0,pad,0)
      center=pad[cbind(select_row+1,select_col+1)]
      featMat[(i-1)*n_points+1:n_points,1,j]=pad[cbind(select_row,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,2,j]=pad[cbind(select_row,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,3,j]=pad[cbind(select_row,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,4,j]=pad[cbind(select_row+1,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,5,j]=pad[cbind(select_row+1,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,6,j]=pad[cbind(select_row+2,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,7,j]=pad[cbind(select_row+2,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,8,j]=pad[cbind(select_row+2,select_col+2)]-center
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
      channelHR=imgHR[,,j]
      
      labMat[(i-1)*n_points+1:n_points,1,j]=channelHR[cbind(select_row*2-1,select_col*2-1)]-center
      labMat[(i-1)*n_points+1:n_points,2,j]=channelHR[cbind(select_row*2-1,select_col*2)]-center
      labMat[(i-1)*n_points+1:n_points,3,j]=channelHR[cbind(select_row*2,select_col*2-1)]-center
      labMat[(i-1)*n_points+1:n_points,4,j]=channelHR[cbind(select_row*2,select_col*2)]-center
      ### step 3. repeat above for three channels
    }
  }
  return(list(feature = featMat, label = labMat))
}

###Canny###
feature_canny <- function(LR_dir, HR_dir, n_points=1000){
  library("EBImage")
  library("imager")
  n_files <- length(list.files(LR_dir))
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  colnames(featMat) <- c("v1","v2","v3","v4","v5","v6","v7","v8")
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  colnames(labMat) <- c("v1","v2","v3","v4")
  
  
  for (i in 1:n_files){
  imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
  imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
  imgHR <- as.array(imgHR)
  
  imgLR <- imgLR %>% as.cimg %>% cannyEdges %>% as.cimg
  dimLR <- dim(imgLR)
  
  
  s1 <- sample(which(imgLR[,,1]==1),n_points*0.6,replace=T)
  s2 <- sample(which(imgLR[,,1]==0),n_points*0.4,replace=T)
  select <- c(s1,s2)
 
  select_row=(select-1)%%dimLR[1]+1
  select_col=(select-1)%/%dimLR[1]+1
  
  imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
  imgLR <- as.array(imgLR)
  
  for(j in 1:3){
    pad=cbind(0,imgLR[,,j],0)
    pad=rbind(0,pad,0)
    center=pad[cbind(select_row+1,select_col+1)]
    featMat[(i-1)*n_points+1:n_points,1,j]=pad[cbind(select_row,select_col)]-center
    featMat[(i-1)*n_points+1:n_points,2,j]=pad[cbind(select_row,select_col+1)]-center
    featMat[(i-1)*n_points+1:n_points,3,j]=pad[cbind(select_row,select_col+2)]-center
    featMat[(i-1)*n_points+1:n_points,4,j]=pad[cbind(select_row+1,select_col)]-center
    featMat[(i-1)*n_points+1:n_points,5,j]=pad[cbind(select_row+1,select_col+2)]-center
    featMat[(i-1)*n_points+1:n_points,6,j]=pad[cbind(select_row+2,select_col)]-center
    featMat[(i-1)*n_points+1:n_points,7,j]=pad[cbind(select_row+2,select_col+1)]-center
    featMat[(i-1)*n_points+1:n_points,8,j]=pad[cbind(select_row+2,select_col+2)]-center
    ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
    channelHR=imgHR[,,j]
    
    labMat[(i-1)*n_points+1:n_points,1,j]=channelHR[cbind(select_row*2-1,select_col*2-1)]-center
    labMat[(i-1)*n_points+1:n_points,2,j]=channelHR[cbind(select_row*2-1,select_col*2)]-center
    labMat[(i-1)*n_points+1:n_points,3,j]=channelHR[cbind(select_row*2,select_col*2-1)]-center
    labMat[(i-1)*n_points+1:n_points,4,j]=channelHR[cbind(select_row*2,select_col*2)]-center
    ### step 3. repeat above for three channels
  }
}
return(list(feature = featMat, label = labMat))
  
}

###Diagonal###
feature_diagonal <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 4, 3))
  colnames(featMat) <- c("v1","v2","v3","v4")
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  colnames(labMat) <- c("v1","v2","v3","v4")

  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgLR <- as.array(imgLR)
    imgHR = as.array(imgHR)
    ### step 1. sample n_points from imgLR
    dimLR=dim(imgLR)
    select=sample(dimLR[1]*dimLR[2],n_points)
    select_row=(select-1)%%dimLR[1]+1
    select_col=(select-1)%/%dimLR[1]+1
    #summary(selectLR)
    ### step 2. for each sampled point in imgLR,
    ### step 2.1. save (the neighbor 24 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    for(j in 1:3){
      pad=cbind(0,imgLR[,,j],0)
      pad=rbind(0,pad,0)
      center=pad[cbind(select_row+1,select_col+1)]
      
     
      featMat[(i-1)*n_points+1:n_points,1,j]=pad[cbind(select_row,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,2,j]=pad[cbind(select_row,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,3,j]=pad[cbind(select_row+2,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,4,j]=pad[cbind(select_row+2,select_col+2)]-center
      
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
      channelHR=imgHR[,,j]
      
      labMat[(i-1)*n_points+1:n_points,1,j]=channelHR[cbind(select_row*2-1,select_col*2-1)]-center
      labMat[(i-1)*n_points+1:n_points,2,j]=channelHR[cbind(select_row*2-1,select_col*2)]-center
      labMat[(i-1)*n_points+1:n_points,3,j]=channelHR[cbind(select_row*2,select_col*2-1)]-center
      labMat[(i-1)*n_points+1:n_points,4,j]=channelHR[cbind(select_row*2,select_col*2)]-center
      ### step 3. repeat above for three channels
    }
  }
  return(list(feature = featMat, label = labMat))
}



###24 pixels feature###
feature_24 <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 24, 3))
  colnames(featMat) <- c("v1","v2","v3","v4","v5","v6","v7","v8","v9","v10","v11","v12",
                         "v13","v14","v15","v16","v17","v18","v19","v20","v21","v22","v23","v24")
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  colnames(labMat) <- c("v1","v2","v3","v4")
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgLR <- as.array(imgLR)
    imgHR = as.array(imgHR)
    ### step 1. sample n_points from imgLR
    dimLR=dim(imgLR)
    select=sample(dimLR[1]*dimLR[2],n_points)
    select_row=(select-1)%%dimLR[1]+1
    select_col=(select-1)%/%dimLR[1]+1
    #summary(selectLR)
    ### step 2. for each sampled point in imgLR,
    ### step 2.1. save (the neighbor 24 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    for(j in 1:3){
      pad=cbind(0,0,imgLR[,,j],0,0)
      pad=rbind(0,0,pad,0,0)
      center=pad[cbind(select_row+2,select_col+2)]
      
      
      featMat[(i-1)*n_points+1:n_points,1,j]=pad[cbind(select_row,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,2,j]=pad[cbind(select_row,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,3,j]=pad[cbind(select_row,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,4,j]=pad[cbind(select_row,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,5,j]=pad[cbind(select_row,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,6,j]=pad[cbind(select_row+1,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,7,j]=pad[cbind(select_row+2,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,8,j]=pad[cbind(select_row+3,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,9,j]=pad[cbind(select_row+4,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,10,j]=pad[cbind(select_row+4,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,11,j]=pad[cbind(select_row+4,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,12,j]=pad[cbind(select_row+4,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,13,j]=pad[cbind(select_row+4,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,14,j]=pad[cbind(select_row+3,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,15,j]=pad[cbind(select_row+2,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,16,j]=pad[cbind(select_row+1,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,17,j]=pad[cbind(select_row+1,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,18,j]=pad[cbind(select_row+1,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,19,j]=pad[cbind(select_row+1,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,20,j]=pad[cbind(select_row+2,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,21,j]=pad[cbind(select_row+3,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,22,j]=pad[cbind(select_row+3,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,23,j]=pad[cbind(select_row+3,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,24,j]=pad[cbind(select_row+2,select_col+1)]-center
      
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
      channelHR=imgHR[,,j]
      
      labMat[(i-1)*n_points+1:n_points,1,j]=channelHR[cbind(select_row*2-1,select_col*2-1)]-center
      labMat[(i-1)*n_points+1:n_points,2,j]=channelHR[cbind(select_row*2-1,select_col*2)]-center
      labMat[(i-1)*n_points+1:n_points,3,j]=channelHR[cbind(select_row*2,select_col*2-1)]-center
      labMat[(i-1)*n_points+1:n_points,4,j]=channelHR[cbind(select_row*2,select_col*2)]-center
      ### step 3. repeat above for three channels
    }
  }
  return(list(feature = featMat, label = labMat))
}



###canny+24

feature_c24 <- function(LR_dir, HR_dir, n_points=1000){
  library("EBImage")
  library("imager")
  n_files <- length(list.files(LR_dir))
  featMat <- array(NA, c(n_files * n_points, 24, 3))
  colnames(featMat) <- c("v1","v2","v3","v4","v5","v6","v7","v8","v9","v10","v11","v12","v13","v14","v15","v16","v17","v18","v19","v20","v21","v22","v23","v24")
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  colnames(labMat) <- c("v1","v2","v3","v4")
  
  
  for (i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- as.array(imgHR)
    
    imgLR <- imgLR %>% as.cimg %>% cannyEdges %>% as.cimg
    dimLR <- dim(imgLR)
    
    
    s1 <- sample(which(imgLR[,,1]==1),n_points*0.6,replace=T)
    s2 <- sample(which(imgLR[,,1]==0),n_points*0.4,replace=T)
    select <- c(s1,s2)
    
    select_row=(select-1)%%dimLR[1]+1
    select_col=(select-1)%/%dimLR[1]+1
    
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgLR <- as.array(imgLR)
    
    for(j in 1:3){
      pad=cbind(0,0,imgLR[,,j],0,0)
      pad=rbind(0,0,pad,0,0)
      center=pad[cbind(select_row+2,select_col+2)]
      
      
      featMat[(i-1)*n_points+1:n_points,1,j]=pad[cbind(select_row,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,2,j]=pad[cbind(select_row,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,3,j]=pad[cbind(select_row,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,4,j]=pad[cbind(select_row,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,5,j]=pad[cbind(select_row,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,6,j]=pad[cbind(select_row+1,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,7,j]=pad[cbind(select_row+2,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,8,j]=pad[cbind(select_row+3,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,9,j]=pad[cbind(select_row+4,select_col+4)]-center
      featMat[(i-1)*n_points+1:n_points,10,j]=pad[cbind(select_row+4,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,11,j]=pad[cbind(select_row+4,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,12,j]=pad[cbind(select_row+4,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,13,j]=pad[cbind(select_row+4,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,14,j]=pad[cbind(select_row+3,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,15,j]=pad[cbind(select_row+2,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,16,j]=pad[cbind(select_row+1,select_col)]-center
      featMat[(i-1)*n_points+1:n_points,17,j]=pad[cbind(select_row+1,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,18,j]=pad[cbind(select_row+1,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,19,j]=pad[cbind(select_row+1,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,20,j]=pad[cbind(select_row+2,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,21,j]=pad[cbind(select_row+3,select_col+3)]-center
      featMat[(i-1)*n_points+1:n_points,22,j]=pad[cbind(select_row+3,select_col+2)]-center
      featMat[(i-1)*n_points+1:n_points,23,j]=pad[cbind(select_row+3,select_col+1)]-center
      featMat[(i-1)*n_points+1:n_points,24,j]=pad[cbind(select_row+2,select_col+1)]-center
      
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
      channelHR=imgHR[,,j]
      
      labMat[(i-1)*n_points+1:n_points,1,j]=channelHR[cbind(select_row*2-1,select_col*2-1)]-center
      labMat[(i-1)*n_points+1:n_points,2,j]=channelHR[cbind(select_row*2-1,select_col*2)]-center
      labMat[(i-1)*n_points+1:n_points,3,j]=channelHR[cbind(select_row*2,select_col*2-1)]-center
      labMat[(i-1)*n_points+1:n_points,4,j]=channelHR[cbind(select_row*2,select_col*2)]-center
      ### step 3. repeat above for three channels
    }
  }
  return(list(feature = featMat, label = labMat))
}


