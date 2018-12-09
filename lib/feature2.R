#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng/Siyu Zhu
### Project 3

#######################

feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  library("imager")
  n_files <- length(list.files(LR_dir)) 
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    # canny feature extraction
    imgLR <- imgLR %>% as.cimg %>% cannyEdges %>% as.cimg
    
    LR_nrow <- nrow(imgLR)
    LR_ncol <- ncol(imgLR)
    
    ### step 1. sample n_points frolm imgLR (Canny + Sampling)
    s1 <- sample(which(imgLR[,,1]==1),n_points*0.6,replace=T)
    s2 <- sample(which(imgLR[,,1]==0),n_points*0.4,replace=T)
    sampled_points <- c(s1,s2)
    
    #sampled_points <- sample(LR_nrow * LR_ncol, n_points)
    sample_row <- (sampled_points - 1) %% LR_nrow + 1
    sample_col <- (sampled_points - 1) %/% LR_nrow + 1
    
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    for (k in c(1:3)) {
  
      # supplementary image matrix
      supp_imgLR <- cbind(imgLR[, 1, k], imgLR[, , k], imgLR[, LR_ncol, k])
      supp_imgLR <- rbind(cbind(imgLR[1, 1, k], imgLR[1, , k],imgLR[1, LR_ncol, k]), 
                           supp_imgLR, cbind(imgLR[LR_nrow, 1, k], imgLR[LR_nrow, , k],imgLR[LR_nrow, LR_ncol, k]))
      #supp_imgLR <- rbind(supp_imgLR[1, , k], supp_imgLR[, , k], supp_imgLR[LR_nrow, , k])
      
      supp_imgHR <- imgHR[, , k]
      
      ### step 2. fill the featM and the labM
      center=supp_imgLR[cbind(sample_row+1, sample_col+1)]
      ## for featM
      featMat[(i-1)*n_points+1:n_points, 1, k] = supp_imgLR[cbind(sample_row,sample_col)] - center
      featMat[(i-1)*n_points+1:n_points, 2, k] = supp_imgLR[cbind(sample_row,sample_col + 1)] - center
      featMat[(i-1)*n_points+1:n_points, 3, k] = supp_imgLR[cbind(sample_row,sample_col + 2)] - center
      featMat[(i-1)*n_points+1:n_points, 4, k] = supp_imgLR[cbind(sample_row + 1,sample_col)] - center
      featMat[(i-1)*n_points+1:n_points, 5, k] = supp_imgLR[cbind(sample_row + 1,sample_col + 2)] - center
      featMat[(i-1)*n_points+1:n_points, 6, k] = supp_imgLR[cbind(sample_row + 2,sample_col)] - center
      featMat[(i-1)*n_points+1:n_points, 7, k] = supp_imgLR[cbind(sample_row + 2,sample_col + 1)] - center
      featMat[(i-1)*n_points+1:n_points, 8, k] = supp_imgLR[cbind(sample_row + 2,sample_col + 2)] - center
      ## for labM
      labMat[(i - 1) * n_points + 1:n_points,  1, k] <- supp_imgHR[cbind(sample_row*2-1,sample_col*2-1)]-center
      labMat[(i - 1) * n_points + 1:n_points,  2, k] <- supp_imgHR[cbind(sample_row*2-1,sample_col*2)]-center
      labMat[(i - 1) * n_points + 1:n_points,  3, k] <- supp_imgHR[cbind(sample_row*2,sample_col*2-1)]-center
      labMat[(i - 1) * n_points + 1:n_points,  4, k] <- supp_imgHR[cbind(sample_row*2,sample_col*2)]-center
      
    }
    
  }
  return(list(feature = featMat, label = labMat))
}

