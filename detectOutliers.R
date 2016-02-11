source('outlierDetectionMethods.R')

detectOutliers <- function(dataset, maxOut){
  
  datasetPath <- sprintf('data/sources/%s.data', dataset)
  dat <- read.csv(datasetPath, header = FALSE)
  #Take non categorical columns
  data_frame <- dat[sapply(dat, function(x) !is.factor(x))]
  
  #Create an empty list to record number of outlier for every method
  num_outliers <- list()
  
  #Creating a data frame to store output of every outlier detection method
  df_outlier <- data.frame(matrix(0, nrow = nrow(data_frame), ncol = 7))
  colnames(df_outlier) <- c('LOF','Mahalanobis','kMeans', 'ChiSq', 'BoxPlot', 'MAD', 'threeSigma')
  
  #' For a given dataset, implement outlier detection methods and fill values in the df_outlier matrix
  #' Try/Catch block used to check if NO outliers detected or if there's an ERROR in anyone of the methods
  #' Check the terminal for output of the error! 
  
  #LOF
  LOF <- NA
  out <- tryCatch({
    LOF <- lof(data_frame, maxOut, 6)
    num_outliers$LOF <- maxOut
  },
  error = function(x){
    num_outliers$LOF <- NA
  },
  finally = {
    if(!is.na(LOF[1])){
      df_outlier$LOF <- replace(df_outlier$LOF, LOF, 1)  
    }
    else{
      df_outlier$LOF <- NA
    }
  }
  )
  
  #Mahalanobis
  Mahalanobis <- NA
  out <- tryCatch({
    Mahalanobis <- mahal(data_frame, maxOut)
    num_outliers$Mahalanobis <- maxOut
  },
  error = function(x){
    print(x)
    num_outliers$Mahalanobis <- NA
  },
  finally = {
    if(!is.na(Mahalanobis[1])){
      df_outlier$Mahalanobis <- replace(df_outlier$Mahalanobis, Mahalanobis, 1)  
    }
    else{
      df_outlier$Mahalanobis <- NA
    }
  }
  )
  
  #Using KMeans
  kMeans <- NA
  out <- tryCatch({
    kMeans <- km(data_frame, maxOut, 4)
    num_outliers$kMeans <- maxOut
  },
  error = function(x){
    print(x)
    num_outliers$kMeans <- NA
  },
  finally = {
    if(!is.na(kMeans[1])){
      df_outlier$kMeans <- replace(df_outlier$kMeans, kMeans, 1)  
    }
    else{
      df_outlier$kMeans <- NA
    }
  }
  )
  
  #ChiSq
  ChiSq <- NA
  out <- tryCatch({
    ChiSq <- chisq(data_frame)
  },
  error = function(x){
    print(x)
    num_outliers$ChiSq <- NA
  },
  finally = {
    if(!is.na(ChiSq[1])){
      num_outliers$ChiSq <- length(ChiSq)
      df_outlier$ChiSq <- replace(df_outlier$ChiSq, ChiSq, 1)  
    }
    else{
      df_outlier$ChiSq <- NA
    }
  }
  )
  
  #Boxplot
  BoxPlot <- NA
  out <- tryCatch({
    BoxPlot <- box(data_frame)
  },
  error = function(x){
    print(x)
    num_outliers$BoxPlot <- NA
  },
  finally = {
    if(!is.na(BoxPlot[1])){
      num_outliers$BoxPlot <- length(BoxPlot)
      df_outlier$BoxPlot <- replace(df_outlier$BoxPlot, BoxPlot, 1)
    }
    else{
      df_outlier$BoxPlot <- NA
    }
  }
  )
  
  #MAD
  MAD <- NA
  out <- tryCatch({
    MAD <- MADO(data_frame)
  },
  error = function(x){
    print(x)
    num_outliers$MAD <- NA
  },
  finally = {
    if(!is.na(MAD[1])){
      num_outliers$MAD <- length(MAD)
      df_outlier$MAD <- replace(df_outlier$MAD, MAD, 1)
    }
    else{
      df_outlier$MAD <- NA
    }
  }
  )
  
  #ThreeSigma
  threeSigma <- NA
  out <- tryCatch({
    threeSigma <- threeSig(data_frame)
  },
  error = function(x){
    print(x)
    num_outliers$threeSigma <- NA
  },
  finally = {
    if(!is.na(threeSigma[1])){
      num_outliers$threeSigma <- length(threeSigma)
      df_outlier$threeSigma <- replace(df_outlier$threeSigma, threeSigma, 1)
    }
    else{
      df_outlier$threeSigma <- NA
    }
  }
  )
  
  #If all of them agree it's an outlier
  #for(i in 1:nrow(df_outlier)){
  #if(df_outlier[i,1] == df_outlier[i,2] && df_outlier[i,2] == df_outlier[i,3] && df_outlier[i,3] == df_outlier[i,4]
  #   && df_outlier[i,5] == df_outlier[i, 4] && df_outlier[i,5] == df_outlier[i, 6] && df_outlier[i,1] == 1){
  #    training_mat[i,5] = 1
  #  }
  #}
  
  #Calculate Disagreement for every column
  #disagreement <- vote_entropy(df_outlier)
  #Order disagreement and fetch only top values according to budged
  #disagreement <- order(disagreement, decreasing = TRUE)[1:maxOut]
  
  filename = sprintf("data/outliers/%s_outliers.rds", dataset)
  saveRDS(df_outlier, filename)
  
  filename = sprintf("data/outliers/%s_outliers.csv", dataset)
  write.csv(df_outlier, filename)
}