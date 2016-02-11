source('detectOutliers.R')

createLink <- function(df, val, Output){
  if(length(val) > 0){
    df[1, 'Output'] <- sprintf('<a href="/%s" target="_blank" class="btn btn-primary">%s</a>',val, val)        
  }
}

vote_entropy <- function(df){
  #Calculate disagreement amongst columns
  ent <- c()
  #Disagreements for every row
  for(i in 1:nrow(df)){
    entropy <- entropy(table(as.vector(as.numeric(df[i,]))))
    ent[length(ent) + 1] <- entropy 
  }
  ent
}

active_learning <- function(dataset, inFile, budget, classifier){
  
  init_time = Sys.time()
  #Oracle is the ground truth we will use to evaluate active learning
  #oracle <- sprintf("oracle_%s",dataset)
  #Fetch specified dataset from data folder
  datasetPath <- sprintf('data/sources/%s.data', dataset)
  dat <- read.csv(datasetPath, header = FALSE)
  #Take non categorical columns
  data_frame <- dat[sapply(dat, function(x) !is.factor(x))]
  
  outlier_file_name = sprintf('data/outliers/%s_outliers.rds', dataset)
  print(outlier_file_name)
  df_outlier <- readRDS(outlier_file_name)
  
  #Get oracle and save it as a label in training matrix
  oracle <- readRDS(inFile)
  
  num_outliers <- list()
  num_outliers$LOF = length(which(df_outlier$LOF == 1))
  if(length(which(is.na(df_outlier$LOF))) > 0){
    num_outliers$LOF <- 'Error'
  }
  
  num_outliers$Mahalanobis = length(which(df_outlier$Mahalanobis == 1))
  if(length(which(is.na(df_outlier$Mahalanobis))) > 0){
    num_outliers$Mahalanobis <- 'Error'
  }
  
  num_outliers$kMeans = length(which(df_outlier$kMeans == 1))
  if(length(which(is.na(df_outlier$kMeans))) > 0){
    num_outliers$kMeans <- 'Error'
  }
  
  num_outliers$ChiSq = length(which(df_outlier$ChiSq == 1))
  if(length(which(is.na(df_outlier$ChiSq))) > 0){
    num_outliers$ChiSq <- 'Error'
  }
  
  num_outliers$BoxPlot = length(which(df_outlier$BoxPlot == 1))
  if(length(which(is.na(df_outlier$BoxPlot))) > 0){
    num_outliers$BoxPlot <- 'Error'
  }
  
  num_outliers$MAD = length(which(df_outlier$MAD == 1))
  if(length(which(is.na(df_outlier$MAD))) > 0){
    num_outliers$MAD <- 'Error'
  }
  
  num_outliers$threeSigma = length(which(df_outlier$threeSigma == 1))
  if(length(which(is.na(df_outlier$threeSigma))) > 0){
    num_outliers$threeSigma <- 'Error'
  }
  
  #Calculate Disagreement for every column
  disagreement <- vote_entropy(df_outlier)
  #Order disagreement and fetch only top values according to budget
  disagreement <- order(disagreement, decreasing = TRUE)[1:budget]
  
  #Create a training matrix
  training_mat <- data_frame
  #Add another column called is_outlier
  training_mat$is_outlier <- NA
  

  #For values with disagreement, fill the oracle output
  training_mat[disagreement, 'is_outlier'] <- oracle[disagreement, 'is_outlier']
  
  #Test matrix is the values not labeled 
  #test_mat <- subset(training_mat, is.na(training_mat$is_outlier))
  #Test matrix, where values are not labeled
  test_mat <- subset(training_mat, select = -c(is_outlier))
  #Training matrix is the values labeled
  training_mat <- subset(training_mat, training_mat$is_outlier != "NA")
  #Renaming labels as yes and no
  training_mat$is_outlier[training_mat$is_outlier == 0] <- 'no'
  training_mat$is_outlier[training_mat$is_outlier == 1] <- 'yes'
  
  #Renaming labels as yes and no
  df_outlier$LOF[df_outlier$LOF == 0] <- 'no'
  df_outlier$LOF[df_outlier$LOF == 1] <- 'yes'
  
  df_outlier$Mahalanobis[df_outlier$Mahalanobis == 0] <- 'no'
  df_outlier$Mahalanobis[df_outlier$Mahalanobis == 1] <- 'yes'
  
  df_outlier$kMeans[df_outlier$kMeans == 0] <- 'no'
  df_outlier$kMeans[df_outlier$kMeans == 1] <- 'yes'
  
  df_outlier$ChiSq[df_outlier$ChiSq == 0] <- 'no'
  df_outlier$ChiSq[df_outlier$ChiSq == 1] <- 'yes'
  
  df_outlier$BoxPlot[df_outlier$BoxPlot == 0] <- 'no'
  df_outlier$BoxPlot[df_outlier$BoxPlot == 1] <- 'yes'
  
  df_outlier$MAD[df_outlier$MAD == 0] <- 'no'
  df_outlier$MAD[df_outlier$MAD == 1] <- 'yes'
  
  df_outlier$threeSigma[df_outlier$threeSigma == 0] <- 'no'
  df_outlier$threeSigma[df_outlier$threeSigma == 1] <- 'yes'
  
  outputdf <- df_outlier
  outputdf$activeLearning <- NA
  
  #Removes rows trained
  #df_outlier <- df_outlier[!rownames(df_outlier) %in% rownames(training_mat),]
  
  #data <- training_mat[sapply(training_mat, function(x) !is.factor(x))]
  #DataFrame without labels
  data <- subset(training_mat, select = -c(is_outlier))
  #DataFrame with labels
  labels <- training_mat[, 'is_outlier']
  
  fmeasure <- NA
  
  #Train using classifier 
  train_out <- NA
  ActiveLearning <- NA
  out <- tryCatch({
    # Trains the classifier with caret:::train
    train_out <- train(x = data, y = labels, method = classifier)
  },
  error = function(x){
    print(x)
  },
  warning = function(y){
    print(y)
  },
  finally = {
    #If the classifier is trained
    if(!is.na(train_out[1])){
      # Classifies the test data set with the constructed classifier.
      test_predictions <- predict(train_out, test_mat)
      num_outliers$ActiveLearning <- length(test_predictions[test_predictions == 'yes'])
      outputdf$activeLearning <- test_predictions
      outputdf$oracle <- oracle$is_outlier
      outputdf$labeled <- 'no'
      outputdf[disagreement, 'labeled'] <- 'yes'
      
      #Fill final result of classifier as an output
      #j = 1
      #k = 1
      #for(i in 1:nrow(outputdf)){
      #  if(i %in% disagreement){
      #    #print(labels[k])
      #    outputdf[i,'activeLearning'] <- as.character(labels[k])
      #    k = k + 1
      #  }  
      #  else{
          #print(test_predictions[j])
      #    outputdf[i,'activeLearning'] <- as.character(test_predictions[j])
      #    j = j + 1
      #  }
      #}
      
      #Training Confusion Matrix
      train_output = confusionMatrix(test_predictions[disagreement], oracle[disagreement, 'is_outlier'])
      tn = train_output$table[1,1]
      fp = train_output$table[1,2]
      fn = train_output$table[2,1]
      tp = train_output$table[2,2]
      precision = tp/tp+fp
      recall = as.numeric(train_output$byClass[2])
      f1 = 1/(1/precision + 1/recall) 
      f1 = round(f1, 3)
      
      #Final Confusion Matrix
      train_output = confusionMatrix(test_predictions, oracle$is_outlier)
      tn = train_output$table[1,1]
      fp = train_output$table[1,2]
      fn = train_output$table[2,1]
      tp = train_output$table[2,2]
      precision = tp/tp+fp
      recall = as.numeric(train_output$byClass[2])
      f2 = 1/(1/precision + 1/recall) 
      f2 = round(f2, 3)
      
      fmeasure = sprintf("%s/%s", f1, f2)
      
      #Save dataset as output dataframe and CSV file
      location = sprintf("%s_%s_%s.csv", dataset, budget, classifier)
      filename = sprintf("www/%s_%s_%s.rds", dataset, budget, classifier)
      saveRDS(outputdf, filename)
      csvfilename = sprintf("www/%s_%s_%s.csv", dataset, budget, classifier)
      write.csv(csvfilename, x = outputdf)
    }
    else{
      #If classification fails then return NA
      num_outliers$ActiveLearning <- NA
    }
  }
  )
  
  #if(okay == TRUE){
  #  lof_output = confusionMatrix(test_predictions, df_outlier$LOF)
  #  mahal_output = confusionMatrix(test_predictions, df_outlier$Mahalanobis)
  #  kmeans_output = confusionMatrix(test_predictions, df_outlier$kMeans)
  #  chisq_output = confusionMatrix(test_predictions, df_outlier$ChiSq)
  #  boxplot_output = confusionMatrix(test_predictions, df_outlier$BoxPlot)
  #  mad_output = confusionMatrix(test_predictions, df_outlier$MAD)
  #  threesigma_output = confusionMatrix(test_predictions, df_outlier$threeSigma)
  #}
    
  #Create a dataframe called final analysis to save results of the current iteration
  final_analysis <- data.frame(matrix(nrow = 1, ncol = 14))
  #colnames(final_analysis) <- c('Summary')
  colnames(final_analysis) <- c('DataSet', 'Budget', 'Classifier', 'LOF', 'Mahalanobis', 'kMeans', 'ChiSq', 'BoxPlot', 'MAD', 'threeSigma', 'ActiveLearning', 'fmeasure', 'Time', 'Output')
  final_analysis <- final_analysis[-1,]
  
  #Assigning Error value, if the method is not an output in num_outliers
  for(i in 1:length(num_outliers)){
    if(is.na(num_outliers[i])){
      num_outliers[i] <- 'Error'
    }
  }
  
  outvalues <-  c('LOF', 'Mahalanobis', 'kMeans', 'ChiSq', 'BoxPlot', 'MAD', 'threeSigma', 'ActiveLearning')
  extractedvalues <- c(names(num_outliers))
  
  #Validation for num_outliers
  #for(i in 1:length(outvalues)){
  #  a = get(num_outliers$outvalues[i])
  #  if(outvalues[i] %in% extractedvalues){
  #    next  
  #  }
  #  else{
  #    #print(length(a))
  #    if(length(a) == 0){
  #      num_outliers[outvalues[i]] = 0
  #    }
  #    if(length(a) == 1){
  #        num_outliers[outvalues[i]] <- NA
  #    }
  #  }
  #}
  
  output <- list()
    
  #Saving output in a dataframe
  final_analysis[1, 1] <- dataset
  final_analysis[1, 2] <- budget
  final_analysis[1, 3] <- classifier
  final_analysis[1, 4] <- num_outliers$LOF
  final_analysis[1, 5] <- num_outliers$Mahalanobis
  final_analysis[1, 6] <- num_outliers$kMeans
  final_analysis[1, 7] <- num_outliers$ChiSq
  final_analysis[1, 8] <- num_outliers$BoxPlot
  final_analysis[1, 9] <- num_outliers$MAD
  final_analysis[1, 10] <- num_outliers$threeSigma
  final_analysis[1, 11] <- num_outliers$ActiveLearning
  final_analysis[1, 12] <- fmeasure
  final_analysis[1, 13] <- Sys.time() - init_time
   
  if(num_outliers$ActiveLearning == 'Error'){
    final_analysis[1, 14] <- NA
  } else{
    final_analysis[1, 14] <- createLink(final_analysis, location, output)
  }
  
  output$data_frame <- final_analysis
  #print(output)
  
  return(output)
}  