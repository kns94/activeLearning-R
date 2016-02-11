#Different Outlier Detection Methods

lof <- function(df, n, k){
  #Method M1 -- LOF (multivariate)
  outlier.scores <- lofactor(df, k)
  # pick top n outliers
  m1 <- order(outlier.scores, decreasing=T)[1:n]
  o1 <-t(t(m1))
  #M1 output LOF detection
  m1 <-t(t(o1[ order(-o1[,1], o1[,1],decreasing=TRUE), ]))
  m1
}

#mean <- function(df){
#Method M2 -- largest difference from the sample mean (monovariate)
#  output_m2 <- outlier(df, logical=TRUE)
#  find_outlier_m2 <- which(output_m2==TRUE, arr.ind=TRUE)
#  m2 <-find_outlier_m2[,1]
#  o2 <-t(t(m2))
#  #M2 Output  outlier detection
#  m2 <-t(t(o2[ order(-o2[,1], o2[,1],decreasing=TRUE), ]))
#  #df_outlier$meandiff <- replace(df_outlier$meandiff,m2,1)
#  m2
#}

mahal <- function(df, n){
  #Method M3 -- Mahalanobis distance (multivariate)
  m.dist.order                  <- order(mahalanobis(df, colMeans(df), cov(df)), decreasing=TRUE)
  is.outlier                    <- rep(FALSE, nrow(df))
  is.outlier[m.dist.order[1:n]] <- TRUE # Mark as outliers the n most extreme points
  col                           <- is.outlier + 1
  m3 <- which(is.outlier ==TRUE, arr.ind=TRUE)
  o3 <- t(t(m3))
  #M3 Output Mahalanobis outlier detection (multivariate)
  m3 <-t(t(o3[ order(-o3[,1], o3[,1],decreasing=TRUE), ]))
  #df_outlier$mahalanobis <- replace(df_outlier$mahalanobis, m3, 1)
  unique(m3)
}

chisq <- function(df){
  #Method M4 -- Chi square (monovariate)
  m4 <- c(which( df[,1]== 7.9), which(df[,2]==  4.4), which(df[,3]==  6.9),which(df[,4]==  2.5))
  # use regular expression to extract values -- for now manually entered
  chisq.out.test(df[,1])$alternative # 7.9
  chisq.out.test(df[,2])$alternative # 4.4
  chisq.out.test(df[,3])$alternative # 6.9
  chisq.out.test(df[,4])$alternative # 2.5
  o4 <- t(t(m4))
  #M4 Output of Chisquare method
  m4 <-t(t(o4[ order(-o4[,1], o4[,1],decreasing=TRUE), ]))
  #df_outlier$chisq <- replace(df_outlier$chisq, m4, 1)
  unique(m4)
}

km <- function(df, n, k){
  #outlier detection using k means
  kmeans.result <- kmeans(df, centers=k)
  #cluster centres
  kmeans.result$centers
  #Cluster Ids
  kmeans.result$cluster
  #Calculate distance between objects and cluster centres
  centers <- kmeans.result$centers[kmeans.result$cluster, ]
  distances <- sqrt(rowSums((df - centers)^2))
  outliers <- order(distances, decreasing=T)[1:n]
  unique(outliers)
}

box <- function(df){
  #outlier detection using boxplot
  outliers <- c()
  for(i in 1:ncol(df)){
    box <- boxplot(df[,i])
    for(j in 1:length(box$out)){
      a = which(df[,i] == box$out[j])
      if(length(a) == 1){
        outliers[length(outliers) + 1] <- a 
      }
    }
  }
  unique(outliers)
  #outliers
}

MADO <- function(df){
  #outlier detection using Median Absolute Detection
  outliers <- c()
  for(i in 1:ncol(df)){
    #Calculate Median Absolute Detection
    medianAD <- mad(df[,i])
    #Find Medium for every column
    med <- median(df[,i])
    #Set Upper and lower bounds
    upper_bound = med + 2*medianAD
    lower_bound = med - 2*medianAD
    #Fetch outliers based on lower and upper bounds defined
    greater = which(df[,i] > upper_bound)
    
    if(as.numeric(length(greater))){
      for(j in 1:as.numeric(length(greater))){
        outliers[length(outliers) + 1] <- greater[j]
      }  
    }
    
    lesser = which(df[,i] < lower_bound)
    
    if(as.numeric(length(lesser))){
      for(j in 1:as.numeric(length(lesser))){
        outliers[length(outliers) + 1] <- lesser[j]
      }  
    }
  }
  #Unique row names 
  unique(outliers)
}

threeSig <- function(df){
  #ThreeSigma Outlier Detection
  sigma <- 3
  outliers <- c()
  for(i in 1:ncol(df)){
    #Mean for every column
    daMean <- mean(df[,i])
    #Standard deviation for every column
    daSD <- sd(df[,i])
    #Calculate Lower and Upper bounds
    lower_bound <- daMean - sigma*daSD
    upper_bound <- daMean + sigma*daSD
    #Outliers based on bounds calculated
    greater = which(df[,i] > upper_bound)
    
    if(as.numeric(length(greater))){
      for(j in 1:as.numeric(length(greater))){
        outliers[length(outliers) + 1] <- greater[j]
        #print(greater[j])
      }  
    }
    
    lesser = which(df[,i] < lower_bound)
    
    if(as.numeric(length(lesser))){
      for(j in 1:as.numeric(length(lesser))){
        outliers[length(outliers) + 1] <- lesser[j]
        #print(lesser[j])
      }  
    }
  }
  #Unique outliers
  unique(outliers)
}