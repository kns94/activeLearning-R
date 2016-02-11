## Download and install the packages
required_packages <- c('outliers', 'DMwR', 'caret', 'MASS', 'entropy', 'shiny', 'DT', 'e1071')

for(i in 1:length(required_packages)){
  if(required_packages[i] %in% installed.packages()){
    library(required_packages[i], character.only = TRUE)
  }
  else{
    install.packages(required_packages[i], dependencies = TRUE)
    library(required_packages[i], character.only = TRUE)
  }
}
