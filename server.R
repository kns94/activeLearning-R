source('active_learning.R')
source('validate_classifier.R')
source('installPackages.R')

#data_sets <- c("iris")

createLink <- function(val){
  sprintf('<a href="%s/%s" target="_blank" class="btn btn-primary">%s</a>',getwd(),val, val)   
}

#Used for creating global variables
values = reactiveValues()

#Create a global dataframe
values$out <- data.frame(matrix(nrow = 1, ncol = 14))
#Used so that the reactive function is called only once
values$first <- TRUE

init <- reactive({
  #colnames(values$out) <- c('Summary')
  colnames(values$out) <- c('DataSet', 'Budget', 'Classifier', 'LOF', 'Mahalanobis', 'kMeans', 'ChiSq', 'BoxPlot', 'MAD', 'threeSigma', 'ActiveLearning', 'fmeasure', 'Time', 'Output')

  values$out <- values$out[-1,]
})

server <- function(input, output, session) {
  
  #out <- data.frame(matrix(nrow = 1, ncol = 11))
  #colnames(final_analysis) <- c('Summary')
  #colnames(out) <- c('Data Set', 'Budget', 'Classifier', 'LOF', 'Mahalanobis', 'K Means', 'Chi Sq', 'Box plot', 'MAD', 'threeSigma','Time')
  #out <- out[-1,]
  
  #Validation
  output$err <- renderText({input$btn
    validate(
      need(!is.na(input$budget),'Budget cannot be null'),
      #need(!is.na(input$batch),'Batch value cannot be null'),
      need(!is.na(as.numeric(input$budget)),'The budget should be numeric'),
      need(validate_classifier(input$classifier), 'Invalid Classifier')
      #need(!is.na(as.numeric(input$batch)),'Batch value should be numeric')
    )
    
    #Load dataset
    datasetPath <- sprintf('data/sources/%s.data', input$dataset)
    dat <- read.csv(datasetPath, header = FALSE)
    #Create an oracle with random values
    name_oracle <- sprintf("data/oracle/oracle_%s.rds",input$dataset)
    oracle <- dat
    oracle$is_outlier <- NA
    
    for(i in 1:nrow(dat)){
      oracle[i, 'is_outlier'] <- sample(c('no', 'yes'), 1)
    }
    
    validate(
      need(input$file1$datapath, 'Please enter the correct oracle!')
      #need(batch < budget/3,batch_range)
    )
    
    #The oracle file uploaded
    oracle <- readRDS(input$file1$datapath)
    
    row <- as.numeric(nrow(dat))
    budget <- as.numeric(input$budget)
    #batch <- as.numeric(input$batch)
    range = sprintf("Budget should be between %s to %s in this case", as.integer(row/10), as.integer(row/3))
    #batch_range = sprintf("Batch should be less than %s", as.integer(row/30))
    
    validate(
      need(budget <= as.integer(row/3) && budget >= as.integer(row/10),range),
      need(nrow(oracle) == row, 'Please enter the correct oracle!')
      #need(batch < budget/3,batch_range)
    )
  })

  #When the button is clicked
  observeEvent(input$btn, {
    
    if(is.na(as.numeric(input$budget))){
      return()
    }
 
    datasetPath <- sprintf('data/sources/%s.data', input$dataset)
    dat <- read.csv(datasetPath, header = FALSE)
    
    valid <- validate_classifier(input$classifier)
    row <- as.numeric(nrow(dat))
    budget <- as.numeric(input$budget)
    #batch <- as.numeric(input$batch)
    classifier <- as.character(input$classifier)
    total_time <- 0
    
    #Validations
    if(budget >= as.integer(row/3) && budget <= as.integer(row/10)){
      print(1)
      return()
    }
    
    if(!valid){
      print(2)
      return()
    }
    
    inFile <- input$file1
    if (is.null(inFile)){
      print(3)
      return()    
    }
  
    if(values$first == TRUE){
      init()
      values$first = FALSE
    }
    
    #print(v())
    #print(input$file1$datapath)
    
    #Get oracle and save it as a label in training matrix
    oracle <- readRDS(input$file1$datapath)
    
    if(nrow(oracle) != row){
      return()
    }
    
    #Pass variables to active learning function
    aloutput <- active_learning(input$dataset, input$file1$datapath, budget, classifier)
    #print(final_analysis)
    out <- values$out
    out[nrow(out) + 1,] <- aloutput$data_frame[1,]
    values$out[nrow(values$out) + 1, ] <- aloutput$data_frame[1,]
    print(out)
    #print(createLink(out$output))
    
    output$final_analysis <- renderDataTable({
      #out$Output <- createLink(out$Output)
      return(out)
    }, escape = FALSE)
  })
}