pwdlibrary(shiny)

ui <- fluidPage(
  
  headerPanel("Outlier Detection through Active Learning"),
  
  sidebarPanel(
    #selectInput("dataset", label = "Data Set", choices = c('iris', 'ionosphere', 'arrhythmia', 'isolet5', 'pima-indians-diabetes'
    #                                                       , 'australian', 'spambase', 'waveform'), selected = TRUE),
    fileInput('infile', 'Upload Dataset', accept=c('rds')),
    textInput('budget', 'Budget', value = 0),
    #textInput('batch', 'Batch', value = 0),
    #selectInput("classifier", label = "Classifier", choices = c('lda'), selected = TRUE),
    textInput('classifier', 'Classifier', value = 'lda'),
    textInput('knn', 'KNN for LOF', value = 6),
    textInput('kmn', 'Num Centres for KMean', value = 4),
    actionButton("btn", "Extract"),
    br(),
    textOutput('err'),
    width = 2
  ),
  
  mainPanel(
    br(),
    textOutput("subheading"),
    fileInput('file1', 'Upload Oracle rds File', accept=c('rds')),
    br(),
    uiOutput("learning"),
    #textOutput('err1'),
    tableOutput("batch_table"),
    br(),
    div(dataTableOutput("final_analysis"), style = "font-size:80%")
    #actionButton('end_button')
  )
)
