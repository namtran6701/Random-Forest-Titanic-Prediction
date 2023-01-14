pacman::p_load(data.table, randomForest, pacman)

# Import the machine learning model 

fit_forest <- readRDS('model.rds')


server <- function(input, output, session){
  
  #input data 
  datasetInput <- reactive({
    #Outlook, temperature, humidity, windy, play
    df <- data.frame(
      Name = c('Pclass',
               'Sex',
               'Age',
               'SibSp',
               'Parch',
               'Fare'), 
      Value = as.character(c(input$Pclass,
                             input$Sex,
                             input$Age,
                             input$SibSp,
                             input$Parch,
                             input$Fare)),
      stringsAsFactors = F)
    
    Survived <- 'Survived'
    
    df <- rbind(df, Survived)
    
    input <- transpose(df)
    
    write.table(input,'input.csv', sep =',', quote = F, row.names = F, col.names = F)
    
    test <- read.csv(paste('input', '.csv', sep =''), header = T)
    
    test$Pclass <- factor(test$Pclass, levels = c('1','2','3'))
    
    test$Sex <- factor(test$Sex, levels = c('male', 'female'))
    
    test$SibSp <- factor(test$SibSp, levels = c('0','1','2','3','4','5'))
    
    test$Parch <- factor(test$Parch, levels = c('0','1','2','3','4','5','6'))
    
    Output <- data.frame(Prediction = predict(fit_forest, test), round(predict(fit_forest, test, type = 'prob'), 4))
    
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton > 0) {
      isolate("Calculation complete.")
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton > 0) {
      isolate(datasetInput())
    }
  })
}