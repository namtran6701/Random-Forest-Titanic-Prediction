# Machine learning Shiny Web app

pacman::p_load(shiny, shinythemes, pacman, magrittr, tidyverse, randomForest, RCurl, data.table)
# install_github('agstn/dataxray')
# library(dataxray)

df <- read.csv('train.csv', stringsAsFactors = T)

# Convert binary (0,1) to survived and not survived

df$Survived <- with(df, ifelse(Survived == 0, 'Not Survived', 'Survived'))


# remove all missing data and change survival status to categorical and remove some unnecessary features.
df %<>%
  na.omit() %>% 
  mutate(Survived = as.factor(Survived),
         Pclass = as.factor(Pclass),
         SibSp = as.factor(SibSp),
         Parch = as.factor(Parch)) %>% 
  select(-c(PassengerId, Name, Ticket, Cabin, Embarked))



# Explore the data
# df %>% 
#   make_xray() %>% 
#   view_xray()

# Link to the dataset description https://www.kaggle.com/competitions/titanic/data?select=train.csv

fit_forest <- randomForest(data = df,
                           Survived ~.,
                           ntree = 500,
                           mtry = 4,
                           importance = T)



# Save model to RDS file 
saveRDS(fit_forest, 'model.rds')

# Read in the RF model
fit_forest <-  readRDS('model.rds')

ui <- fluidPage(theme = shinytheme('sandstone'),
                # Page header
                headerPanel('Titanic Surived?'),
                # input values 
                sidebarPanel(
                  HTML('<h3>Input parameters</h3>'),
                  
                  selectInput('Pclass', label = 'Ticket Class:',
                              choices = list('Upper' = '1', 'Middle' = '2', 'Lower' = '3'),
                              selected = '3'),
                  
                  selectInput('Sex', label = 'Sex:',
                              choices = list('Male' = 'male', 'Female' = 'female'),
                              selected = 'male'),
                  
                  selectInput('Parch', label = 'Number of Parents/Children',
                              choices = list('No parents/Children' = '0', '1' = '1', '2'='2', '3'='3', '4'='4', '5'='5', '6'='6'),
                              selected = 'No parents/Children'),
                  
                  selectInput('SibSp', label = 'Number of Sibling/Spouses:',
                              choices = list('No Siblings/Spouses'='0', '1'='1', '2'='2', '3'='3', '4'='4', '5'='5'),
                              selected = '0'),
                  
                  sliderInput('Age', label = 'Age:',
                              min = 1, max = 90,
                              value =  35),
                  
                  sliderInput('Fare', label = 'Fare:',
                              min = 0, max = 513,
                              value = 100),
                  
                  actionButton('submitbutton', 'Submit', class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # status/output text box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # prediction results table. 
                  
                )
            )


# Create a server


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

# Create Shiny app

shinyApp(ui = ui, server = server)
