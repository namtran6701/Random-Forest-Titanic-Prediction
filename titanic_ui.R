library(shiny)

pacman::p_load(shiny, shinythemes, pacman, magrittr, tidyverse, devtools, randomForest, RCurl, data.table)
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
