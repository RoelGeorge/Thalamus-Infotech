#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("shinythemes")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("cowplot")
#install.packages("scales")
#install.packages("readr")
#install.packages( "lavaan")
#install.packages("smooth", "Hmisc")
#install.packages("plyr")

library("cowplot")
library("dplyr")
library("shiny")
library("shinydashboard")
library("shinythemes")
library("ggplot2")
library("scales")
library("plyr")
library("smooth","Hmisc")
library("readr")

source("R_rainclouds.R")
source("summarySE.R")
source("simulateData.R")

server <- function(input, output) {
  
  riskdata<-read.csv(input$file$datapath)
  
  output$table<-renderTable({
    
    return(head(riskdata))
  },width = "30%")
  
}

ui <- fluidPage(
  theme = shinytheme("darkly"),
  titlePanel(title = "App2.R"),
  sidebarPanel(
    width = 3,
    fileInput(
      inputId = "file",
      label="Choose  a file",
      accept=c("text/csv",
               "text/comma-separated-values,text/plain",
               ".csv")
      ),

    tags$hr(),
    
    wellPanel(tableOutput("table")),
    
    tags$hr(),
    selectInput(
      inputId = "dp_var",
      label = "Dependent Variable(y):",
      choices =colnames(riskdata) ,
      selected = NULL
    ),
    
    tags$hr(),
    selectInput(
      inputId = "indp_var",
      label = "Independent Variable(x):",
      choices =colnames(riskdata) ,
      selected = NULL
    )
    
    
    
  ),
  mainPanel(
    
  )
  
)




shinyApp(ui=ui, server=server)










