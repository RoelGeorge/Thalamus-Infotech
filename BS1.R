library(shiny)

if (interactive()){
  

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      selectInput(
        inputId = "si1",
        label="choose dependent variable:",
        "Names"
      ),
      
      # Horizontal line ----
      tags$hr(),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output,session) {
  observe({
    output$contents <- renderTable({

      req(input$file1)
      
      df <- read.csv(input$file1$datapath)
      return(head(df))
    })
    updateSelectInput(session,inputId ="si1",choices = colnames(df))
  })
  

  
}
# Run the app ----
shinyApp(ui, server)
}