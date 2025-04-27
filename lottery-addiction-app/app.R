# Load the required libraries
library(shiny)
library(tidyverse)
library(shinythemes)
library(sets)
library(ggplot2)

# UI for the app
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  
  # App title
  titlePanel("Lottery Addiction App"),
  
  # Sidebar with input options
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      actionButton("loadData", "Load Data")
    ),
    
    # Main panel to display outputs
    mainPanel(
      textOutput("dataInfo"),  # Display information about the data
      tableOutput("table"),    # Display the data as a table
      plotOutput("barPlot"),   # Bar plot
      plotOutput("boxPlot"),   # Box plot
      verbatimTextOutput("summary")  # Summary statistics
    )
  )
)

# Server logic for the app
server <- function(input, output) {
  
  # Reactive value to store the loaded data
  data <- reactiveVal(NULL)
  
  # Load the data when the "Load Data" button is pressed
  observeEvent(input$loadData, {
    req(input$file1)  # Ensure the file input is not NULL
    
    # Read the CSV file
    df <- read.csv(input$file1$datapath)
    
    # Update the reactive data
    data(df)
    
    # Provide feedback on the number of rows and columns
    output$dataInfo <- renderText({
      paste("Data loaded with", nrow(df), "rows and", ncol(df), "columns.")
    })
    
    # Display the data as a table
    output$table <- renderTable({
      head(df)
    })
    
    # Summary statistics
    output$summary <- renderPrint({
      summary(df)
    })
  })
  
  # Bar plot: Lottery spending by age group
  output$barPlot <- renderPlot({
    req(data())  # Ensure data is loaded
    
    ggplot(data(), aes(x = AgeGroup, y = LotterySpending, fill = AgeGroup)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      theme_minimal() +
      labs(title = "Lottery Spending by Age Group", x = "Age Group", y = "Total Spending")
  })
  
  # Box plot: Distribution of lottery spending
  output$boxPlot <- renderPlot({
    req(data())  # Ensure data is loaded
    
    ggplot(data(), aes(x = AgeGroup, y = LotterySpending)) +
      geom_boxplot(fill = "skyblue", color = "black") +
      theme_minimal() +
      labs(title = "Distribution of Lottery Spending by Age Group", x = "Age Group", y = "Spending")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
