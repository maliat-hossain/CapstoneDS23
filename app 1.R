#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




library(shiny)
library(readr)
library(ggplot2)
library(httr)

# Load data
github_url <- "https://raw.githubusercontent.com/maliat-hossain/FileProcessing/main/future_predictions3.csv"
github_url1 <- "https://raw.githubusercontent.com/maliat-hossain/FileProcessing/main/trendresults_disease3"

future_predictions3 <- read_csv(url(github_url))
trend_results <- read.csv(url(github_url1))

# UI definition
ui <- fluidPage(
  titlePanel("Future Disease Predictions"),
  sidebarLayout(
    sidebarPanel(
      selectInput("diseaseSelect", "Select Disease:", choices = c("Select a Disease", "Giardiasis")),
      textInput("chatInput", "Chat with AI:", value = ""),
      actionButton("sendButton", "Send")
    ),
    mainPanel(
      tableOutput("predictions_table"),
      plotOutput("temp_trend_plot"),
      plotOutput("case_count_trend_plot"),
      verbatimTextOutput("chatResponse")
    )
  )
)

# Server logic
server <- function(input, output) {
  observeEvent(input$diseaseSelect, {
    if(input$diseaseSelect == "Giardiasis") {
      output$predictions_table <- renderTable({ future_predictions3 })
      output$temp_trend_plot <- renderPlot({
        ggplot(data = trend_results, aes(x = State.Name, y = Temp_Slope, fill = "Temperature")) +
          geom_bar(stat = "identity") +
          labs(x = "State", y = "Temperature Trend") +
          ggtitle("Temperature Trend Across States") +
          coord_flip() +
          scale_fill_manual(values = "orange")
      })
      output$case_count_trend_plot <- renderPlot({
        ggplot(data = trend_results, aes(x = State.Name, y = Case_Count_Slope, fill = "Case Count")) +
          geom_bar(stat = "identity") +
          labs(x = "State", y = "Case Count Trend") +
          ggtitle("Case Count Trend Across States") +
          coord_flip() +
          scale_fill_manual(values = "blue")
      })
    } else {
      output$predictions_table <- renderTable(NULL)
      output$temp_trend_plot <- renderPlot(NULL)
      output$case_count_trend_plot <- renderPlot(NULL)
    }
  })
  
  observeEvent(input$sendButton, {
    req(input$chatInput)
    
    api_url <- "https://api.openai.com/v1/engines/davinci/completions"
    api_key <- ""  
    headers <- httr::add_headers(`Authorization` = paste("Bearer", api_key))
    data <- list(prompt = input$chatInput, max_tokens = 150)
    
    response <- tryCatch({
      httr::POST(url = api_url, body = data, encode = "json", headers)
    }, error = function(e) {
      return(NULL)
    })
    
    if (!is.null(response) && response$status_code == 200) {
      result <- content(response, "text")
      output$chatResponse <- renderText({ result })
    } else {
      output$chatResponse <- renderText({ "Failed to get response from the API." })
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)