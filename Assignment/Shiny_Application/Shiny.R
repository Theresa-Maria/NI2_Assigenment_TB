#Make a Shiny for Allergy Metadata

#Install and load the necessary libraries
install.packages("shiny")
install.packages("shinythemes")
library("shiny")
library("readxl")
library("ggplot2")
library("dplyr")
library("shinythemes")


#code the Shiny application
ui <- fluidPage(
  tags$style(HTML("
    body {
      background-color: lavenderblush;
    }
     .title-panel {
      background-color: violet;
      color: white;
      padding: 10px;
      text-align: center;
    }
  ")),
  theme = shinytheme("cerulean"),
  
  titlePanel(
    title=div("Pink Shiny App for Data Visualization", class = "title-panel")
    ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("outcome", "Outcome", choices = c("All", unique(data$Outcome)), selected = "All"),
      selectInput("bat", "BAT", choices = c("All", unique(data$BAT)), selected = "All"),
      selectInput("threshold", "Threshold", choices = c("All", unique(data$Threshold)), selected = "All"),
      selectInput("symptoms", "Symptoms", choices = c("All", unique(data$Symptoms)), selected = "All"),
      width = 3 
    ),
    
    mainPanel(
      tableOutput("filteredTable"),
      br(),
      plotOutput("dataPlot"),
      width = 9 
    )
  )
)

server <- function(input, output) {
  
  filteredData <- reactive({
    df <- data
    if (input$outcome != "All") {
      df <- df %>% filter(Outcome == input$outcome)
    }
    if (input$bat != "All") {
      df <- df %>% filter(BAT == input$bat)
    }
    if (input$threshold != "All") {
      df <- df %>% filter(Threshold == input$threshold)
    }
    if (input$symptoms != "All") {
      df <- df %>% filter(Symptoms == input$symptoms)
    }
    df
  })
  
  output$filteredTable <- renderTable({
    filteredData()
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$dataPlot <- renderPlot({
    df <- filteredData()
    
    # Count occurrences of each Outcome category
    outcome_counts <- df %>%
      count(Outcome) %>%
      mutate(Outcome = factor(Outcome, levels = c("pos", "neg")))
    
    ggplot(outcome_counts, aes(x = Outcome, y = n, fill = Outcome)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_manual(values = c("pos" = "purple", "neg" = "plum")) +
      labs(title = "Count of Each Outcome Category",
           x = "Outcome",
           y = "Count") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid.major = element_line(size = 0.5, color = "gray"),
        panel.grid.minor = element_line(size = 0.25, color = "gray")
      )
  })
}

shinyApp(ui = ui, server = server)
