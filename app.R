# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

# Load the cleaned data
clean_data <- read.csv("cleaned_breast_cancer_data.csv")

# Ensure the Diagnosis column is a factor (if not already)
clean_data$Diagnosis <- factor(clean_data$Diagnosis, levels = c("Benign", "Malignant"))

# Define UI
ui <- fluidPage(
  titlePanel("Breast Cancer Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Select Visualization:",
                  choices = c("Age Distribution" = "age",
                              "Tumor Size Analysis" = "tumor",
                              "Family History Impact" = "history",
                              "Risk Factors" = "risk")),
      
      checkboxGroupInput("diagnosis", "Filter by Diagnosis:",
                         choices = c("Benign", "Malignant"),
                         selected = c("Benign", "Malignant")),
      
      sliderInput("age_range", "Age Range:",
                  min = 0, max = 100,
                  value = c(0, 100))
    ),
    
    mainPanel(
      plotlyOutput("main_plot"),
      verbatimTextOutput("statistics")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive expression to filter data based on user inputs
  filtered_data <- reactive({
    clean_data %>%
      filter(Diagnosis %in% input$diagnosis,
             Age >= input$age_range[1],
             Age <= input$age_range[2])
  })
  
  # Render the main plot
  output$main_plot <- renderPlotly({
    data <- filtered_data()
    
    if (input$plot_type == "age") {
      p <- ggplot(data, aes(x = Age, fill = Diagnosis)) +
        geom_histogram(binwidth = 5, position = "dodge") +
        theme_minimal() +
        labs(title = "Age Distribution by Diagnosis")
    }
    else if (input$plot_type == "tumor") {
      p <- ggplot(data, aes(x = Tumor.Size..cm., y = Age, color = Diagnosis)) +
        geom_point() +
        theme_minimal() +
        labs(title = "Tumor Size vs Age")
    }
    else if (input$plot_type == "history") {
      p <- ggplot(data, aes(x = History, fill = Diagnosis)) +
        geom_bar(position = "dodge") +
        theme_minimal() +
        labs(title = "Diagnosis Distribution by Family History")
    }
    else {
      p <- ggplot(data, aes(x = Tumor.Size..cm., fill = Diagnosis)) +
        geom_density(alpha = 0.5) +
        theme_minimal() +
        labs(title = "Risk Factor Analysis")
    }
    
    ggplotly(p)
  })
  
  # Render summary statistics
  output$statistics <- renderPrint({
    data <- filtered_data()
    summary(data)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
