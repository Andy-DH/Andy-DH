# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)
library(broom)
library(tidyr)
library(shinyjs)
library(bslib)
library(DT)

# Global variables for consistent styling
COLORS <- list(
  benign = "#3498db",
  malignant = "#e74c3c",
  background = "#ffffff",
  text = "#2c3e50"
)

# [Previous UI Components remain exactly the same through the ui definition]
header_ui <- function() {
  titlePanel(
    div(
      class = "d-flex align-items-center",
      tags$h1("Breast Cancer Analysis Dashboard", class = "mb-0"),
      tags$span(class = "badge badge-info ml-3", "Interactive Analytics")
    )
  )
}

sidebar_ui <- function() {
  sidebarPanel(
    tags$div(
      class = "sidebar-content",
      selectInput("plot_type", "Select Visualization:",
                  choices = list(
                    "Patient Demographics" = list(
                      "Age Distribution" = "age_plot",
                      "Time Series Analysis" = "time_series_plot"
                    ),
                    "Clinical Metrics" = list(
                      "Tumor Size Analysis" = "tumor_plot",
                      "Risk Factors" = "risk_plot"
                    ),
                    "Advanced Analytics" = list(
                      "Correlation Heatmap" = "correlation_plot",
                      "Logistic Regression" = "logistic_plot",
                      "Family History Impact" = "history_plot"
                    )
                  )),
      
      checkboxGroupInput("diagnosis_filter", "Filter by Diagnosis:",
                         choices = c("Benign", "Malignant"),
                         selected = c("Benign", "Malignant")),
      
      sliderInput("age_range", "Select Age Range:",
                  min = 0, max = 100,
                  value = c(0, 100),
                  step = 1),
      
      actionButton("reset_filters", "Reset Filters",
                   class = "btn-secondary btn-block mb-3"),
      
      downloadButton("download_data", "Download Filtered Data",
                     class = "btn-info btn-block"),
      
      tags$hr(),
      
      tags$div(
        class = "text-muted small",
        "Data last updated: ", tags$span(id = "last_update", textOutput("update_date"))
      )
    )
  )
}

main_panel_ui <- function() {
  mainPanel(
    tabsetPanel(
      id = "main_tabs",
      tabPanel("Visualization",
               div(
                 class = "visualization-wrapper",
                 uiOutput("error_message"),
                 div(
                   id = "plot_wrapper",
                   plotlyOutput("age_plot", height = "400px"),
                   plotlyOutput("tumor_plot", height = "400px"),
                   plotlyOutput("history_plot", height = "400px"),
                   plotlyOutput("risk_plot", height = "400px"),
                   plotOutput("correlation_plot", height = "600px"),
                   plotlyOutput("time_series_plot", height = "400px"),
                   plotlyOutput("logistic_plot", height = "400px")
                 ),
                 tags$div(
                   class = "plot-description mt-3",
                   uiOutput("plot_description")
                 )
               )
      ),
      tabPanel("Data Explorer",
               DTOutput("data_table"))
    )
  )
}

# Define UI
ui <- fluidPage(
  useShinyjs(),
  theme = bs_theme(
    version = 4,
    primary = COLORS$benign,
    secondary = COLORS$malignant,
    bg = COLORS$background,
    fg = COLORS$text
  ),
  
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto&display=swap');
      body {
        font-family: 'Roboto', sans-serif;
      }
      .sidebar-content { padding: 15px; }
      .visualization-wrapper { padding: 20px; }
      .plot-description { 
        background: #f8f9fa;
        padding: 15px;
        border-radius: 5px;
      }
    "))
  ),
  
  header_ui(),
  
  sidebarLayout(
    sidebar_ui(),
    main_panel_ui()
  )
)

# Server logic
server <- function(input, output, session) {
  # Load and preprocess data
  data <- reactive({
    clean_data <- read.csv("cleaned_breast_cancer_data.csv") %>%
      rename(TumorSize = `Tumor.Size..cm.`,
             InvNodes = `Inv.Nodes`) %>%
      mutate(
        Diagnosis = factor(Diagnosis, levels = c("Benign", "Malignant")),
        Year = as.numeric(as.character(Year))
      ) %>%
      filter(!is.na(Age), !is.na(TumorSize), !is.na(InvNodes))
    
    # Update age slider range
    updateSliderInput(session, "age_range",
                      min = floor(min(clean_data$Age, na.rm = TRUE)),
                      max = ceiling(max(clean_data$Age, na.rm = TRUE)),
                      value = c(floor(min(clean_data$Age, na.rm = TRUE)),
                                ceiling(max(clean_data$Age, na.rm = TRUE))))
    
    clean_data
  })
  
  # Filtered data
  filtered_data <- reactive({
    req(input$diagnosis_filter, input$age_range)
    
    data() %>%
      filter(
        Diagnosis %in% input$diagnosis_filter,
        Age >= input$age_range[1],
        Age <= input$age_range[2]
      )
  })
  
  # Error message output
  output$error_message <- renderUI({
    if (nrow(filtered_data()) == 0) {
      div(class = "alert alert-warning",
          "No data available for the selected filters.")
    }
  })
  
  # Age Distribution Plot
  output$age_plot <- renderPlotly({
    req(filtered_data())
    p <- ggplot(filtered_data(), aes(x = Age, fill = Diagnosis)) +
      geom_density(alpha = 0.6) +
      scale_fill_manual(values = c("Benign" = COLORS$benign, 
                                   "Malignant" = COLORS$malignant)) +
      theme_minimal() +
      labs(title = "Age Distribution by Diagnosis",
           x = "Age",
           y = "Density")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Tumor Size Analysis Plot
  output$tumor_plot <- renderPlotly({
    req(filtered_data())
    p <- ggplot(filtered_data(), 
                aes(x = Diagnosis, y = TumorSize, fill = Diagnosis)) +
      geom_boxplot() +
      scale_fill_manual(values = c("Benign" = COLORS$benign, 
                                   "Malignant" = COLORS$malignant)) +
      theme_minimal() +
      labs(title = "Tumor Size Distribution by Diagnosis",
           x = "Diagnosis",
           y = "Tumor Size (cm)")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Family History Impact Plot
  output$history_plot <- renderPlotly({
    req(filtered_data())
    history_summary <- filtered_data() %>%
      group_by(History, Diagnosis) %>%
      summarise(count = n(), .groups = 'drop') %>%
      group_by(History) %>%
      mutate(percentage = count / sum(count) * 100)
    
    p <- ggplot(history_summary, 
                aes(x = History, y = percentage, fill = Diagnosis)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Benign" = COLORS$benign, 
                                   "Malignant" = COLORS$malignant)) +
      theme_minimal() +
      labs(title = "Impact of Family History on Diagnosis",
           x = "Family History",
           y = "Percentage (%)")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Risk Factors Plot
  output$risk_plot <- renderPlotly({
    req(filtered_data())
    p <- ggplot(filtered_data(), 
                aes(x = TumorSize, y = InvNodes, color = Diagnosis)) +
      geom_point(alpha = 0.6) +
      scale_color_manual(values = c("Benign" = COLORS$benign, 
                                    "Malignant" = COLORS$malignant)) +
      theme_minimal() +
      labs(title = "Risk Factors: Tumor Size vs. Invaded Nodes",
           x = "Tumor Size (cm)",
           y = "Number of Invaded Nodes")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Correlation Plot
  output$correlation_plot <- renderPlot({
    req(filtered_data())
    numeric_data <- filtered_data() %>% 
      select(where(is.numeric)) %>%
      select(-Year)
    
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    
    corrplot(cor_matrix,
             method = "color",
             type = "upper",
             order = "hclust",
             addCoef.col = "black",
             tl.col = "black",
             tl.srt = 45,
             col = colorRampPalette(c("#67001f", "#b2182b", "#d6604d",
                                      "#f4a582", "#fddbc7", "#f7f7f7",
                                      "#d1e5f0", "#92c5de", "#4393c3",
                                      "#2166ac", "#053061"))(200))
  })
  
  # Time Series Analysis Plot
  output$time_series_plot <- renderPlotly({
    req(filtered_data())
    yearly_summary <- filtered_data() %>%
      group_by(Year, Diagnosis) %>%
      summarise(count = n(), .groups = 'drop')
    
    p <- ggplot(yearly_summary, aes(x = Year, y = count, color = Diagnosis)) +
      geom_line() +
      geom_point() +
      scale_color_manual(values = c("Benign" = COLORS$benign, 
                                    "Malignant" = COLORS$malignant)) +
      theme_minimal() +
      labs(title = "Cases Over Time by Diagnosis",
           x = "Year",
           y = "Number of Cases")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Logistic Regression Plot
  output$logistic_plot <- renderPlotly({
    req(filtered_data())
    model <- glm(Diagnosis ~ Age + TumorSize + InvNodes, 
                 data = filtered_data(), 
                 family = binomial)
    
    coefficients <- tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term != "(Intercept)")
    
    p <- ggplot(coefficients, aes(x = estimate, y = term)) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
      geom_point(size = 3, color = "#2c3e50") +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                     height = 0.2, color = "#e74c3c") +
      scale_x_log10() +
      theme_minimal() +
      labs(title = "Logistic Regression: Risk Factors for Malignancy",
           x = "Odds Ratio (log scale)",
           y = "Predictor")
    
    ggplotly(p) %>% config(displayModeBar = FALSE)
  })
  
  # Reset filters
  observeEvent(input$reset_filters, {
    updateCheckboxGroupInput(session, "diagnosis_filter",
                             selected = c("Benign", "Malignant"))
    updateSliderInput(session, "age_range",
                      value = c(floor(min(data()$Age, na.rm = TRUE)),
                                ceiling(max(data()$Age, na.rm = TRUE))))
  })
  
  # Plot visibility control
  observe({
    req(input$plot_type)
    plots <- c("age_plot", "tumor_plot", "history_plot", "risk_plot",
               "correlation_plot", "time_series_plot", "logistic_plot")
    
    lapply(plots, function(id) {
      if (id == input$plot_type) {
        showElement(id)
      } else {
        hideElement(id)
      }
    })
  })
  
  # Plot descriptions
  plot_descriptions <- list(
    age_plot = "Distribution of patient ages by diagnosis type, showing the relative frequency of benign and malignant cases across different age groups.",
    tumor_plot = "Box plot comparing tumor sizes between benign and malignant cases, including statistical quartiles and outliers.",
    history_plot = "Analysis of the relationship between family history and diagnosis outcome, displayed as percentage distribution.",
    risk_plot = "Scatter plot showing the relationship between tumor size and number of invaded nodes, colored by diagnosis.",
    correlation_plot = "Correlation matrix heatmap showing relationships between numerical variables in the dataset.",
    time_series_plot = "Temporal trend of diagnosed cases over time, split by diagnosis type.",
    logistic_plot = "Forest plot of logistic regression coefficients showing the odds ratios for different risk factors."
  )
  
  output$plot_description <- renderUI({
    req(input$plot_type)
    tags$p(class = "mb-0", plot_descriptions[[input$plot_type]])
  })
  
  # Data table
  output$data_table <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      filter = 'top',
      class = 'cell-border stripe'
    )
  })
  
  # Update date
  output$update_date <- renderText({
    format(file.info("cleaned_breast_cancer_data.csv")$mtime, "%B %d, %Y")
  })
}

# Initialize the Shiny app
shinyApp(ui = ui, server = server)
