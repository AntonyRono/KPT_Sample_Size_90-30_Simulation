library(shiny)
library(ggplot2)
library(shinythemes)
library(readxl)

# UI
ui <- fluidPage(
  theme = shinytheme("united"),  # Apply a professional theme
  titlePanel("Sample Size Simulation for 90/30 Rule"),
  tags$style(HTML("
    .btn-block {margin-bottom: 20px;}
    /*.btn-success { background-color: #EB895F !important; border-color: #EB895F !important; }
    .btn-success:hover { background-color: #d0734e !important; border-color: #d0734e !important; } */
    .dataTables_wrapper .dataTables_paginate .paginate_button {
      color: #337ab7 !important;
      background-color: #f5f5f5 !important;
    }
      /* Table Styling and Ensure the table fits within the sidebar and text wraps */
    .sidebar-table {
      width: 100%;
      table-layout: fixed;
      word-wrap: break-word;
      border-collapse: collapse;
      font-size: 10px; 
    }
    .sidebar-table th, .sidebar-table td {
      padding: 10px;
      text-align: left;
      border: 1px solid #dddddd;
    }
    .sidebar-table th {
      background-color: #f2f2f2;
      font-weight: bold;
    }
    .sidebar-table tr:nth-child(even) {
      background-color: #f9f9f9;
    }
    .sidebar-table tr:hover {
      background-color: #f1f1f1;
    }
    
  ")),
  
  sidebarLayout(
    sidebarPanel(
      # Common inputs
      numericInput("z_score", "Confidence Level Z-Score:", value = 1.645, step = 0.001),
      numericInput("precision", "Relative Precision (%):", value = 30, step = 1),
      numericInput("population_size", "Population Size (Number of Data Points):", value = 1000, step = 10),
      
      # Simulation method selection
      radioButtons("method", "Simulation Method:",
                   choices = c("Parametric (Mixture Log-Normal)" = "parametric",
                               "Non-Parametric (Empirical Data)" = "non_parametric")),
      
      # Parametric inputs
      conditionalPanel(
        condition = "input.method == 'parametric'",
        numericInput("mean", "Mean of Non-Zero Values:", value = 5, step = 0.1),
        numericInput("sd", "Standard Deviation of Non-Zero Values:", value = 2, step = 0.1),
        numericInput("zero_inflation", "Zero Inflation Rate (%):", value = 80, min = 0, max = 100, step = 1)
      ),
      
      # Non-parametric file upload
      # conditionalPanel(
      #   condition = "input.method == 'non_parametric'",
      #   fileInput("data_file", "Upload Your Data (CSV):", accept = ".csv")
      # ),
      
      conditionalPanel(
        condition = "input.method == 'non_parametric'",
        fileInput("data_file", "Upload Your Data (CSV):", accept = c(".csv", ".xlsx")),
        uiOutput("fuel_selector")  # Dynamically generated fuel options
      ),
      
      # Submit button
      actionButton("run_simulation", "Run Simulation",class = "btn btn-success btn-block")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Results", 
                 uiOutput("optimal_sample_size"),
                 plotOutput("precision_plot"),
                 plotOutput("population_hist")
                 ),
        tabPanel("About", 
                 p("This app simulates the required sample size to achieve 30% precision 
                   (relative error) based on the 90/30 rule. Users can choose between 
                   parametric and non-parametric methods to simulate data. The app calculates
                   the optimal sample size, displays a histogram of the simulated population,
                   and visualizes the relationship between sample size and precision."))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive expression to read uploaded data and extract fuel columns
  fuel_columns <- reactive({
    req(input$data_file)  # Ensure a file is uploaded
    #data <- read.csv(input$data_file$datapath, header = TRUE)
    ext <- tools::file_ext(input$data_file$name)
    if (ext == "xlsx") {
      data <- read_xlsx(input$data_file$datapath,)
    } else if (ext == "csv") {
      data <- read.csv(input$data_file$datapath, header = TRUE)
    }
    colnames(data)[-1]  # Exclude the first column (identifier)
  })
  
  # Dynamically generate the fuel selector
  output$fuel_selector <- renderUI({
    req(fuel_columns())
    selectInput("selected_fuel", "Select Fuel to Simulate:", choices = fuel_columns())
  })
  
  # Reactive expression to extract the selected fuel data
  selected_fuel_data <- reactive({
    req(input$data_file, input$selected_fuel)
    ext <- tools::file_ext(input$data_file$name)
    if (ext == "xlsx") {
      data <- read_xlsx(input$data_file$datapath,)
    } else if (ext == "csv") {
      data <- read.csv(input$data_file$datapath, header = TRUE)
    }
    #data <- read.csv(input$data_file$datapath, header = TRUE)
    data[[input$selected_fuel]]  # Return the selected fuel column
  })
  
  
  # Reactive expression to simulate the population
  simulated_population <- reactive({
    req(input$run_simulation)
    
    if (input$method == "parametric") {
      # Parametric simulation
      zero_count <- round(input$population_size * input$zero_inflation / 100)
      non_zero_count <- input$population_size - zero_count
      zeros <- rep(0, zero_count)
      non_zeros <- rlnorm(non_zero_count, 
                          meanlog = log(input$mean^2 / sqrt(input$mean^2 + input$sd^2)), 
                          sdlog = sqrt(log(1 + (input$sd^2 / input$mean^2))))
      c(zeros, non_zeros)
      
    } else if (input$method == "non_parametric") {
      req(selected_fuel_data())
      selected_fuel_data()  # Use the uploaded data for the selected fuel
      
    }
  })
  
  # Reactive expression to calculate optimal sample size
  results <- reactive({
    population <- simulated_population()
    sample_sizes <- seq(10, 1000, by = 10)
    z <- input$z_score
    target_precision <- input$precision / 100

    relative_precisions <- sapply(sample_sizes, function(n) {
      sample <- sample(population, n, replace = TRUE)
      mean_sample <- mean(sample)
      sd_sample <- sd(sample)
      se <- sd_sample / sqrt(n)
      z * se / mean_sample
    })

    optimal_size <- min(sample_sizes[relative_precisions <= target_precision], na.rm = TRUE)

    list(
      sample_sizes = sample_sizes,
      relative_precisions = relative_precisions,
      optimal_size = optimal_size
    )
  })

  # Display optimal sample size
  output$optimal_sample_size <- renderUI({
    req(results())
    optimal_size <- results()$optimal_size
    
    div(
      style = "padding: 20px; border: 2px solid #0073C2FF; border-radius: 10px; 
            background-color: #f9f9f9; text-align: left; display: flex; align-items: center; 
            max-width: 600px; margin: 10px 0;",
      div(
        style = "flex: 0 0 150px; font-size: 48px; font-weight: bold; color: #E69F00; text-align: center;",
        span(optimal_size)
      ),
      div(
        style = "flex: 1; margin-left: 20px;",
        h3("Optimal Sample Size", style = "color: #0073C2FF; margin-bottom: 5px;"),
        p("This is the minimum sample size required to achieve 30% precision based on the 90/30 rule.",
          style = "font-size: 14px; color: #333; margin: 0;")
      )
    )
  })
  
  # Population histogram
  output$population_hist <- renderPlot({
    req(simulated_population())
    population <- simulated_population()
    ggplot(data.frame(Value = population), aes(x = Value)) +
      geom_histogram(bins = 30, fill = "#0073C2FF", color = "black", alpha = 0.7) +
      labs(title = "Histogram of Simulated Population", x = "Value", y = "Frequency") +
      theme_minimal()
  })
  
  # Precision plot
  output$precision_plot <- renderPlot({
    req(results())
    res <- results()
    ggplot(data.frame(Sample_Size = res$sample_sizes,
                      Relative_Precision = res$relative_precisions), aes(x = Sample_Size, y = Relative_Precision)) +
      geom_line(color = "#E69F00", size = 1) +
      geom_hline(yintercept = input$precision / 100, linetype = "dashed", color = "red") +
      labs(title = "Relative Precision vs. Sample Size",
           x = "Sample Size", y = "Relative Precision") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui, server)
