library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)

# Define UI
ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  titlePanel("Advanced Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      # Data Upload
      fileInput("file1", "Choose CSV File", 
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
      radioButtons("quote", "Quote",
                   choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"), selected = '"'),
      actionButton("loadData", "Load Data", class = "btn-primary"),
      
      hr(),
      
      # Column Selection
      uiOutput("columnSelectUI"),
      actionButton("updateColumns", "Update Columns", class = "btn-primary"),
      
      hr(),
      
      # Filtering Section
      h4("Filtering"),
      uiOutput("filterUI"),
      actionButton("addFilter", "Add Filter", class = "btn-info"),
      actionButton("applyFilters", "Apply Filters", class = "btn-primary"),
      
      hr(),
      
      # Plotting Section
      h4("Plotting"),
      selectInput("variable", "Select Variable for Plot", NULL),
      selectInput("plotType", "Select Plot Type", 
                  choices = c("Histogram" = "hist", "Scatter Plot" = "scatter", "Box Plot" = "box")),
      uiOutput("customizeUI"),
      # New UI elements for plot sizing
      numericInput("plotWidth", "Plot Width (px)", value = 600, min = 200, max = 2000, step = 50),
      numericInput("plotHeight", "Plot Height (px)", value = 400, min = 200, max = 2000, step = 50),
      actionButton("plotBtn", "Generate Plot", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data", 
                 DTOutput("contents"),
                 downloadButton("downloadData", "Download Filtered Data", class = "btn-primary mt-2")
        ),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Plot", 
                 plotlyOutput("plot"),
                 downloadButton("downloadPlot", "Download Plot", class = "btn-primary mt-2")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    data = NULL,
    filters = list(),
    selectedColumns = NULL
  )
  
  # Load data
  observeEvent(input$loadData, {
    req(input$file1)
    rv$data <- read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote)
    rv$selectedColumns <- names(rv$data)
    updateSelectInput(session, "variable", choices = names(rv$data))
  })
  
  # Column selection UI
  output$columnSelectUI <- renderUI({
    req(rv$data)
    checkboxGroupInput("selectedColumns", "Select Columns to Display", 
                       choices = names(rv$data), selected = rv$selectedColumns)
  })
  
  # Update selected columns
  observeEvent(input$updateColumns, {
    rv$selectedColumns <- input$selectedColumns
  })
  
  # Filter UI
  output$filterUI <- renderUI({
    req(rv$data)
    lapply(seq_along(rv$filters), function(i) {
      fluidRow(
        column(4, selectInput(paste0("filterVar", i), "Variable", choices = names(rv$data))),
        column(4, uiOutput(paste0("filterValueUI", i))),
        column(4, actionButton(paste0("removeFilter", i), "Remove", class = "btn-danger btn-sm"))
      )
    })
  })
  
  # Add new filter
  observeEvent(input$addFilter, {
    newFilter <- list(variable = NULL, value = NULL)
    rv$filters <- c(rv$filters, list(newFilter))
  })
  
  # Remove filter
  observe({
    lapply(seq_along(rv$filters), function(i) {
      observeEvent(input[[paste0("removeFilter", i)]], {
        rv$filters <- rv$filters[-i]
      })
    })
  })
  
  # Dynamic filter value UI
  observe({
    lapply(seq_along(rv$filters), function(i) {
      output[[paste0("filterValueUI", i)]] <- renderUI({
        req(input[[paste0("filterVar", i)]])
        var <- input[[paste0("filterVar", i)]]
        if (is.numeric(rv$data[[var]])) {
          sliderInput(paste0("filterVal", i), "Value", 
                      min = min(rv$data[[var]], na.rm = TRUE),
                      max = max(rv$data[[var]], na.rm = TRUE),
                      value = c(min(rv$data[[var]], na.rm = TRUE),
                                max(rv$data[[var]], na.rm = TRUE)))
        } else {
          selectInput(paste0("filterVal", i), "Value", 
                      choices = c("All", unique(rv$data[[var]])),
                      selected = "All", multiple = TRUE)
        }
      })
    })
  })
  
  # Apply filters
  filteredData <- eventReactive(input$applyFilters, {
    req(rv$data)
    df <- rv$data
    for (i in seq_along(rv$filters)) {
      var <- input[[paste0("filterVar", i)]]
      val <- input[[paste0("filterVal", i)]]
      if (!is.null(var) && !is.null(val)) {
        if (is.numeric(df[[var]])) {
          df <- df %>% filter(get(var) >= val[1] & get(var) <= val[2])
        } else if (!"All" %in% val) {
          df <- df %>% filter(get(var) %in% val)
        }
      }
    }
    df %>% select(all_of(rv$selectedColumns))
  })
  
  # Render data table
  output$contents <- renderDT({
    datatable(filteredData(), 
              options = list(pageLength = 15, 
                             scrollX = TRUE,
                             autoWidth = TRUE,
                             columnDefs = list(list(width = '100px', targets = "_all"))),
              class = 'cell-border stripe')
  })
  
  # Render summary
  output$summary <- renderPrint({
    summary(filteredData())
  })
  
  # Plot customization UI
  output$customizeUI <- renderUI({
    req(input$plotType)
    if (input$plotType == "hist") {
      numericInput("bins", "Number of Bins", value = 30, min = 1)
    } else if (input$plotType == "scatter") {
      selectInput("yVariable", "Select Y Variable", choices = names(filteredData()))
    }
  })
  
  # Generate plot
  plotData <- eventReactive(input$plotBtn, {
    req(input$variable)
    p <- ggplot(filteredData(), aes_string(x = input$variable))
    if (input$plotType == "hist") {
      p <- p + geom_histogram(bins = input$bins, fill = "#78C2AD", color = "white")
    } else if (input$plotType == "scatter") {
      req(input$yVariable)
      p <- p + geom_point(aes_string(y = input$yVariable), color = "#78C2AD")
    } else if (input$plotType == "box") {
      p <- p + geom_boxplot(fill = "#78C2AD")
    }
    p <- p + theme_minimal() +
      theme(
        text = element_text(color = "#888"),
        axis.text = element_text(color = "#888"),
        axis.title = element_text(color = "#888"),
        plot.title = element_text(color = "#888")
      )
    ggplotly(p) %>% layout(autosize = F, width = input$plotWidth, height = input$plotHeight)
  })
  
  # Render plot
  output$plot <- renderPlotly({
    plotData()
  })
  
  # Download handlers
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("plot", ".png", sep = "") },
    content = function(file) {
      ggsave(file, plot = plotData(), width = input$plotWidth/72, height = input$plotHeight/72, units = "in")
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("filtered_data", ".csv", sep = "") },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)