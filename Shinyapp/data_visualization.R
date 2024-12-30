# Load required libraries
library(shiny)
library(ggplot2)
library(DT)

# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Data Visualization Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File", accept = c(".csv")),
      uiOutput("columns_ui"),
      selectInput("plot_type", "Select Plot Type", 
                  choices = c("Scatter Plot", "Bar Plot", "Line Plot")),
      actionButton("generate_plot", "Generate Plot")
    ),
    
    mainPanel(
      plotOutput("data_plot"),
      DTOutput("data_table")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Reactive data object to store the uploaded dataset
  uploaded_data <- reactive({
    req(input$file)  # Ensure a file is uploaded
    read.csv(input$file$datapath, header = TRUE, stringsAsFactors = FALSE)
  })
  
  # Dynamically generate column selection UI
  output$columns_ui <- renderUI({
    req(uploaded_data())
    data <- uploaded_data()
    tagList(
      selectInput("x_col", "X-axis Column", choices = names(data)),
      selectInput("y_col", "Y-axis Column", choices = names(data))
    )
  })
  
  # Generate the plot based on user inputs
  output$data_plot <- renderPlot({
    req(uploaded_data(), input$x_col, input$y_col, input$plot_type)
    data <- uploaded_data()
    
    p <- ggplot(data, aes_string(x = input$x_col, y = input$y_col))
    
    if (input$plot_type == "Scatter Plot") {
      p <- p + geom_point()
    } else if (input$plot_type == "Bar Plot") {
      p <- p + geom_bar(stat = "identity")
    } else if (input$plot_type == "Line Plot") {
      p <- p + geom_line()
    }
    
    p + theme_minimal() + labs(title = paste(input$plot_type, "of", input$x_col, "vs", input$y_col))
  })
  
  # Display the uploaded dataset as a table
  output$data_table <- renderDT({
    req(uploaded_data())
    datatable(uploaded_data(), options = list(pageLength = 10))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
