# Load required libraries
library(shiny)
library(DT)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Personalized Study Plan Generator"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Input Your Details"),
      numericInput("study_hours", "Available Study Hours per Day", value = 4, min = 1, max = 12),
      textInput("name", "Your Name", value = ""),
      textAreaInput("subjects", "Enter Subjects (separate by commas)", value = "Math, Science, English, History"),
      numericInput("days", "Number of Days to Study", value = 7, min = 1, max = 30),
      actionButton("generate", "Generate Study Plan")
    ),
    
    mainPanel(
      h3("Generated Study Plan"),
      DTOutput("study_plan_table")
    )
  )
)

# Define the server logic for the application
server <- function(input, output, session) {
  
  # Reactive expression to generate the study plan when the button is pressed
  study_plan_data <- eventReactive(input$generate, {
    # Get user inputs
    study_hours_per_day <- input$study_hours
    subjects <- unlist(strsplit(input$subjects, ","))
    num_days <- input$days
    
    # Calculate total study hours for the given days
    total_study_hours <- study_hours_per_day * num_days
    
    # Distribute hours evenly among subjects
    num_subjects <- length(subjects)
    hours_per_subject <- total_study_hours / num_subjects
    
    # Generate study plan table
    study_plan <- data.frame(
      Subject = subjects,
      Hours_Per_Day = rep(hours_per_subject, num_subjects),
      Total_Hours = rep(hours_per_subject * num_days, num_subjects),
      stringsAsFactors = FALSE
    )
    
    return(study_plan)
  })
  
  # Render the generated study plan table
  output$study_plan_table <- renderDT({
    plan <- study_plan_data()
    if (nrow(plan) > 0) {
      datatable(plan, options = list(pageLength = 5))
    } else {
      return(NULL)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

