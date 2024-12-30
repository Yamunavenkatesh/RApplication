# Load required libraries
library(shiny)
library(ggplot2)
library(DT)
library(lubridate)
library(shinyTime)  # Add shinyTime for timeInput

# Sample data for tracking vital signs
vital_data <- data.frame(
  Date = as.Date(c("2024-01-01", "2024-01-05", "2024-01-10", "2024-01-15", "2024-01-20")),
  BloodPressure = c(120, 118, 125, 130, 128),
  BloodSugar = c(95, 100, 110, 105, 98),
  Weight = c(70, 71, 72, 71, 70),
  stringsAsFactors = FALSE
)

# Define UI
ui <- fluidPage(
  titlePanel("Elderly Care Assistant"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Track Health Data"),
      
      dateInput("date", "Date:", value = Sys.Date()),
      numericInput("blood_pressure", "Blood Pressure (mmHg):", value = 120, min = 0),
      numericInput("blood_sugar", "Blood Sugar (mg/dL):", value = 100, min = 0),
      numericInput("weight", "Weight (kg):", value = 70, min = 0),
      
      actionButton("save_data", "Save Data"),
      hr(),
      
      h4("Medication Schedule"),
      textInput("medication_name", "Medication Name:", value = ""),
      timeInput("medication_time", "Time for Medication:", value = Sys.time()),  # Using timeInput
      actionButton("add_medication", "Add Medication"),
      
      hr(),
      h4("Medication Reminder"),
      tableOutput("medication_table")
    ),
    
    mainPanel(
      h3("Health Trends"),
      plotOutput("health_trends"),
      hr(),
      h4("Health Data Table"),
      DTOutput("health_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data for storing health records
  health_data <- reactiveVal(vital_data)
  
  # Reactive data for storing medication schedule
  medication_schedule <- reactiveVal(data.frame(
    Medication = character(0),
    Time = as.POSIXct(character(0)),
    stringsAsFactors = FALSE
  ))
  
  # Save health data
  observeEvent(input$save_data, {
    new_data <- data.frame(
      Date = input$date,
      BloodPressure = input$blood_pressure,
      BloodSugar = input$blood_sugar,
      Weight = input$weight,
      stringsAsFactors = FALSE
    )
    
    updated_data <- rbind(health_data(), new_data)
    health_data(updated_data)
    
    # Reset inputs
    updateDateInput(session, "date", value = Sys.Date())
    updateNumericInput(session, "blood_pressure", value = 120)
    updateNumericInput(session, "blood_sugar", value = 100)
    updateNumericInput(session, "weight", value = 70)
  })
  
  # Add medication reminder
  observeEvent(input$add_medication, {
    new_medication <- data.frame(
      Medication = input$medication_name,
      Time = input$medication_time,
      stringsAsFactors = FALSE
    )
    
    updated_schedule <- rbind(medication_schedule(), new_medication)
    medication_schedule(updated_schedule)
    
    # Reset medication inputs
    updateTextInput(session, "medication_name", value = "")
    updateTimeInput(session, "medication_time", value = Sys.time())
  })
  
  # Render health trends plot
  output$health_trends <- renderPlot({
    data <- health_data()
    if (nrow(data) > 0) {
      ggplot(data, aes(x = Date)) +
        geom_line(aes(y = BloodPressure, color = "Blood Pressure")) +
        geom_line(aes(y = BloodSugar, color = "Blood Sugar")) +
        geom_line(aes(y = Weight, color = "Weight")) +
        labs(title = "Health Trends Over Time", x = "Date", y = "Values") +
        scale_color_manual(values = c("Blood Pressure" = "red", "Blood Sugar" = "blue", "Weight" = "green")) +
        theme_minimal()
    }
  })
  
  # Render health data table
  output$health_table <- renderDT({
    datatable(health_data())
  })
  
  # Render medication schedule table
  output$medication_table <- renderTable({
    medication_schedule()
  })
}

# Run the app
shinyApp(ui = ui, server = server)


