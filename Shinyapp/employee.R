library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Employee Details Management"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Name"),
      textInput("department", "Department"),
      numericInput("salary", "Salary", value = 0, min = 0),
      dateInput("hire_date", "Hire Date", value = Sys.Date()),
      actionButton("add_button", "Add Employee"),
      br(),
      br(),
      actionButton("clear_button", "Clear All Employees")
    ),
    
    mainPanel(
      h3("Employee Details"),
      tableOutput("employee_table")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Initial employee details data frame
  employee_details <- reactiveVal(
    data.frame(
      EmployeeID = integer(),
      Name = character(),
      Department = character(),
      Salary = numeric(),
      HireDate = as.Date(character()),
      stringsAsFactors = FALSE
    )
  )
  
  # Function to add employee
  observeEvent(input$add_button, {
    print(input$hire_date)
    new_id <- ifelse(nrow(current_data) == 0, 1, max(current_data$EmployeeID) + 1)
    
    new_employee <- data.frame(
      EmployeeID = new_id,
      Name = input$name,
      Department = input$department,
      Salary = input$salary,
      HireDate = as.Date(input$hire_date),
      stringsAsFactors = FALSE
    )
    
    # Append the new employee to the current data
    employee_details(rbind(current_data, new_employee))
  })
  
  # Clear all employee details
  observeEvent(input$clear_button, {
    employee_details(
      data.frame(
        EmployeeID = integer(),
        Name = character(),
        Department = character(),
        Salary = numeric(),
        HireDate = as.Date(character()),
        stringsAsFactors = FALSE
      )
    )
  })
  
  # Render the table
  output$employee_table <- renderTable({
    employee_details()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
