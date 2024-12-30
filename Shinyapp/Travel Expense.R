# Load required libraries
library(shiny)
library(ggplot2)
library(DT)

# Define UI
ui <- fluidPage(
  titlePanel("Travel Expense Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Enter Travel Expenses"),
      
      textInput("trip_name", "Trip Name:", value = "Vacation"),
      dateInput("date", "Date:", value = Sys.Date()),
      selectInput("category", "Expense Category:",
                  choices = c("Flights", "Accommodation", "Meals", "Activities", "Transportation", "Other")),
      numericInput("amount", "Amount (in USD):", value = 0, min = 0),
      actionButton("add_expense", "Add Expense"),
      hr(),
      h4("Expense Summary"),
      DTOutput("expense_table")
    ),
    
    mainPanel(
      h3("Expenses by Category"),
      plotOutput("expense_plot")
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Reactive data frame to store expenses
  expenses <- reactiveVal(data.frame(
    Trip = character(0),
    Date = as.Date(character(0)),
    Category = character(0),
    Amount = numeric(0),
    stringsAsFactors = FALSE
  ))
  
  # Add expense to the data frame
  observeEvent(input$add_expense, {
    new_expense <- data.frame(
      Trip = input$trip_name,
      Date = input$date,
      Category = input$category,
      Amount = input$amount,
      stringsAsFactors = FALSE
    )
    
    # Update the reactive data frame
    current_expenses <- expenses()
    updated_expenses <- rbind(current_expenses, new_expense)
    expenses(updated_expenses)
    
    # Reset input fields
    updateTextInput(session, "trip_name", value = "")
    updateDateInput(session, "date", value = Sys.Date())
    updateSelectInput(session, "category", selected = "Flights")
    updateNumericInput(session, "amount", value = 0)
  })
  
  # Display the expenses in a table
  output$expense_table <- renderDT({
    datatable(expenses(), options = list(pageLength = 5))
  })
  
  # Plot expenses by category
  output$expense_plot <- renderPlot({
    expense_data <- expenses()
    if (nrow(expense_data) > 0) {
      ggplot(expense_data, aes(x = Category, y = Amount, fill = Category)) +
        geom_bar(stat = "identity") +
        labs(title = "Expenses by Category", y = "Amount (USD)", x = "Category") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
