

fluidPage(
  titlePanel("Search for Player ID"),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectizeInput("lname",
                       "Last Name:",
                       c("All",
                         as.character(bb_names$nameLast)))
    )
  ),
  # Create a new row for the table.
  fluidRow(
    DT::dataTableOutput("table")
  )
)
