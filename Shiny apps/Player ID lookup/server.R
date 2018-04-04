function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- bb_names
    if (input$lname != "All") {
      data <- data[data$nameLast == input$lname,]
    }
   data
  }))
  
}