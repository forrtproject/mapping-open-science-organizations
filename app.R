library(shiny)

# Define UI for application
ui <- fluidPage(
  titlePanel("Mapping Open Science Organizations"),
  sidebarLayout(
    sidebarPanel(
      helpText("Interactive database and visualization tool for Open Science initiatives."),
      textInput("search", "Search Organizations:", ""),
      actionButton("searchBtn", "Search"),
      hr(),
      helpText("Community contributions and feedback are welcome.")
    ),
    mainPanel(
      h3("Search Results"),
      tableOutput("results"),
      hr(),
      h4("About"),
      p("This ShinyApp is developed for the FORRT Open Science Community Mapping Initiative."),
      p("Data integration from multiple sources will be added soon."),
      p("Enhencing open science practices and FAIR Principles Reproducible Research.")
      
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Placeholder reactive data frame for search results
  data <- reactiveVal(data.frame(
    Organization = character(),
    Description = character(),
    stringsAsFactors = FALSE
  ))
  
  observeEvent(input$searchBtn, {
    # For now, just echo the search term as a placeholder
    search_term <- input$search
    if (nchar(search_term) > 0) {
      # Placeholder: return a dummy row with the search term
      data(data.frame(
        Organization = paste("Result for:", search_term),
        Description = "Description will be added here.",
        stringsAsFactors = FALSE
      ))
    } else {
      data(data.frame(
        Organization = character(),
        Description = character(),
        stringsAsFactors = FALSE
      ))
    }
  })
  
  output$results <- renderTable({
    data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
