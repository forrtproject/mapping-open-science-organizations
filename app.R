library(shiny)
library(leaflet)
library(dplyr)

# Sample data frame simulating integrated datasets with metadata
# In practice, this data would be loaded from external sources and harmonized
data <- data.frame(
  Organization = paste("Open Science Org", seq(1, 196)),
  Type = sample(c("Training", "Research", "Policies", "Educational Materials"), 196, replace = TRUE),
  Discipline = sample(c("Biology", "Physics", "Social Sciences", "Chemistry", "Mathematics", "Computer Science", "Environmental Science", "Medicine", "Engineering", "Psychology"), 196, replace = TRUE),
  DateCreated = sample(seq(as.Date('2010-01-01'), as.Date('2023-01-01'), by="day"), 196, replace = TRUE),
  Region = sample(c("Europe", "North America", "South America", "Asia", "Africa", "Oceania"), 196, replace = TRUE),
  Country = c(
    "Afghanistan", "Albania", "Algeria", "Andorra", "Angola", "Antigua and Barbuda", "Argentina",
    "Armenia", "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh",
    "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bhutan", "Bolivia", "Bosnia and Herzegovina",
    "Botswana", "Brazil", "Brunei", "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde", "Cambodia",
    "Cameroon", "Canada", "Central African Republic", "Chad", "Chile", "China", "Colombia", "Comoros",
    "Congo", "Costa Rica", "Croatia", "Cuba", "Cyprus", "Czech Republic", "Denmark", "Djibouti", "Dominica",
    "Dominican Republic", "DR Congo", "Ecuador", "Egypt", "El Salvador", "Equatorial Guinea", "Eritrea",
    "Estonia", "Eswatini", "Ethiopia", "Fiji", "Finland", "France", "Gabon", "Gambia", "Georgia", "Germany",
    "Ghana", "Greece", "Grenada", "Guatemala", "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Honduras",
    "Hungary", "Iceland", "India", "Indonesia", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Ivory Coast",
    "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", "Kuwait", "Kyrgyzstan", "Laos",
    "Latvia", "Lebanon", "Lesotho", "Liberia", "Libya", "Liechtenstein", "Lithuania", "Luxembourg",
    "Madagascar", "Malawi", "Malaysia", "Maldives", "Mali", "Malta", "Marshall Islands", "Mauritania",
    "Mauritius", "Mexico", "Micronesia", "Moldova", "Monaco", "Mongolia", "Montenegro", "Morocco",
    "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "New Zealand", "Nicaragua",
    "Niger", "Nigeria", "North Korea", "North Macedonia", "Norway", "Oman", "Pakistan", "Palau",
    "Palestine", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Poland", "Portugal",
    "Qatar", "Romania", "Russia", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia",
    "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia",
    "Senegal", "Serbia", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia",
    "Solomon Islands", "Somalia", "South Africa", "South Korea", "South Sudan", "Spain", "Sri Lanka",
    "Sudan", "Suriname", "Sweden", "Switzerland", "Syria", "Taiwan", "Tajikistan", "Tanzania",
    "Thailand", "Timor-Leste", "Togo", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey",
    "Turkmenistan", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", "USA",
    "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City", "Venezuela", "Vietnam", "Yemen", "Zambia",
    "Zimbabwe"
  ),
  Language = sample(c("English", "French", "Spanish", "Mandarin", "Hindi", "Arabic"), 196, replace = TRUE),
  Latitude = runif(196, -90, 90),
  Longitude = runif(196, -180, 180),
  Lead = paste("Lead", seq(1, 196)),
  Mission = paste("Mission", seq(1, 196)),
  Funding = sample(c("Grant X", "Institutional", "Crowdfunding", "Grant Y", "Grant Z"), 196, replace = TRUE),
  stringsAsFactors = FALSE
)

# Define UI for application
ui <- fluidPage(
  titlePanel("Mapping Open Science Organizations"),
  sidebarLayout(
    sidebarPanel(
      helpText("Interactive database and visualization tool for Open Science initiatives."),
      textInput("search", "Search Organizations:", ""),
      selectInput("typeFilter", "Type of Initiative:",
                  choices = c("All", unique(data$Type)), selected = "All"),
      selectInput("disciplineFilter", "Discipline:",
                  choices = c("All", unique(data$Discipline)), selected = "All"),
      dateRangeInput("dateFilter", "Date of Creation:",
                     start = min(data$DateCreated), end = max(data$DateCreated)),
      selectInput("regionFilter", "Geographic Region:",
                  choices = c("All", unique(data$Region)), selected = "All"),
      uiOutput("countryUI"),
      selectInput("languageFilter", "Language of Initiative:",
                  choices = c("All", unique(data$Language)), selected = "All"),
      actionButton("searchBtn", "Search"),
      hr(),
      helpText("Community contributions and feedback are welcome."),
      actionButton("feedbackBtn", "Submit Feedback")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 leafletOutput("map", height = 600)
        ),
        tabPanel("Search Results",
                 tableOutput("results")
        ),
        tabPanel("Dashboard Analytics",
                 h4("Summary Statistics"),
                 verbatimTextOutput("summaryStats")
        ),
        tabPanel("About",
                 p("This ShinyApp is developed for the FORRT Open Science Community Mapping Initiative."),
                 p("Data integration from multiple sources is ongoing."),
                 p("Enhancing open science practices and FAIR Principles Reproducible Research.")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Dynamic UI for country filter based on selected region
  output$countryUI <- renderUI({
    req(input$regionFilter)
    if (input$regionFilter == "All") {
      selectInput("countryFilter", "Country:",
                  choices = c("All", unique(data$Country)), selected = "All")
    } else {
      countries <- unique(data$Country[data$Region == input$regionFilter])
      selectInput("countryFilter", "Country:",
                  choices = c("All", countries), selected = "All")
    }
  })
  
  # Reactive expression for filtered data
  filteredData <- eventReactive(input$searchBtn, {
    df <- data
    
    # Filter by search term
    if (nchar(input$search) > 0) {
      df <- df[grepl(input$search, df$Organization, ignore.case = TRUE), ]
    }
    
    # Filter by type
    if (input$typeFilter != "All") {
      df <- df[df$Type == input$typeFilter, ]
    }
    
    # Filter by discipline
    if (input$disciplineFilter != "All") {
      df <- df[df$Discipline == input$disciplineFilter, ]
    }
    
    # Filter by date range
    df <- df[df$DateCreated >= input$dateFilter[1] & df$DateCreated <= input$dateFilter[2], ]
    
    # Filter by region
    if (input$regionFilter != "All") {
      df <- df[df$Region == input$regionFilter, ]
    }
    
    # Filter by country
    if (!is.null(input$countryFilter) && input$countryFilter != "All") {
      df <- df[df$Country == input$countryFilter, ]
    }
    
    # Filter by language
    if (input$languageFilter != "All") {
      df <- df[df$Language == input$languageFilter, ]
    }
    
    df
  }, ignoreNULL = FALSE)
  
  # Render search results table
  output$results <- renderTable({
    df <- filteredData()
    if (nrow(df) == 0) {
      return(data.frame(Message = "No results found"))
    }
    df %>%
      select(Organization, Type, Discipline, DateCreated, Region, Country, Language, Lead, Mission, Funding)
  })
  
  # Render leaflet map
  output$map <- renderLeaflet({
    df <- filteredData()
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        popup = ~paste0("<b>", Organization, "</b><br>",
                        "Type: ", Type, "<br>",
                        "Discipline: ", Discipline, "<br>",
                        "Lead: ", Lead, "<br>",
                        "Mission: ", Mission, "<br>",
                        "Funding: ", Funding),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Render dashboard analytics summary
  output$summaryStats <- renderPrint({
    df <- filteredData()
    cat("Total Organizations:", nrow(df), "\n")
    cat("Types of Initiatives:\n")
    print(table(df$Type))
    cat("\nDisciplines:\n")
    print(table(df$Discipline))
    cat("\nRegions:\n")
    print(table(df$Region))
  })
  
  # Placeholder for feedback modal
  observeEvent(input$feedbackBtn, {
    showModal(modalDialog(
      title = "Submit Feedback",
      textAreaInput("feedbackText", "Your feedback:", "", width = "100%", height = "100px"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("submitFeedback", "Submit")
      )
    ))
  })
  
  # Handle feedback submission
  observeEvent(input$submitFeedback, {
    feedback <- input$feedbackText
    # For now, just print feedback to console (to be replaced with storage or email)
    cat("User Feedback:", feedback, "\n")
    removeModal()
    showNotification("Thank you for your feedback!", type = "message")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
