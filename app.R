library(shiny)
library(leaflet)
library(dplyr)

# Sample data frame simulating integrated datasets with metadata
# In practice, this data would be loaded from external sources and harmonized

# Define regions and their corresponding countries
regions_countries <- list(
  "Europe" = c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina",
               "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia",
               "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland",
               "Italy", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta",
               "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia",
               "Norway", "Poland", "Portugal", "Romania", "San Marino", "Serbia",
               "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom"),
  "North America" = c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", "Canada",
                      "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "El Salvador",
                      "Grenada", "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico",
                      "Nicaragua", "Panama", "Saint Kitts and Nevis", "Saint Lucia",
                      "Saint Vincent and the Grenadines", "Trinidad and Tobago", "USA"),
  "South America" = c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
                      "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela"),
  "Asia" = c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan",
             "Brunei", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia",
             "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait",
             "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Maldives", "Mongolia",
             "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan", "Palestine",
             "Philippines", "Qatar", "Russia", "Saudi Arabia", "Singapore", "South Korea",
             "Sri Lanka", "Syria", "Taiwan", "Tajikistan", "Thailand", "Timor-Leste",
             "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", "Yemen"),
  "Africa" = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
               "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros",
               "Congo", "Djibouti", "DR Congo", "Egypt", "Equatorial Guinea", "Eritrea",
               "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau",
               "Ivory Coast", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi",
               "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger",
               "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles",
               "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania",
               "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe"),
  "Oceania" = c("Australia", "Fiji", "Kiribati", "Marshall Islands", "Micronesia",
                "Nauru", "New Zealand", "Palau", "Papua New Guinea", "Samoa",
                "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu")
)

# Generate data with proper region-country associations
set.seed(123)  # For reproducibility
n_orgs <- 196
data_list <- list()

for (i in 1:n_orgs) {
  # Randomly select a region
  region <- sample(names(regions_countries), 1)
  # Select a country from that region
  country <- sample(regions_countries[[region]], 1)
  
  data_list[[i]] <- list(
    Organization = paste("Open Science Org", i),
    Type = sample(c("Training", "Research", "Policies", "Educational Materials"), 1),
    Discipline = sample(c("Biology", "Physics", "Social Sciences", "Chemistry", "Mathematics", 
                          "Computer Science", "Environmental Science", "Medicine", "Engineering", 
                          "Psychology"), 1),
    DateCreated = sample(seq(as.Date('2010-01-01'), as.Date('2023-01-01'), by="day"), 1),
    Region = region,
    Country = country,
    Language = sample(c("English", "French", "Spanish", "Mandarin", "Hindi", "Arabic"), 1),
    Latitude = runif(1, -90, 90),
    Longitude = runif(1, -180, 180),
    Lead = paste("Lead", i),
    Mission = paste("Mission", i),
    Funding = sample(c("Grant X", "Institutional", "Crowdfunding", "Grant Y", "Grant Z"), 1)
  )
}

# Convert to data frame
data <- do.call(rbind, lapply(data_list, as.data.frame))
data$DateCreated <- as.Date(data$DateCreated)
data$Latitude <- as.numeric(as.character(data$Latitude))
data$Longitude <- as.numeric(as.character(data$Longitude))

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
