library(shiny)
library(leaflet)
library(dplyr)
library(plotly)
library(openxlsx)
library(shinythemes)

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

#' Placeholder organization records until a real data source (e.g. an API) is connected.
#'
#' @return One row per organization. Coordinates are uniform random, so points can fall
#'   in oceans and do not match the sampled Country.
fetch_data <- function() {
  # Keeps the placeholder dataset identical across restarts and deployments
  set.seed(123)
  n_orgs <- 196
  data_list <- vector("list", n_orgs)

  for (i in seq_len(n_orgs)) {
    region <- sample(names(regions_countries), 1)
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
      Funding = sample(c("Grant X", "Institutional", "Crowdfunding", "Grant Y", "Grant Z"), 1),
      URL = paste0("https://example.org/org", i)
    )
  }

  df <- do.call(rbind, lapply(data_list, as.data.frame))
  df$DateCreated <- as.Date(df$DateCreated)
  df$Latitude <- as.numeric(as.character(df$Latitude))
  df$Longitude <- as.numeric(as.character(df$Longitude))
  df
}

# Built at startup rather than per session: the UI filter choices below are derived from it
data <- fetch_data()

#' Esri Light Gray basemap plus its place-name layer; neither needs an API key.
#'
#' Not CARTO Positron: CARTO now watermarks keyless tiles with "API KEY REQUIRED", and a key
#' would be exposed in public HTML. The place-name layer is empty below zoom 3.
#' @param map A leaflet map widget.
add_basemap <- function(map) {
  map %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addTiles(
      urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Reference/MapServer/tile/{z}/{y}/{x}",
      options = tileOptions(maxZoom = 16)
    )
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$style(type = "text/css", "
    html, body {width:100%;height:100%;margin:0;padding:0; font-size: 16px;}
    label, input, select, button, .shiny-input-container {
      font-size: 16px !important;
    }
    #map {width:100%;height:800px;}
    .leaflet-control.legend {
      background-color: #f8f9fa !important;
      border: 1px solid #ddd !important;
      padding: 10px !important;
      box-shadow: 0 0 15px rgba(0,0,0,0.2) !important;
      font-size: 14px !important;
      color: #333 !important;
    }
    .leaflet-control.legend .legend-title {
      font-weight: bold;
      margin-bottom: 5px;
    }
    .leaflet-control.legend .legend-label {
      margin-left: 5px;
    }
    .well {
      background-color: #f8f9fa;
      border: 1px solid #ddd;
      padding: 15px;
      border-radius: 4px;
      box-shadow: none;
    }
  "),
  titlePanel("Mapping Open Science Organizations"),
  sidebarLayout(
    sidebarPanel(
      div(class = "well",
          p("Interactive database and visualization tool for Open Science initiatives."),
          textInput("search", strong("Search Organizations:"), ""),
          selectInput("typeFilter", strong("Type of Initiative:"), choices = c("All", unique(data$Type)), selected = "All"),
          selectInput("disciplineFilter", strong("Discipline:"), choices = c("All", unique(data$Discipline)), selected = "All"),
          dateRangeInput("dateFilter", strong("Date of Creation:"), start = min(data$DateCreated), end = max(data$DateCreated)),
          selectInput("regionFilter", strong("Geographic Region:"), choices = c("All", unique(data$Region)), selected = "All"),
          uiOutput("countryUI"),
          selectInput("languageFilter", strong("Language of Initiative:"), choices = c("All", unique(data$Language)), selected = "All"),
          actionButton("searchBtn", "Search"),
          hr(),
          p("Community contributions and feedback are welcome."),
          actionButton("feedbackBtn", "Submit Feedback")
      ),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Map",
                 leafletOutput("map", width = "100%", height = "600px")
        ),
        tabPanel("Search Results",
                 div(style="height: 400px; overflow-y: scroll;", tableOutput("results")),
                 br(),
                 downloadButton("downloadCSV", "Download as CSV"),
                 downloadButton("downloadExcel", "Download as Excel"),
                 br(),
                 textOutput("totalResults")
        ),
        tabPanel("Dashboard Analytics",
                 h4("Summary Statistics"),
                 fluidRow(
                   column(6, plotlyOutput("typePlot", height = "400px")),
                   column(6, plotlyOutput("disciplinePlot", height = "400px"))
                 ),
                 fluidRow(
                   column(6, plotlyOutput("regionPlot", height = "400px")),
                   column(6, plotlyOutput("datePlot", height = "400px"))
                 )
        ),
        tabPanel("About",
                 p("The FORRT Open Science Community Mapping Initiative aims to systematically identify, document, and visualize Open Science (OS) communities worldwide. The goal is to create an interactive, crowdsourced, and dynamic database that enhances community building, metascience, and funding opportunities."),
                 p("This ShinyApp is developed for the FORRT Open Science Community Mapping Initiative."),
                 p("Data Integration: Combine and harmonize datasets (e.g., Skubera et al.’s Literature Search Database, Metadocencia’s Mapped Communities, Access2Perspectives Kumo Network, COSGN Database, FORRT’s Curated Resources, ICOR’s Project Library, IOI’s Infra Finder, Open Science Learning Gate, and Embassy of Good Science)."),
                 p("Data integration from multiple sources is ongoing."),
                 p("Enhancing open science practices and FAIR Principles Reproducible Research."),
                 p(markdown("This builds on Skubera Magda, Korbmacher Max, Evans Thomas Rhys, Azevedo Flavio and Pennington Charlotte R. 2025 International initiatives to enhance awareness and uptake of open research in psychology: a systematic mapping review. *R. Soc. Open Sci.* 12241726. https://doi.org/10.1098/rsos.241726"))
        )
      ),
      width = 9
    )
  )
)

# Names must match the Type values in fetch_data(); an unmatched type gets an NA marker colour
type_colors <- c(
  "Training" = "#e41a1c",
  "Research" = "#377eb8",
  "Policies" = "#4daf4a",
  "Educational Materials" = "#984ea3"
)

server <- function(input, output, session) {
  output$countryUI <- renderUI({
    req(input$regionFilter)
    countries <- if (input$regionFilter == "All") {
      unique(unlist(regions_countries))
    } else {
      regions_countries[[input$regionFilter]]
    }
    selectInput("countryFilter", "Country:",
                choices = c("All", countries), selected = "All")
  })

  filteredData <- reactive({
    df <- data

    if (nchar(input$search) > 0) {
      df <- df[grepl(input$search, df$Organization, ignore.case = TRUE), ]
    }

    if (input$typeFilter != "All") {
      df <- df[df$Type == input$typeFilter, ]
    }

    if (input$disciplineFilter != "All") {
      df <- df[df$Discipline == input$disciplineFilter, ]
    }

    df <- df[df$DateCreated >= input$dateFilter[1] & df$DateCreated <= input$dateFilter[2], ]

    if (input$regionFilter != "All") {
      df <- df[df$Region == input$regionFilter, ]
    }

    # countryFilter comes from renderUI, so it is NULL until that UI has rendered once
    if (!is.null(input$countryFilter) && input$countryFilter != "All") {
      df <- df[df$Country == input$countryFilter, ]
    }

    if (input$languageFilter != "All") {
      df <- df[df$Language == input$languageFilter, ]
    }

    # as.character() first: as.numeric() on a factor returns its level codes, not the values
    df <- df[!is.na(as.numeric(as.character(df$Latitude))) & !is.na(as.numeric(as.character(df$Longitude))), ]
    df$Latitude <- as.numeric(as.character(df$Latitude))
    df$Longitude <- as.numeric(as.character(df$Longitude))

    df$Color <- type_colors[df$Type]

    df
  })

  output$results <- renderTable({
    df <- filteredData()
    if (nrow(df) == 0) {
      return(data.frame(Message = "No results found"))
    }
    df %>%
      select(Organization, Type, Discipline, DateCreated, Region, Country, Language, Lead, Mission, Funding)
  })

  output$totalResults <- renderText({
    df <- filteredData()
    paste("Total results:", nrow(df))
  })
  
  output$map <- renderLeaflet({
    df <- filteredData()
    print(paste("Filtered data rows:", nrow(df)))  # Debug print
    # Clean data: remove rows with missing or invalid coordinates
    df <- df[!is.na(df$Longitude) & !is.na(df$Latitude) & df$Longitude >= -180 & df$Longitude <= 180 & df$Latitude >= -90 & df$Latitude <= 90, ]
    print(paste("Cleaned data rows:", nrow(df)))  # Debug print
    if (nrow(df) == 0) {
      leaflet() %>%
        add_basemap() %>%
        addControl("No data to display", position = "topright")
    } else {
      leaflet() %>%
        add_basemap() %>%
        addCircleMarkers(
          data = df,
          lng = ~Longitude,
          lat = ~Latitude,
          color = ~Color,
          radius = 5,
          popup = ~paste0("<b>", Organization, "</b><br>",
                          "Type: ", Type, "<br>",
                          "Discipline: ", Discipline, "<br>",
                          "Lead: ", Lead, "<br>",
                          "Mission: ", Mission, "<br>",
                          "Funding: ", Funding),
          clusterOptions = markerClusterOptions(),
          label = ~Organization,
          labelOptions = labelOptions(noHide = FALSE, direction = 'auto'),
          layerId = ~paste0("marker_", row.names(df))
        ) %>%
        addLegend(
          position = "bottomright",
          colors = unname(type_colors),
          labels = names(type_colors),
          title = "Initiative Type"
        )
    }
  })

  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click)) {
      leafletProxy("map") %>%
        setView(lng = click$lng, lat = click$lat, zoom = 8)
    }
  })

  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("search_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filteredData(), file, row.names = FALSE)
    }
  )

  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste("search_results_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(filteredData(), file)
    }
  )
  
  output$typePlot <- renderPlotly({
    df <- filteredData()
    type_counts <- table(df$Type)
    df_type <- data.frame(Type = names(type_counts), Count = as.numeric(type_counts))
    plot_ly(data = df_type, x = ~Type, y = ~Count, type = "bar",
            marker = list(color = 'rgba(49,130,189,0.7)')) %>%
      layout(title = paste("Type of Initiative (Total:", sum(type_counts), ")"),
             xaxis = list(title = "Type"), yaxis = list(title = "Count"))
  })

  output$disciplinePlot <- renderPlotly({
    df <- filteredData()
    disc_counts <- table(df$Discipline)
    df_disc <- data.frame(Discipline = names(disc_counts), Count = as.numeric(disc_counts))
    plot_ly(data = df_disc, x = ~Discipline, y = ~Count, type = "bar",
            marker = list(color = 'rgba(204,204,204,0.7)')) %>%
      layout(title = paste("Discipline (Total:", sum(disc_counts), ")"),
             xaxis = list(title = "Discipline"), yaxis = list(title = "Count"))
  })

  output$regionPlot <- renderPlotly({
    df <- filteredData()
    reg_counts <- table(df$Region)
    df_reg <- data.frame(Region = names(reg_counts), Count = as.numeric(reg_counts))
    plot_ly(data = df_reg, x = ~Region, y = ~Count, type = "bar",
            marker = list(color = 'rgba(222,45,38,0.7)')) %>%
      layout(title = paste("Region (Total:", sum(reg_counts), ")"),
             xaxis = list(title = "Region"), yaxis = list(title = "Count"))
  })

  output$datePlot <- renderPlotly({
    df <- filteredData()
    plot_ly(data = df, x = ~DateCreated, type = "histogram",
            marker = list(color = 'rgba(142,186,229,0.7)')) %>%
      layout(title = paste("Date Created (Total:", nrow(df), ")"),
             xaxis = list(title = "Date"), yaxis = list(title = "Count"))
  })
  
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
  
  observeEvent(input$submitFeedback, {
    feedback <- input$feedbackText
    # Placeholder: feedback reaches only the server log until storage or email delivery exists
    cat("User Feedback:", feedback, "\n")
    removeModal()
    showNotification("Thank you for your feedback!", type = "message")
  })
}

shinyApp(ui = ui, server = server, options = list(port = 8103))
