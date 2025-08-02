# Mapping Open Science Organizations

This project is an interactive Shiny application developed for the FORRT Open Science Community Mapping Initiative. The goal is to create a user-friendly, searchable, and accessible platform to visualize and disseminate data on Open Science initiatives globally.

## Features and Functionality

### Interactive Search and Filtering
- **Organization Search**: Search for specific open science organizations by name
- **Type Filter**: Filter organizations by initiative type (Training, Research, Policies, Educational Materials)
- **Discipline Filter**: Narrow down results by academic discipline (Biology, Physics, Social Sciences, etc.)
- **Date Range Filter**: Find organizations created within specific time periods
- **Geographic Filters**: 
  - Region Filter: Filter by geographic region (Europe, North America, South America, Asia, Africa, Oceania)
  - Country Filter: Select specific countries (dynamically updates based on region selection)
- **Language Filter**: Filter initiatives by language

### Visualization Tools
- **Interactive World Map**: Visualize organization locations with marker clustering
- **Popup Information**: Click on map markers to view detailed organization information
- **Search Results Table**: Comprehensive tabular view of filtered results with key details
- **Dashboard Analytics**: Statistical overview of organizations including:
  - Total count of organizations
  - Distribution by initiative type
  - Breakdown by discipline
  - Geographic distribution by region

### Community Engagement
- **Feedback System**: Built-in mechanism for users to submit feedback and suggestions
- **Community Contributions**: Framework for integrating community-submitted data

### Technical Features
- **Responsive Design**: Adapts to different screen sizes and devices
- **Real-time Filtering**: Instant updates as filters are applied
- **Data Consistency**: Proper region-country associations ensuring accurate geographic filtering

## Getting Started

### Prerequisites

- R (version 4.0 or higher recommended)
- RStudio (optional but recommended)
- Required R packages:
  - shiny
  - leaflet
  - dplyr

Install required packages with:
```R
install.packages(c("shiny", "leaflet", "dplyr"))
```

### Running the App

Open the project in RStudio and run the `app.R` file, or use the following command in R:

```R
shiny::runApp("mapping-open-science-organizations")
```

To run on a specific port:
```R
shiny::runApp(port=3838)
```

### Data Structure

The application uses simulated data representing 196 open science organizations with the following attributes:
- Organization name
- Initiative type
- Academic discipline
- Creation date
- Geographic region and country
- Language
- Geographic coordinates (latitude/longitude)
- Lead contact information
- Mission statement
- Funding source

## Contributing

We welcome contributions from the Open Science community. Please submit issues, feature requests, or pull requests to improve the app.

### Areas for Development
- Integration of real-world coordinated data from multiple sources
- Enhanced visualization capabilities
- Additional filtering options
- Improved data submission and validation processes

## License

This project is open source and available under the MIT License.
shiny::runApp("mapping-open-science-organizations")
