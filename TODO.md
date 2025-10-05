# TODO for Mapping Open Science Organizations App

## [x] 1. Define Color Palette for Initiative Types
- Create a color mapping for each unique "Type" (e.g., Training, Research, Policies, Educational Materials).
- Use distinct colors for each type.

## [x] 2. Update Leaflet Map Markers
- Modify addCircleMarkers to use color based on the "Type" column.
- Ensure colors are applied correctly.

## [x] 3. Add Legend to the Map
- Add a legend control to the leaflet map showing the color for each type.
- Position the legend appropriately.

## [x] 4. Add Interactivity for Zoom and Highlight
- On marker click, zoom the map to the marker's location.
- Highlight surrounding markers (e.g., change opacity or add a radius circle around nearby markers).

## [x] 5. Improve Responsiveness and Labels
- Ensure popups are detailed and responsive.
- Add marker labels if needed for better UX.
- Test the app to ensure it works as expected.

## Additional Notes from Feedback
- [x] Add library(plotly) to app.R
- [x] Update UI for Dashboard Analytics tab with responsive layout using fluidRow and columns for plotlyOutput elements
- [x] Add renderPlotly outputs in server for typePlot, disciplinePlot, regionPlot, datePlot
- [x] Run the app to test the changes
- [x] Add export functionality to Search Results tab (CSV, Excel, PDF)
- [x] Make general dashboard more responsive and appealing (add themes, colors, better layout)
- [x] Fix map not displaying by changing filteredData to reactive, removing search button, fixing leaflet data passing, ensuring lat/lng are numeric
- [x] Add scroll bar to Search Results table
- [x] Add total results indicator below download buttons in Search Results
