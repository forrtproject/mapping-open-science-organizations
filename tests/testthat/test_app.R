library(shinytest2)
test_that("Open Science app works correctly", {
  app <- AppDriver$new(name = "open_science_app", height = 800, width = 1200)
  
  # Initial state: map tab visible
  expect_true(app$get_value(input = "search") == "")
  
  # Test filters and search
  app$set_inputs(search = "Open Science Org 1")
  app$click("searchBtn")
  Sys.sleep(2) # wait for reactive update
  
  # Check search results table has rows
  results <- app$get_value(output = "results")
  expect_true(nrow(results) > 0)
  
  # Test download buttons exist
  expect_true(app$exists("downloadCSV"))
  expect_true(app$exists("downloadExcel"))
  
  # Navigate to Dashboard Analytics tab
  app$set_inputs(`:shiny-tab` = "Dashboard Analytics")
  Sys.sleep(1)
  
  # Check plots exist
  expect_true(app$exists("typePlot"))
  expect_true(app$exists("disciplinePlot"))
  expect_true(app$exists("regionPlot"))
  expect_true(app$exists("datePlot"))
  
  # Navigate to About tab
  app$set_inputs(`:shiny-tab` = "About")
  Sys.sleep(1)
  about_text <- app$get_value(output = "About")
  expect_true(grepl("FORRT Open Science Community Mapping Initiative", about_text))
  
  # Open feedback modal
  app$click("feedbackBtn")
  Sys.sleep(1)
  expect_true(app$exists("feedbackText"))
  
  # Submit feedback
  app$set_inputs(feedbackText = "Test feedback")
  app$click("submitFeedback")
  Sys.sleep(1)
  
  # Check notification
  notif <- app$get_notifications()
  expect_true(any(grepl("Thank you for your feedback", notif$message)))
  
  app$stop()
})
