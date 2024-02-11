# Load required packages
library(DT)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyFeedback)
library(dplyr)
library(readxl)
library(readODS)

# Define dataset_import server function
datasetImportServer <- function(id = "dataset_import") {
  moduleServer(
    id,
    function(input, output, session) {
      # Define dataset reactive loader
      dataset <- reactive({
        # Check if file uploaded correctly
        req(input$file)
        file <- input$file$datapath
        # Try to parse uploaded file
        tryCatch(
          {
            hideFeedback("format")
            hideFeedback("file")
            df <- switch(input$format,
              ".csv" = {
                read.csv(file, header = input$header)
              },
              ".xls" = {
                data.frame(
                    read_excel(
                      file,
                      sheet = 1,
                      col_names = input$header
                    )
                )
              },
              ".xlsx" = {
                data.frame(
                    read_excel(
                      file,
                      sheet = 1,
                      col_names = input$header
                    )
                )
              },
              ".ods" = {
                read_ods(
                  file,
                  sheet = 1,
                  col_names = input$header
                )
              }
            )
            if (input$index) {
              df[1] <- NULL
            }
            showFeedbackSuccess("format")
            showFeedbackSuccess("file")
            return(df)
          },
          error = function(e) {
            showFeedbackDanger("format")
            showFeedbackDanger("file")
            showModal(
              modalDialog(
                safeError(e),
                title = "Oops! Something went wrong!",
                footer = modalButton("Dismiss")
              )
            )
          }
        )
      })

      # Get dataset head as preview
      output$preview <- renderDataTable(dataset())

      # Get dataset summary
      output$summary <- renderDataTable({
        req(dataset())
        # Compute summary statistics and convert to data.frame
        do.call(
          cbind,
          lapply(dataset() %>% select(where(
            is.numeric
          )), summary, digits = 4)
        )
      })

      # Return the dataset
      return(dataset)
    }
  )
}
