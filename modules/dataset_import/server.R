# Load required packages
library(DT)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyFeedback)
library(dplyr)
library(gplots)
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
      output$dataset_preview <- renderDataTable(dataset())

      # Get dataset summary
      output$dataset_summary <- renderDataTable({
        req(dataset())
        # Compute summary statistics and convert to data.frame
        do.call(
          cbind,
          lapply(dataset() %>% select(where(
            is.numeric
          )), summary, digits = 4)
        )
      })

      # Plot dataset histogram
      output$dataset_histogram <- renderPlot({
        req(dataset())
        # Compute variables histogram
        par(mfrow = c(round(ncol(dataset()) / 8), 8))
        for (c in names(dataset())) {
          x <- dataset()[, c]
          hist(x, prob = TRUE, xlab = c, ylab = "", main = "", col = "ivory")
          lines(density(x), lwd = 2, col = "tomato")
          curve(dnorm(x, mean = mean(x), sd = sd(x)), from = min(x), to = max(x), add = TRUE, lwd = 2, col = "steelblue")
        }
      })

      # Plot dataset correlation
      output$dataset_correlation <- renderPlot({
        req(dataset())
        # Compute variables correlation
        pairs(
          dataset(),
          upper.panel = function(x, y, ...) {
            points(x = x, y = y, col = "grey")
            abline(coef(lm(y ~ x)), col = "tomato", lwd = 2)
          },
          lower.panel = function(x, y, ...) {
            par(usr = c(0, 1, 0, 1))
            text(x = 0.5, y = 0.5, round(cor(x, y), 2), cex = 2)
          }
        )
      })

      # Plot dataset heatmap
      output$dataset_heatmap <- renderPlot({
        req(dataset())
        # Compute variables heatmap
        rho <- cor(dataset())
        palette_breaks <- seq(0, 1, 0.1)
        par(oma = c(2, 2, 2, 1))
        heatmap.2(rho, scale = "none", trace = "none", revC = TRUE, breaks = palette_breaks)
      })

      # Return the dataset
      return(dataset)
    }
  )
}
