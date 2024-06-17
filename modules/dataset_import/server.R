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
            # Hide feedback
            hideFeedback("dtype")
            hideFeedback("format")
            hideFeedback("file")
            # Read uploaded file
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
            # Remove index column if present
            if (input$index) {
              df[1] <- NULL
            }
            # Set data type
            attributes(df)$is_continuous <- TRUE
            # Check data type
            if (input$dtype == "discrete") {
              # Apply `as.factor` to all columns
              df <- df %>% mutate_all(as.factor)
              attributes(df)$is_continuous <- FALSE
            }
            # Show feedback
            showFeedbackSuccess("dtype")
            showFeedbackSuccess("format")
            showFeedbackSuccess("file")
            # Return dataset
            return(df)
          },
          error = function(e) {
            showFeedbackDanger("dtype")
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
      output$dataset_preview <- DT::renderDT(dataset())

      # Get dataset summary
      output$dataset_summary <- DT::renderDT({
        # Check for non-null dataset
        req(dataset())
        # Compute summary statistics and convert to data.frame
        do.call(cbind, lapply(dataset(), summary, digits = 4))
      })

      # Plot dataset histogram
      output$dataset_histogram <- renderPlot({
        # Check for non-null dataset
        req(dataset())
        # Compute variables histogram
        par(mfrow = c(round(ncol(dataset()) / 8), 8))
        # Check if dataset is continuous.
        if (attributes(dataset())$is_continuous) {
          for (c in names(dataset())) {
            x <- dataset()[, c]
            hist(x, prob = TRUE, xlab = c, ylab = "", main = "", col = "ivory")
            lines(density(x), lwd = 2, col = "tomato")
            curve(
              dnorm(x, mean = mean(x), sd = sd(x)),
              from = min(x),
              to = max(x),
              add = TRUE,
              lwd = 2,
              col = "steelblue"
            )
          }
        } else {
          for (c in names(dataset())) {
            x <- dataset()[, c]
            barplot(prop.table(table(x)), ylim = c(0., 1.), main = c)
          }
        }
      })

      # Plot dataset correlation
      output$dataset_correlation <- renderPlot({
        # Check for non-null dataset
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
        # Check for non-null dataset
        req(dataset())
        # Compute variables heatmap
        rho <- if (attributes(dataset())$is_continuous) {
          cor(dataset())
        } else {
          cor(
            model.matrix(~ 0 + ., data = dataset()),
            use = "pairwise.complete.obs"
          )
        }
        palette_breaks <- seq(0, 1, 0.1)
        par(oma = c(2, 2, 2, 1))
        heatmap.2(rho, scale = "none", trace = "none", revC = TRUE, breaks = palette_breaks)
      })

      # Return the dataset
      return(dataset)
    }
  )
}
