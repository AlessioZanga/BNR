# Load required packages
library(shiny)
library(shinydashboard)
library(shinyFeedback)
library(bnlearn)

# Define validation server function
validationServer <- function(id = "validation", models) {
  moduleServer(
    id,
    function(input, output, session) {
      # Update models keys.
      observeEvent(models(), {
        updateSelectInput(
          session,
          "selected_model",
          choices = names(models()),
          selected = tail(names(models()), n = 1)
        )
      })

      # Remove selected model.
      observeEvent(input$selected_model_remove, {
        # Require a model to be selected.
        req(input$selected_model)
        # Get models.
        m <- models()
        # Remove the selected model.
        m[[input$selected_model]] <- NULL
        # Update models.
        models(m)
      })

      # Download selected model.
      output$selected_model_download <- downloadHandler(
        filename = paste0("model-", input$selected_model, ".rds"),
        content = function(file) {
          # Require a model to be selected.
          req(input$selected_model)

          saveRDS(
            models()[[input$selected_model]],
            file
          )
        }
      )

      # Download all models.
      output$models_download <- downloadHandler(
        filename = paste0(
          "models-",
          format(Sys.time(), "%Y%m%d-%H%M%S"),
          ".rds"
        ),
        content = function(file) saveRDS(models(), file)
      )

      # Print selected model.
      output$models_verbatim <- renderPrint({
        # Require a model to be selected.
        req(input$selected_model)

        # Print the model.
        models()[[input$selected_model]]
      })

      # Model comparison using graphviz.compare and do.call
      output$models_compare <- renderPlot({
        # Require a model to be selected.
        req(input$selected_model)
        # Set plot layout.
        par(mfrow = c(ceiling(length(models()) / 2), 2))
        # Copy the models list.
        tail <- models()
        # Get the selected model.
        reference <- tail[[input$selected_model]]
        # Remove the selected model.
        tail[[input$selected_model]] <- NULL
        # Add the selected model as the first element.
        head <- list()
        head[[input$selected_model]] <- reference
        # Concatenate the selected model with the rest.
        comparison <- c(head, tail)
        # Compare the selected model with the rest.
        do.call(
          graphviz.compare,
          c(
            # Get models to compare.
            unname(comparison),
            # Set the plot arguments.
            list(
              shape = "rectangle",
              layout = "dot",
              main = names(comparison),
              diff = "from-first",
              diff.args = list(
                tp.lwd = 2,
                tp.col = "green",
                fn.col = "darkorange2"
              )
            )
          )
        )
      })
    }
  )
}
