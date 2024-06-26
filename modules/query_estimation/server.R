# Load required packages
library(shiny)
library(shinydashboard)
library(shinyFeedback)
library(parallel)
library(bnlearn)

# Define query estimation server function
queryEstimationServer <- function(id = "query_estimation", models) {
  moduleServer(
    id,
    function(input, output, session) {
      # Reactive value for simulated data.
      simulated <- reactiveVal(NULL)

      # Update models keys.
      observeEvent(models(), {
        updateSelectInput(
          session,
          "selected_model",
          choices = names(models()),
          selected = tail(names(models()), n = 1)
        )
      })

      # Execute query
      observeEvent(input$query_execute, {
        # Require a model to be selected.
        req(input$selected_model)

        # Retrieve the selected model.
        selected_model <- models()[[input$selected_model]]

        # Validate query_event
        valid_query_event <- FALSE
        hideFeedback("query_event")
        if (isTruthy(input$query_event)) {
          tryCatch(
            {
              str2expression(input$query_event)
              valid_query_event <- TRUE
              showFeedbackSuccess("query_event")
            },
            error = function(e) {
              showFeedbackDanger("query_event")
              showModal(
                modalDialog(
                  HTML(paste0("
                    The evidence `", input$query_event, "` is invalid, a valid evidence is any valid combination of the following:
                    <br>
                    <ul>
                        <li><b>Numeric constants</b>, i.e. integers, real numbers, etc., </li>
                        <li><b>Known vars</b>, i.e. `", toString(names(selected_model)), "`,</li>
                        <li><b>Known ops</b>, i.e. `", unlist(lapply(c("Arith", "Compare", "Logic", "Math"), getGroupMembers)), "`.</li>
                    </ul>
                  ")),
                  title = "Invalid evidence!",
                  footer = modalButton("Dismiss")
                )
              )
            }
          )
        } else {
          showFeedbackDanger("query_event")
        }

        # Validate query_variables
        vars <- NULL
        valid_variables <- FALSE
        hideFeedback("query_variables")
        if (input$query_type == "cpd") {
          if (isTruthy(input$query_variables)) {
            vars <- strsplit(input$query_variables, ",")
            vars <- lapply(vars, trimws)
            vars <- unlist(vars)
            if (all((vars %in% names(selected_model)))) {
              valid_variables <- TRUE
              showFeedbackSuccess("query_variables")
            } else {
              showFeedbackDanger("query_variables", text = "Invalid input.")
              showModal(
                modalDialog(
                  HTML(paste("
                    The target variables `", input$query_variables, "` are invalid,
                    valid target variables is a sequence of variables separated by a comma ',' .
                  ")),
                  title = "Invalid target variables!",
                  footer = modalButton("Dismiss")
                )
              )
            }
          } else {
            showFeedbackDanger("query_variables", text = "Required input.")
          }
        }

        # Validate query_evidence
        valid_query_evidence <- FALSE
        hideFeedback("query_evidence")
        if (isTruthy(input$query_evidence)) {
          tryCatch(
            {
              str2expression(gsub("[\r\n]", " ", input$query_evidence))
              valid_query_evidence <- TRUE
              showFeedbackSuccess("query_evidence")
            },
            error = function(e) {
              showFeedbackDanger("query_evidence")
              showModal(
                modalDialog(
                  HTML(paste0("
                    The evidence `", input$query_evidence, "` is invalid, a valid evidence is any valid combination of the following:
                    <br>
                    <ul>
                        <li><b>Numeric constants</b>, i.e. integers, real numbers, etc., </li>
                        <li><b>Known vars</b>, i.e. `", toString(names(selected_model)), "`,</li>
                        <li><b>Known ops</b>, i.e. `", unlist(lapply(c("Arith", "Compare", "Logic", "Math"), getGroupMembers)), "`.</li>
                    </ul>
                  ")),
                  title = "Invalid evidence!",
                  footer = modalButton("Dismiss")
                )
              )
            }
          )
        } else {
          showFeedbackDanger("query_evidence")
        }

        # Validate query_samples
        valid_query_samples <- FALSE
        hideFeedback("query_samples")
        valid_query_samples <- (input$query_samples > 0)
        if (valid_query_samples) {
          showFeedbackSuccess("query_samples")
        } else {
          showFeedbackDanger("query_samples")
        }

        # Validate query_batch
        valid_query_batch <- FALSE
        hideFeedback("query_batch")
        valid_query_batch <- (input$query_batch > 0)
        if (valid_query_batch) {
          showFeedbackSuccess("query_batch")
        } else {
          showFeedbackDanger("query_batch")
        }

        req(
          selected_model,
          valid_query_event,
          (input$query_type == "cp" || valid_variables),
          valid_query_evidence,
          valid_query_samples,
          valid_query_batch
        )

        if (input$query_type == "cp") {
          # Initialize progress bar
          withProgress(message = "Sampling from conditional probability", {
            # Update progress bar
            incProgress(0.10)
            # Setup cluster
            cluster <- NULL
            if (input$query_cluster) {
              cluster <- detectCores()
              cluster <- makeCluster(cluster)
            }
            # Update progress bar
            incProgress(0.10)
            # Compute conditional probability distribution
            simulated(replicate(30, eval(parse(text = paste0("
              cpquery(
                fitted = selected_model,
                event = (", input$query_event, "),
                evidence = (", input$query_evidence, "),
                method = input$query_method,
                n = input$query_samples,
                batch = input$query_batch,
                cluster = cluster,
                debug = TRUE
              )")))))
            # Close cluster
            if (!is.null(cluster)) {
              stopCluster(cluster)
            }
            # Update progress bar
            incProgress(0.70)
            # Plot conditional probability distribution
            output$query_plot <- renderPlot(boxplot(simulated()))
            # Complete progress bar
            incProgress(0.30)
          })
        }

        if (input$query_type == "cpd") {
          # Initialize progress bar
          withProgress(message = "Sampling from conditional distribution", {
            # Update progress bar
            incProgress(0.10, detail = "Initializing sampling procedure.")
            # Setup cluster
            cluster <- NULL
            if (input$query_cluster) {
              cluster <- detectCores()
              cluster <- makeCluster(cluster)
            }
            # Update progress bar
            incProgress(0.10, detail = "Performing sampling procedure.")
            # Compute conditional probability distribution
            simulated(eval(parse(text = paste0("cpdist(
              fitted = selected_model,
              nodes = vars,
              evidence = (", input$query_event, ") & (", input$query_evidence, "),
              method = input$query_method,
              n = input$query_samples,
              batch = input$query_batch,
              cluster = cluster,
              debug = TRUE
            )"))))
            # Close cluster
            if (!is.null(cluster)) {
              stopCluster(cluster)
            }
            # Update progress bar
            incProgress(0.70, detail = "Plotting conditional distribution.")
            # Plot sampled conditional distribution
            output$query_plot <- renderPlot(plot(simulated()))
            # Complete progress bar
            incProgress(0.30)
          })
        }
      })

      output$download_simulated <- downloadHandler(
        filename = function() {
          paste0(
            "simulated-",
            format(Sys.time(), "%Y%m%d-%H%M%S"),
            ".csv"
          )
        },
        content = function(file) {
          write.csv(simulated(), file)
        }
      )
    }
  )
}
