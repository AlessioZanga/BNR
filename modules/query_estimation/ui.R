# Load required packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyhelper)

# Define query_estimation UI function
queryEstimationUI <- function(id = "query_estimation") {
  # Load module namespace
  ns <- NS(id)

  # Module UI
  tagList(
    fluidRow(
      column(
        width = 6,
        box(
          title = "Query Plot",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotOutput(ns("query_plot"))
        ),
        box(
          title = "Export Simulated Data",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          downloadButton(ns("download_simulated"))
        )
      ),
      column(
        width = 6,
        box(
          title = "Execute Query",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          selectInput(
            ns("selected_model"),
            "Select a model:",
            choices = c()
          ),
          selectInput(ns("query_type"), "Query type:", list(
            "Conditional Probability of an Event given Evidence" = "cp",
            "Conditional Probability Distribution given Evidence" = "cpd"
          )) %>%
            helper(
              type = "markdown",
              content = "query_estimation-query_type"
            ),
          conditionalPanel(
            condition = "input.query_type == 'cpd'", ns = ns,
            textInput(ns("query_variables"), "Target variables:") %>%
              helper(
                type = "markdown",
                content = "query_estimation-query_variables"
              )
          ),
          textInput(ns("query_event"), "Target event:") %>%
            helper(
              type = "markdown",
              content = "query_estimation-query_event"
            ),
          textAreaInput(ns("query_evidence"), "Given evidence:", rows = 3) %>%
            helper(
              type = "markdown",
              content = "query_estimation-query_evidence"
            ),
          selectInput(ns("query_method"), "Estimation method:", list(
            "Logic Sampling" = "ls"
          )) %>%
            helper(
              type = "markdown",
              content = "query_estimation-query_method"
            ),
          autonumericInput(
            ns("query_samples"),
            "Number of samples:",
            value = 10^5,
            align = "left",
            digitGroupSeparator = ",",
            allowDecimalPadding = FALSE,
            decimalPlaces = 0,
            minimumValue = 1
          ) %>%
            helper(
              type = "markdown",
              content = "query_estimation-query_samples"
            ),
          autonumericInput(
            ns("query_batch"),
            "Batch size:",
            value = 10^3,
            align = "left",
            digitGroupSeparator = ",",
            allowDecimalPadding = FALSE,
            decimalPlaces = 0,
            minimumValue = 1
          ) %>%
            helper(
              type = "markdown",
              content = "query_estimation-query_batch"
            ),
          checkboxInput(
            ns("query_cluster"),
            "Enable multi-threading.",
            TRUE
          ) %>%
            helper(
              type = "markdown",
              content = "query_estimation-query_cluster"
            ),
          actionButton(
            ns("query_execute"),
            "Execute Query",
            icon = icon("cogs")
          )
        )
      )
    )
  )
}
