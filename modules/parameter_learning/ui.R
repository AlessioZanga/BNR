# Load required packages
library(shiny)
library(shinydashboard)
library(shinyhelper)

# Define parameter_learning UI function
parameterLearningUI <- function(id = "parameter_learning") {
  # Load module namespace
  ns <- NS(id)

  # Module UI
  tagList(
    fluidRow(
      column(
        width = 6,
        box(
          title = "Model Preview",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotOutput(ns("preview")) %>%
            helper(
              type = "markdown",
              content = "parameter_learning-preview"
            )
        ),
        box(
          title = "Model Description",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          verbatimTextOutput(ns("description")) %>%
            helper(
              type = "markdown",
              content = "parameter_learning-description"
            ),
          br()
        )
      ),
      column(
        width = 6,
        box(
          title = "Fitting Constraints",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          verbatimTextOutput(ns("constraints_list")),
          textInput(ns("constraints_input"), "Write a constraint:") %>%
            helper(
              type = "markdown",
              content = "parameter_learning-constraints_input"
            ),
          actionButton(
            ns("constraints_add"),
            "Add",
            icon = icon("plus")
          ),
          actionButton(
            ns("constraints_clear"),
            "Clear",
            icon = icon("trash")
          )
        ),
        fluidRow(
          column(
            width = 6,
            box(
              title = "Fitting Parameter",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              checkboxInput(
                ns("constraints_flag"),
                "Solve with constraits.",
                value = TRUE
              ) %>%
                helper(
                  type = "markdown",
                  content = "parameter_learning-constraints_flag"
                ),
              actionButton(
                ns("fit_parameters"),
                "Fit Model",
                icon = icon("cogs")
              )
            ),
          ),
          column(
            width = 6,
            box(
              title = "Export Model",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              downloadButton(ns("download_model"))
            )
          )
        ),
        box(
          title = "Quantiles Plot",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotOutput(ns("quantiles_plot"))
        ),
        box(
          title = "Residuals Plot",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotOutput(ns("residuals_plot"))
        ),
        box(
          title = "Histogram Plot",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotOutput(ns("histogram_plot"))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = "Regression Coefficients",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotOutput(
            ns("regression_plot"),
            width = "auto",
            height = "750px"
          )
        )
      )
    )
  )
}
