# Load required packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyhelper)

# Define validation UI function
validationUI <- function(id = "validation") {
  # Load module namespace
  ns <- NS(id)

  # Module UI
  tagList(
    fluidRow(
      column(
        width = 5,
        box(
          title = "Model Selection",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          selectInput(
            ns("selected_model"),
            "Select a model:",
            choices = c()
          ),
          actionButton(
            ns("selected_model_remove"),
            "Remove",
            icon = icon("trash")
          ),
          downloadButton(ns("selected_model_download")),
          downloadButton(ns("models_download"), "Download All")
        ),
        box(
          title = "Model Inspection",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          verbatimTextOutput(ns("models_verbatim"))
        )
      ),
      column(
        width = 7,
        box(
          title = "Model Comparison",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotOutput(ns("models_compare"))
        )
      )
    )
  )
}
