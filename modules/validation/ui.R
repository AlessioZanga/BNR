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
          )
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
