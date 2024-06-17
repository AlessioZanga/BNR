# Load required packages
library(DT)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyhelper)

# Define dataset_import UI function
datasetImportUI <- function(id = "dataset_import") {
  # Load module namespace
  ns <- NS(id)

  # Accepted dtypes
  dtypes <- list(
    "Continuous" = "continuous",
    "Discrete" = "discrete"
  )

  # Accepted extensions
  formats <- list(
    "Comma Serparated Value (.csv)" = ".csv",
    "Excel Spreadsheet 2003 (.xls)" = ".xls",
    "Excel Spreadsheet 2007 (.xlsx)" = ".xlsx",
    "OpenDocument Spreadsheet (.ods)" = ".ods"
  )

  # Module UI
  tagList(
    fluidRow(
      box(
        title = "Dataset Import",
        status = "primary",
        solidHeader = TRUE,
        width = 3,
        radioButtons(ns("dtype"), "Choose a data type:", dtypes) %>%
          helper(
            type = "markdown",
            content = "dataset_import-dtype"
          ),
        selectInput(ns("format"), "Choose a file format:", formats) %>%
          helper(
            type = "markdown",
            content = "dataset_import-format"
          ),
        checkboxInput(ns("header"), "First row contains header.", TRUE) %>%
          helper(
            type = "markdown",
            content = "dataset_import-header"
          ),
        checkboxInput(ns("index"), "First column contains index.", FALSE) %>%
          helper(
            type = "markdown",
            content = "dataset_import-index"
          ),
        fileInput(
          ns("file"),
          "Choose a file to upload:",
          multiple = FALSE,
          accept = unlist(formats, use.names = FALSE),
        ) %>%
          helper(
            type = "markdown",
            content = "dataset_import-file"
          )
      ),
      box(
        title = "Dataset Preview",
        status = "primary",
        solidHeader = TRUE,
        width = 9,
        DT::DTOutput(ns("dataset_preview"))
      )
    ),
    fluidRow(
      box(
        title = "Dataset Summary",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        DT::DTOutput(ns("dataset_summary"))
      )
    ),
    fluidRow(
      box(
        title = "Dataset Histogram Plot",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        plotOutput(ns("dataset_histogram"))
      )
    ),
    fluidRow(
      column(
        6,
        box(
          title = "Dataset Correlation Plot",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotOutput(ns("dataset_correlation"), height = "750px")
        )
      ),
      column(
        6,
        box(
          title = "Dataset Correlation Heatmap",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          plotOutput(ns("dataset_heatmap"), height = "750px")
        )
      )
    ),
  )
}
