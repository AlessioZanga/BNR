# Load global configuration
source("config.R")

# Load required packages
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyFeedback)
library(shinyhelper)
library(markdown)
# Add development packages
library(languageserver)

# Define dashboard header
header <- dashboardHeader(title = "BNR")

# Define dashboard sidebar
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Home",
    tabName = "home",
    icon = icon("home")
  ),
  menuItem(
    "Dataset Import",
    tabName = "dataset_import",
    icon = icon("database")
  ),
  menuItem(
    "Graph Design",
    tabName = "graph_builder",
    icon = icon("project-diagram")
  ),
  menuItem(
    "Parameter Learning",
    tabName = "parameter_learning",
    icon = icon("calculator")
  ),
  menuItem(
    "Query Estimation",
    tabName = "query_estimation",
    icon = icon("cogs")
  ),
  menuItem(
    "Validation",
    tabName = "validation",
    icon = icon("check")
  )
))

# Define the home page
home <- tagList(
  fluidRow(
    box(
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      includeMarkdown("README.md")
    )
  )
)

# Define dashboard body
body <- dashboardBody(
  useShinyjs(),
  useShinyFeedback(),
  tabItems(
    tabItem(tabName = "home", home),
    tabItem(tabName = "dataset_import", datasetImportUI()),
    tabItem(tabName = "graph_builder", graphDesignUI()),
    tabItem(tabName = "parameter_learning", parameterLearningUI()),
    tabItem(tabName = "query_estimation", queryEstimationUI()),
    tabItem(tabName = "validation", validationUI())
  )
)

# Define UI for the application
ui <- dashboardPage(header, sidebar, body)

# Define on start setup
onStart <- function() {

}

# Define server logic required
server <- function(input, output, session) {
  # Define observer for helpers.
  observe_helpers(session, help_dir = "help")

  # Define the list of models
  models <- reactiveVal(list())

  # Load modules
  dataset <- datasetImportServer()
  graph <- graphDesignServer(dataset = dataset)
  models <- parameterLearningServer(dataset = dataset, graph = graph, models = models)
  queryEstimationServer(models = models)
  validationServer(models = models)
}

# Get default port
port <- strtoi(Sys.getenv("R_SHINY_PORT"))
if (is.na(port)) {
  port <- 8080
}

# Run the application
shinyApp(
  ui = ui,
  server = server,
  onStart = onStart,
  options = list(
    "host" = "0.0.0.0",
    "port" = port
  )
)
