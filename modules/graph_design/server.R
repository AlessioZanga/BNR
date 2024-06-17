# Load required packages
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyFeedback)
library(igraph)
library(bnlearn)
library(visNetwork)

# Define graph_design server function
graphDesignServer <- function(id = "graph_design", dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Initialize graph to the null graph
      graph <- reactiveVal(make_empty_graph())

      # Build empty graph from variables in dataset
      observeEvent(
        {
          input$definition
          dataset()
        },
        {
          # Get nodes
          nodes <- names(dataset())
          len <- length(nodes)
          # Check for non-empty dataset
          hideFeedback("definition")
          req(input$definition == "interactive" && len > 0)
          showFeedbackSuccess("definition")
          # Build new null graph with labels
          g <- make_empty_graph(n = len)
          V(g)$label <- nodes
          # Update reactive val
          graph(g)
        }
      )

      # Learn structure from dataset
      observeEvent(input$structure_learning_run, {
        # Unset validation
        hideFeedback("definition")
        hideFeedback("structure_learning_algorithm")
        hideFeedback("structure_learning_score")
        # Check for structure definition
        req(input$definition == "structure_learning")
        # Check for non-empty dataset
        req(dataset())
        # Set validation
        showFeedbackSuccess("definition")
        showFeedbackSuccess("structure_learning_algorithm")
        showFeedbackSuccess("structure_learning_score")
        # Initialize progress bar
        withProgress(message = "Learning model", {
          # Set initial progress
          incProgress(0.10)
          # Select structure learning algorithm
          algorithm <- getFromNamespace(
            input$structure_learning_algorithm,
            "bnlearn"
          )
          # Get score function
          s <- input$structure_learning_score
          if (attributes(dataset())$is_continuous) {
            s <- paste0(s, "-g")
          }
          # Run structure learning algorithm
          g <- algorithm(dataset(), score = s)
          # Set final progress
          incProgress(0.90)

          # Convert to igraph
          label <- bnlearn::nodes(g)
          g <- as.igraph(g)
          V(g)$label <- label
          
          graph(g)
        })
      })

      # Load graph from file
      observeEvent(
        {
          input$definition
          input$file
        },
        {
          req(input$definition == "file")
          # Check if file uploaded correctly
          req(input$file)
          file <- input$file$datapath
          # Try to parse uploaded file
          tryCatch(
            {
              hideFeedback("definition")
              hideFeedback("upload_format")
              hideFeedback("file")
              g <- switch(input$upload_format,
                ".txt" = {
                  read_graph(file, format = "edgelist")
                },
                ".graphml" = {
                  read_graph(file, format = "graphml")
                },
                ".gml" = {
                  read_graph(file, format = "gml")
                }
              )
              showFeedbackSuccess("definition")
              showFeedbackSuccess("upload_format")
              showFeedbackSuccess("file")
              graph(g)
            },
            error = function(e) {
              showFeedbackDanger("definition")
              showFeedbackDanger("upload_format")
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
        }
      )

      # Map igraph to visNetwork representation
      output$preview <- renderVisNetwork({
        # Get graph
        g <- graph()
        # Plot only on non-null graphs
        len <- gorder(g)
        req(len > 0)
        # Add node style
        V(g)$shape <- rep(input$graph_node_shape, len)
        # FIXME: V(g)$color <- rep(input$graph_node_color, len)
        V(g)$shadow <- rep(input$graph_node_shadow, len)
        # Add edge style
        len <- ecount(g)
        E(g)$shadow <- rep(input$graph_edge_shadow, len)
        # Get graph data
        data <- toVisNetworkData(g, idToLabel = FALSE)
        # If edges are empty, set missing columns
        if (length(names(data$edges)) == 0) {
          data$edges <- data.frame(
            from = integer(),
            to = integer()
          )
        }
        # Build visNetwork
        visNetwork(
          nodes = data$nodes,
          edges = data$edges,
          height = "100%",
          width = "100%",
          main = ""
        ) %>%
          # Add arrows direction
          visEdges(arrows = "to") %>%
          # Add igraph layout
          visIgraphLayout(layout = input$graph_layout) %>%
          # Add multi-selection
          visInteraction(
            multiselect = TRUE,
            selectConnectedEdges = FALSE
          ) %>%
          # Attach event listener for node and edge selection
          visEvents(
            selectNode = sprintf(
              "function(graph) {
                            Shiny.onInputChange('%s', graph.nodes);
                        ;}",
              ns("selected_nodes")
            ),
            selectEdge = sprintf(
              "function(graph) {
                            Shiny.onInputChange('%s', graph.edges);
                        ;}",
              ns("selected_edges")
            )
          )
      })

      # Add edge on click- and long-click- events
      observeEvent(input$selected_nodes, {
        nodes <- input$selected_nodes
        req(length(nodes) > 1)
        # If adding this edge results in a cycle, return error
        if (length(all_simple_paths(graph(), nodes[2], nodes[1])) > 0) {
          showModal(
            modalDialog(
              "Graph cannot contain cycles! Adding this edge will create a cycle.",
              title = "Oops! Something went wrong!",
              footer = modalButton("Dismiss")
            )
          )

          return()
        }
        # Add edge to graph, ignoring multiedges
        graph(add_edges(graph(), nodes) %>% simplify())
      })

      # Map edge id to pair of node ids
      observeEvent(input$selected_edges, {
        req(length(input$selected_edges) == 1)
        visNetworkProxy(ns("preview")) %>% #
          visGetConnectedNodes(
            id = input$selected_edges,
            input = ns("selected_pairs")
          )
      })

      # Delete pair of node ids from graph on 'delete' key press
      observeEvent(input$keyup, {
        # If 'delete' key is pressed and nodes pair is set
        req(as.integer(input$keyup[1]) == 46)
        req(input$selected_pairs)
        # Get edge id
        edge <- get.edge.ids(graph(), input$selected_pairs)
        # If no edge id is returned (e.g. a vertex is select), return error
        if (edge == 0) {
          showModal(
            modalDialog(
              "Vertices cannot be deleted! Deleting a node will change the problem structure.",
              title = "Oops! Something went wrong!",
              footer = modalButton("Dismiss")
            )
          )

          return()
        }
        # Delete edge from graph
        graph(delete_edges(graph(), edge))
      })

      # Download graph
      output$download_graph <- downloadHandler(
        filename = function() {
          paste0(
            "graph-",
            format(Sys.time(), "%Y%m%d-%H%M%S"),
            input$download_format
          )
        },
        content = function(file) {
          ext <- substring(input$download_format, 2)
          if (ext == "txt") {
            ext <- "edgelist"
          }
          write_graph(graph(), file, format = ext)
        }
      )

      output$download_plot <- renderUI({
        format <- input$download_plot_format
        actionButton(
          ns("download_plot_button"),
          label = "Download",
          icon = icon("download"),
          onclick = sprintf("
            var date = new Date;
            var date = '' + date.getFullYear()
                + (date.getMonth()+1)
                + ('' + date.getDate()).padStart(2, '0')
                + '-'
                + ('' + date.getHours()).padStart(2, '0')
                + ('' + date.getMinutes()).padStart(2, '0')
                + ('' + date.getSeconds()).padStart(2, '0')
            var a = document.createElement('a'); // Add link
            a.href = $('canvas')[0].toDataURL('image/%s'); // Base64 image
            a.download = 'plot-' + date + '.%s'; // Download file name
            a.click(); // Trigger download
          ", format, format)
        )
      })

      # Return igraph
      return(graph)
    }
  )
}
