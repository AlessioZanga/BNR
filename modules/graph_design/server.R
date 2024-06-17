# Load required packages
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyFeedback)
library(igraph)
library(bnlearn)
library(parallel)
library(visNetwork)

# Define graph_design server function
graphDesignServer <- function(id = "graph_design", dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Initialize graph to the null graph
      graph <- reactiveVal(make_empty_graph())
      # Initialize the forbidden and required dataframes.
      forbidden_edges <- reactiveVal(
        data.frame(from = character(), to = character())
      )
      required_edges <- reactiveVal(
        data.frame(from = character(), to = character())
      )

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
          req(input$definition != "file" && len > 0)
          showFeedbackSuccess("definition")
          # Build new null graph with labels
          g <- make_empty_graph(n = len)
          V(g)$label <- nodes
          # Update reactive val
          graph(g)

          # If the dataset has time lag, then forbidden edges are added.
          if (attributes(dataset())$has_time_lag) {
            # Get colnames.
            cols <- colnames(dataset())
            # Get 0-lag and 1-lag variables.
            lag_0 <- cols[sapply(cols, function(x) endsWith(x, "_0"))]
            lag_1 <- cols[sapply(cols, function(x) endsWith(x, "_1"))]

            forbidden_edges_lag <- rbind(
              expand.grid(lag_0, lag_0), # Forbid istantaneous feedback.
              expand.grid(lag_1, lag_0) # Forbid feedback from the future.
            )
            colnames(forbidden_edges_lag) <- c("from", "to")

            forbidden_edges(
              rbind(
                forbidden_edges(),
                forbidden_edges_lag
              )
            )
          }
        }
      )

      # Render the forbidden and required edges.
      output$forbidden_edges <- DT::renderDT(forbidden_edges())
      output$required_edges <- DT::renderDT(required_edges())
      # Add forbidden edge
      observeEvent(input$forbidden_edges_add, {
        # Get the vertices
        from <- input$forbidden_edges_from
        to <- input$forbidden_edges_to
        # Check that the vertices are in the graph
        hideFeedback("forbidden_edges_from")
        if (from %in% V(graph())$label) {
          showFeedbackSuccess("forbidden_edges_from")
        } else {
          showFeedbackDanger("forbidden_edges_from")
        }
        hideFeedback("forbidden_edges_to")
        if (to %in% V(graph())$label) {
          showFeedbackSuccess("forbidden_edges_to")
        } else {
          showFeedbackDanger("forbidden_edges_to")
        }
        # Add the edge
        forbidden_edges(
          unique(
            rbind(
              forbidden_edges(),
              data.frame(from, to)
            )
          )
        )
      })
      # Remove forbidden edge
      observeEvent(input$forbidden_edges_remove, {
        # Get the vertices
        from <- input$forbidden_edges_from
        to <- input$forbidden_edges_to
        # Check that the vertices are in the graph
        hideFeedback("forbidden_edges_from")
        if (from %in% V(graph())$label) {
          showFeedbackSuccess("forbidden_edges_from")
        } else {
          showFeedbackDanger("forbidden_edges_from")
        }
        hideFeedback("forbidden_edges_to")
        if (to %in% V(graph())$label) {
          showFeedbackSuccess("forbidden_edges_to")
        } else {
          showFeedbackDanger("forbidden_edges_to")
        }
        # Remove the edge
        forbidden_edges(
          forbidden_edges() %>%
            filter(from != from & to != to)
        )
      })
      # Clear forbidden edges
      observeEvent(input$forbidden_edges_clear, {
        forbidden_edges(data.frame(from = character(), to = character()))
      })

      # Add required edge
      observeEvent(input$required_edges_add, {
        # Get the vertices
        from <- input$required_edges_from
        to <- input$required_edges_to
        # Check that the vertices are in the graph
        hideFeedback("required_edges_from")
        if (from %in% V(graph())$label) {
          showFeedbackSuccess("required_edges_from")
        } else {
          showFeedbackDanger("required_edges_from")
        }
        hideFeedback("required_edges_to")
        if (to %in% V(graph())$label) {
          showFeedbackSuccess("required_edges_to")
        } else {
          showFeedbackDanger("required_edges_to")
        }
        # Add the edge
        required_edges(
          unique(
            rbind(
              required_edges(),
              data.frame(from, to)
            )
          )
        )
      })
      # Remove required edge
      observeEvent(input$required_edges_remove, {
        # Get the vertices
        from <- input$required_edges_from
        to <- input$required_edges_to
        # Check that the vertices are in the graph
        hideFeedback("required_edges_from")
        if (from %in% V(graph())$label) {
          showFeedbackSuccess("required_edges_from")
        } else {
          showFeedbackDanger("required_edges_from")
        }
        hideFeedback("required_edges_to")
        if (to %in% V(graph())$label) {
          showFeedbackSuccess("required_edges_to")
        } else {
          showFeedbackDanger("required_edges_to")
        }
        # Remove the edge
        required_edges(
          required_edges() %>%
            filter(from != from & to != to)
        )
      })
      # Clear required edges
      observeEvent(input$required_edges_clear, {
        required_edges(data.frame(from = character(), to = character()))
      })

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
          algorithm <- input$structure_learning_algorithm
          # Get score function
          s <- input$structure_learning_score
          if (attributes(dataset())$is_continuous) {
            s <- paste0(s, "-g")
          }
          # Add required and forbidden edges if not empty
          required <- NULL
          if (nrow(required_edges()) > 0) {
            required <- required_edges()
          }
          forbidden <- NULL
          if (nrow(forbidden_edges()) > 0) {
            forbidden <- forbidden_edges()
          }
          # Check if strength is to be estimated
          if (input$structure_learning_strength) {
            # Initialize cluster
            cluster <- detectCores()
            cluster <- makeCluster(cluster)
            # Estimate strength of edges
            strength <- boot.strength(
              data = dataset(),
              cluster = cluster,
              R = 100,
              algorithm = algorithm,
              algorithm.args = list(
                score = s,
                whitelist = required,
                blacklist = forbidden
              ),
            )
            # Close cluster
            stopCluster(cluster)
            # Get DAG from strength
            g <- cextend(averaged.network(strength, 0.50))
          } else {
            # Run structure learning algorithm
            g <- getFromNamespace(algorithm, "bnlearn")(
              dataset(),
              score = s,
              whitelist = required,
              blacklist = forbidden
            )
          }
          # Set final progress
          incProgress(0.90)

          # Convert to igraph
          label <- bnlearn::nodes(g)
          g_ <- as.igraph(g)
          V(g_)$label <- label

          graph(g_)

          # Plot edges strength if available
          if (input$structure_learning_strength) {
            output$strength <- renderPlot({
              strength.plot(g, strength)
            })
          }
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
