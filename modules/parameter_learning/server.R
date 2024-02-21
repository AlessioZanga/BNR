# Load required packages
library(shiny)
library(shinydashboard)
library(shinyFeedback)
library(shinyjs)
library(igraph)
library(Rgraphviz)
library(gRain)
library(bnlearn)
library(CVXR)

# Define parameter_learning server function
parameterLearningServer <-
  function(id = "parameter_learning", dataset, graph) {
    moduleServer(
      id,
      function(input, output, session) {
        # Initialize model to the null model
        model <- reactiveVal(NULL)

        # Initialize constraints
        constraints <- reactiveVal(list())

        # Map igraph object to bnlearn model
        observeEvent(graph(), {
          g <- graph()
          # Check for non-null graph
          req(gorder(g) > 0)
          # Workaround for non-aligned igraph and bnlearn attributes
          g <- set_vertex_attr(g, "name", value = V(g)$label)
          # Check if graph is acyclic
          req(is_dag(g))
          # Convert igraph to bnlearn
          model(as.bn(g))
        })

        # Plot model
        output$preview <- renderPlot({
          req(model())
          graphviz.plot(model(), layout = "dot", shape = "rectangle")
        })

        # Print model description
        output$description <- renderPrint({
          req(model())
          model()
        })

        # Fit parameters
        observeEvent(input$fit_parameters, {
          # Get data from dataset
          data <- dataset()
          # Get DAG from empty model
          dag <- model()
          # Check if dag is bn.net
          if (class(dag)[[1]] != "bn") {
            dag <- bn.net(dag)
          }
          # Initialize parameters given nodes
          parameters <- vector(nnodes(dag), mode = "list")
          names(parameters) <- nodes(dag)
          # Initialize progress bar
          withProgress(message = "Fitting model", {
            # For each node in DAG
            for (node in nodes(dag)) {
              # Update progress bar
              incProgress(
                1 / nnodes(dag),
                detail = paste0("Processing '", node, "'")
              )
              # Get response given node
              response <- data[, node]
              # If node has no parents
              if (node %in% root.nodes(dag)) {
                # Then set parameters to normal distribution
                parameters[[node]] <-
                  list(
                    coef = mean(response),
                    sd = sd(response),
                    fitted = rep(mean(response), nrow(data)),
                    resid = response - mean(response)
                  )
                # Continue to next node
                next
              }
              # Else if node has at least one parent, set the parents...
              predictors <- as.matrix(data[, bnlearn::parents(dag, node), drop <- FALSE])
              # ... and add the intercept as first value
              predictors <- cbind(rep(1, nrow(predictors)), predictors)
              # Initialize variable given associated node
              coefs <- Variable(length(bnlearn::parents(dag, node)) + 1)
              # Minimize the objective function...
              objective <- Minimize(
                sum_squares(
                  response - predictors %*% coefs
                )
              )
              # ... subject to given constraints
              cs <- list()
              if (input$constraints_flag) {
                # Set RESPONSE scope for eval resolution
                assign("RESPONSE", predictors %*% coefs, envir = environment())
                # Get the constraints
                cs <- constraints()
                # Filter only local constraints
                cs <- Filter(function(x) grepl(node, x), cs)
                # Replace local variable with REPONSE placeholder
                cs <- lapply(cs, function(x) gsub(node, "RESPONSE", x))
                # Replace non-local variables with data.
                cs <- lapply(cs, function(x) {
                    for (var in nodes(model())) {
                        x <- gsub(var, paste0("data[ , \"", var,"\"]"), x)
                    }
                    
                    x
                })
                # Parse the constraints
                cs <- lapply(cs, str2lang)
                # Evaluate the constraints
                cs <- lapply(cs, function(x) eval(x, envir = environment()))
              }
              # Define the constrained optimization problem...
              problem <- Problem(objective, constraints = cs)
              # ... and solve it
              result <- solve(problem)
              # Get the resulting coefficients
              coefficients <- result$getValue(coefs)
              fitted.values <- as.numeric(predictors %*% coefficients)
              residuals <- as.numeric(response - fitted.values)
              # Update parameters given coefficients
              parameters[[node]] <-
                list(
                  coef = as.numeric(coefficients),
                  sd = sd(residuals) * sqrt(
                    (nrow(data) - 1) / (nrow(data) - ncol(predictors))
                  ),
                  fitted = fitted.values,
                  resid = residuals
                )
            }
            # Build the Bayesian Network from the DAG and parameters
            model(custom.fit(dag, parameters))
          })
        })

        # Plot regression coefficients
        output$regression_plot <- renderPlot({
          req(model(), inherits(model(), "bn.fit"))
          graphviz.chart(
            model(),
            type = "barprob",
            grid = TRUE,
            scale = c(1.50, 3.0)
          )
        })

        # Plot quantiles
        output$quantiles_plot <- renderPlot({
          req(model(), inherits(model(), "bn.fit"))
          bn.fit.qqplot(model())
        })

        # Plot residuals
        output$residuals_plot <- renderPlot({
          req(model(), inherits(model(), "bn.fit"))
          bn.fit.xyplot(model())
        })

        # Plot histogram
        output$histogram_plot <- renderPlot({
          req(model(), inherits(model(), "bn.fit"))
          bn.fit.histogram(model())
        })

        # Check expression syntax

        ## Get valid ops
        ops <- unlist(lapply(c("Arith", "Compare", "Math"), getGroupMembers))
        ## Add brackets for nested ops
        ops <- c(ops, c("(", ")", "[", "]", "{", "}"))

        ## Build Abstract Syntax Tree (AST)
        ast <- function(expr) {
          lapply(as.list(expr), function(x) `if`(is.call(x), ast(x), x))
        }

        ## Validate AST against selected vars and ops
        is_valid_ast <- function(ast, vars, ops) {
          # Base case: a leaf is just a single element, wrap in list.
          if (!is.list(ast)) ast <- list(ast)
          # Base case: numeric constants are valid
          if (is.numeric(ast[[1]])) {
            return(TRUE)
          }
          # Base case: vars are valid
          if (as.character(ast[[1]]) %in% vars) {
            return(TRUE)
          }
          # Recursive case: ops are n-ary valid
          if (as.character(ast[[1]]) %in% ops && length(ast) > 1) {
            return(
              all(sapply(
                ast[-1],
                is_valid_ast,
                vars,
                ops
              ))
            )
          }
          # Otherwise, its invalid
          return(FALSE)
        }

        ## Check expression validity
        is_valid_expr <- function(expr, vars, ops) {
          is_valid_ast(ast(expr), vars, ops)
        }

        # Validate constraints
        observeEvent(input$constraints_add, {
          # Require existing model and get its vars
          req(model())
          vars <- nodes(model())
          # Add dummy RESPONSE placeholder
          vars <- c("RESPONSE", vars)
          # Require existing constraint input
          req(input$constraints_input)
          # Check lang validity
          tryCatch(
            {
              hideFeedback("constraints_input")
              # Catch exception if str2lang fail
              expr <- str2lang(input$constraints_input)
              # Raise exception if expr is not valid
              if (!is_valid_expr(expr, vars, ops)) {
                stop("Invalid expression")
              }
              constraints(c(constraints(), input$constraints_input))
              showFeedbackSuccess("constraints_input")
              reset("constraints_input")
            },
            error = function(e) {
              showFeedbackDanger("constraints_input")
              showModal(
                modalDialog(
                  HTML(paste0("
                    The constraint `", input$constraints_input, "` is invalid, a valid constraint is any valid combination of the following:
                    <br>
                    <ul>
                        <li><b>Numeric constants</b>, i.e. integers, real numbers, etc., </li>
                        <li><b>Known vars</b>, i.e. `", toString(vars), "`,</li>
                        <li><b>Known ops</b>, i.e. `", toString(ops), "`.</li>
                    </ul>
                  ")),
                  title = "Invalid constraint!",
                  footer = modalButton("Dismiss")
                )
              )
            }
          )
        })

        # Clear constraints
        observeEvent(input$constraints_clear, {
          hideFeedback("constraints_input")
          constraints(list())
        })

        output$constraints_list <- renderPrint(constraints())

        output$download_model <- downloadHandler(
          filename = function() {
            paste0(
              "model-",
              format(Sys.time(), "%Y%m%d-%H%M%S"),
              ".rds"
            )
          },
          content = function(file) {
            saveRDS(model(), file = file)
          }
        )

        # Return model
        return(model)
      }
    )
  }
