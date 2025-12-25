#' ODE Semantics for Stock-Flow Diagrams
#'
#' Implements functorial semantics: Open(StockFlow) → Open(Dynam)
#' Translates stock-flow diagrams to ODE systems compatible with deSolve.
#'
#' @name semantics
NULL

# Dependency Analysis ========================================================

#' Build dependency graph for variables
#'
#' Determines which variables depend on which stocks and other variables.
#' This is needed to compute variables in the correct order (topological sort).
#'
#' @param diagram StockFlowDiagram object
#' @return Named list mapping variable names to their dependencies
#' @keywords internal
build_dependency_graph <- function(diagram) {
  deps <- list()

  # For each auxiliary variable, extract dependencies
  for (var in diagram@variables) {
    # Get function body and try to extract variable names
    # This is a heuristic - we look for stock/variable names in the function
    var_deps <- character(0)

    # Get all stock names
    stock_names_vec <- sapply(diagram@stocks, function(s) s@name)

    # Get all variable names (auxiliary + sum)
    var_names_vec <- c(
      sapply(diagram@variables, function(v) v@name),
      sapply(diagram@sumvars, function(v) v@name)
    )

    # Check function body for references to stocks/variables
    func_text <- paste(deparse(var@expression), collapse = " ")

    # Check for stock references
    for (sname in stock_names_vec) {
      # Look for the stock name in the function text
      # Pattern: state["name"] or state$name or state[['name']]
      if (grepl(paste0('state\\[[\'\"]', sname, '[\'\"]\\]'), func_text) ||
          grepl(paste0('state\\$', sname), func_text) ||
          grepl(paste0('state\\[\\[[\'\"]', sname, '[\'\"]\\]\\]'), func_text)) {
        var_deps <- c(var_deps, sname)
      }
    }

    # Check for variable references
    for (vname in var_names_vec) {
      if (vname != var@name) {  # Don't include self
        # Look for var_values["name"] or var_values$name
        if (grepl(paste0('var_values\\[[\'\"]', vname, '[\'\"]\\]'), func_text) ||
            grepl(paste0('var_values\\$', vname), func_text) ||
            grepl(paste0('var_values\\[\\[[\'\"]', vname, '[\'\"]\\]\\]'), func_text)) {
          var_deps <- c(var_deps, vname)
        }
      }
    }

    deps[[var@name]] <- unique(var_deps)
  }

  # Sum variables depend on their constituent stocks
  for (var in diagram@sumvars) {
    stock_deps <- character(0)
    for (stock_id in var@stock_ids) {
      stock <- get_stock(diagram, stock_id)
      if (!is.null(stock)) {
        stock_deps <- c(stock_deps, stock@name)
      }
    }
    deps[[var@name]] <- unique(stock_deps)
  }

  deps
}

#' Topological sort for variable evaluation order
#'
#' Orders variables so that dependencies are evaluated before dependents.
#'
#' @param diagram StockFlowDiagram object
#' @param deps Dependency graph from build_dependency_graph
#' @return Character vector of variable names in evaluation order
#' @keywords internal
topological_sort_variables <- function(diagram, deps) {
  # Get all variable names
  all_vars <- c(
    if (length(diagram@variables) > 0) sapply(diagram@variables, function(v) v@name) else character(0),
    if (length(diagram@sumvars) > 0) sapply(diagram@sumvars, function(v) v@name) else character(0)
  )

  if (length(all_vars) == 0) {
    return(character(0))
  }

  # Kahn's algorithm
  sorted <- character(0)
  in_degree <- sapply(all_vars, function(v) {
    # Count how many OTHER VARIABLES this variable depends on (not stocks)
    dep_list <- deps[[v]]
    if (is.null(dep_list) || length(dep_list) == 0) return(0)
    # Only count dependencies that are variables, not stocks
    sum(dep_list %in% all_vars)
  })

  # Start with variables that have no dependencies on other variables
  queue <- names(in_degree)[in_degree == 0]

  while (length(queue) > 0) {
    # Take first from queue
    current <- queue[1]
    queue <- queue[-1]

    # Add to sorted list
    sorted <- c(sorted, current)

    # For each variable that depends on current
    for (v in all_vars) {
      if (current %in% deps[[v]]) {
        in_degree[v] <- in_degree[v] - 1
        if (in_degree[v] == 0) {
          queue <- c(queue, v)
        }
      }
    }
  }

  # Check for cycles
  if (length(sorted) != length(all_vars)) {
    warning("Circular dependency detected in variables. Some variables may not be computed.")
  }

  sorted
}

# Flow Rate Evaluation =======================================================

#' Evaluate a flow's rate function
#'
#' Collects inputs from links and evaluates the flow's rate function.
#'
#' @param diagram StockFlowDiagram object
#' @param flow Flow object
#' @param state Named numeric vector of current stock values
#' @param params Named vector of parameters
#' @param var_values Named list of computed variable values
#' @param sumvar_values Named list of computed sum variable values
#' @return Numeric flow rate value
#' @keywords internal
evaluate_flow_rate <- function(diagram, flow, state, params, var_values, sumvar_values) {
  # Collect inputs to the flow from links
  link_ids <- get_links_to_flow(diagram, flow@id)

  if (length(link_ids) == 0) {
    # No links - flow rate function should not depend on any inputs
    # Call with empty inputs
    return(flow@rate_function(list(), params))
  }

  # Build inputs list from links
  inputs <- list()

  for (link_id in link_ids) {
    link <- get_link(diagram, link_id)
    if (is.null(link)) next

    if (link@source_type == "stock") {
      stock <- get_stock(diagram, link@source_id)
      if (!is.null(stock)) {
        inputs[[stock@name]] <- state[[stock@name]]
      }
    } else if (link@source_type == "variable") {
      # Find variable by ID
      var <- NULL
      for (v in diagram@variables) {
        if (v@id == link@source_id) {
          var <- v
          break
        }
      }
      if (!is.null(var)) {
        inputs[[var@name]] <- var_values[[var@name]]
      }
    } else if (link@source_type == "sumvar") {
      # Find sum variable by ID
      var <- NULL
      for (v in diagram@sumvars) {
        if (v@id == link@source_id) {
          var <- v
          break
        }
      }
      if (!is.null(var)) {
        inputs[[var@name]] <- sumvar_values[[var@name]]
      }
    }
  }

  # Evaluate flow rate function
  # Note: params should already be a list (converted in generate_ode)
  tryCatch({
    result <- flow@rate_function(inputs, params)
    if (is.null(result) || length(result) == 0) {
      stop("Flow rate function returned NULL or zero-length result")
    }
    if (!is.numeric(result)) {
      stop("Flow rate function must return a numeric value")
    }
    if (length(result) > 1) {
      warning("Flow rate function returned multiple values, using first")
      result <- result[1]
    }
    result
  }, error = function(e) {
    stop("Error evaluating flow '", flow@name, "': ", e$message)
  })
}

# ODE Generation =============================================================

#' Generate ODE function from stock-flow diagram
#'
#' Creates a function compatible with deSolve::ode() that computes
#' derivatives for all stocks.
#'
#' Mathematical formula for stock σ:
#' dσ/dt = Σ_{f ∈ F(d)^{-1}(σ)} φ_f - Σ_{f ∈ F(u)^{-1}(σ)} φ_f
#'
#' That is: rate of change = inflows - outflows
#'
#' @param diagram StockFlowDiagram object
#' @return Function with signature function(time, state, params)
#'   that returns list(derivatives)
#'
#' @examples
#' \dontrun{
#' ode_func <- generate_ode(sir_diagram)
#' result <- deSolve::ode(
#'   y = initial_state(sir_diagram),
#'   times = seq(0, 100, by = 0.1),
#'   func = ode_func,
#'   parms = c(beta = 0.5, gamma = 0.1)
#' )
#' }
#'
#' @export
generate_ode <- function(diagram) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  # Validate diagram
  validate_diagram(diagram)

  # Build dependency graph and compute evaluation order
  deps <- build_dependency_graph(diagram)
  var_order <- topological_sort_variables(diagram, deps)

  # Pre-compute which variables are auxiliary vs sum
  aux_var_names <- sapply(diagram@variables, function(v) v@name)
  sum_var_names <- sapply(diagram@sumvars, function(v) v@name)

  # Cache stock names for faster lookup
  stock_names_vec <- sapply(diagram@stocks, function(s) s@name)

  # Return ODE function
  function(time, state, params) {
    # Ensure state is a named numeric vector
    if (is.null(names(state))) {
      names(state) <- stock_names_vec
    }

    # Convert params to list for $ operator support
    params_list <- as.list(params)

    # Initialize storage for variable values
    var_values <- list()
    sumvar_values <- list()

    # Evaluate variables in topological order
    for (var_name in var_order) {
      if (var_name %in% sum_var_names) {
        # Evaluate sum variable
        sumvar <- NULL
        for (sv in diagram@sumvars) {
          if (sv@name == var_name) {
            sumvar <- sv
            break
          }
        }

        if (!is.null(sumvar)) {
          # Sum the constituent stocks
          total <- 0
          for (stock_id in sumvar@stock_ids) {
            stock <- get_stock(diagram, stock_id)
            if (!is.null(stock)) {
              total <- total + state[[stock@name]]
            }
          }
          sumvar_values[[var_name]] <- total
        }
      } else if (var_name %in% aux_var_names) {
        # Evaluate auxiliary variable
        var <- NULL
        for (v in diagram@variables) {
          if (v@name == var_name) {
            var <- v
            break
          }
        }

        if (!is.null(var)) {
          tryCatch({
            var_values[[var_name]] <- var@expression(state, params_list, var_values)
          }, error = function(e) {
            stop("Error evaluating variable '", var_name, "': ", e$message)
          })
        }
      }
    }

    # Compute derivatives for each stock
    derivatives <- numeric(length(state))
    names(derivatives) <- names(state)

    for (i in seq_along(diagram@stocks)) {
      stock <- diagram@stocks[[i]]
      stock_name <- stock@name

      # Inflows: flows with this stock as target (d_map)
      inflow_ids <- get_flows_to_stock(diagram, stock@id)
      inflow_rate <- 0

      for (flow_id in inflow_ids) {
        flow <- get_flow(diagram, flow_id)
        if (!is.null(flow)) {
          rate <- evaluate_flow_rate(diagram, flow, state, params_list,
                                     var_values, sumvar_values)
          inflow_rate <- inflow_rate + rate
        }
      }

      # Outflows: flows with this stock as source (u_map)
      outflow_ids <- get_flows_from_stock(diagram, stock@id)
      outflow_rate <- 0

      for (flow_id in outflow_ids) {
        flow <- get_flow(diagram, flow_id)
        if (!is.null(flow)) {
          rate <- evaluate_flow_rate(diagram, flow, state, params_list,
                                     var_values, sumvar_values)
          outflow_rate <- outflow_rate + rate
        }
      }

      # dσ/dt = inflows - outflows
      derivatives[[stock_name]] <- inflow_rate - outflow_rate
    }

    # Return in deSolve format
    list(derivatives)
  }
}

# deSolve Integration ========================================================

#' Solve a stock-flow diagram as an ODE system
#'
#' Numerically integrates the ODE system using deSolve.
#'
#' @param diagram StockFlowDiagram object
#' @param times Numeric vector of time points
#' @param params Named numeric vector of parameters
#' @param method Integration method (default "lsoda"). See deSolve::ode.
#' @param ... Additional arguments passed to deSolve::ode
#' @return Data frame with columns: time, stocks...
#'
#' @examples
#' \dontrun{
#' result <- solve_diagram(
#'   sir,
#'   times = seq(0, 100, by = 0.1),
#'   params = c(beta = 0.5, gamma = 0.1)
#' )
#' }
#'
#' @export
solve_diagram <- function(diagram, times, params, method = "lsoda", ...) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  if (!requireNamespace("deSolve", quietly = TRUE)) {
    stop("Package 'deSolve' is required. Please install it with: install.packages('deSolve')")
  }

  # Get initial state
  state <- initial_state(diagram)

  if (length(state) == 0) {
    stop("Diagram has no stocks")
  }

  # Generate ODE function
  ode_func <- generate_ode(diagram)

  # Solve using deSolve
  result <- deSolve::ode(
    y = state,
    times = times,
    func = ode_func,
    parms = params,
    method = method,
    ...
  )

  # Convert to data frame
  result_df <- as.data.frame(result)

  # Add metadata as attributes
  attr(result_df, "diagram") <- diagram
  attr(result_df, "params") <- params

  # Add class for custom methods
  class(result_df) <- c("stockflow_solution", "data.frame")

  result_df
}

# Helper Functions ===========================================================

#' Extract parameters from a diagram's flow functions
#'
#' Attempts to identify parameter names from flow rate functions.
#' This is a heuristic and may not catch all parameters.
#'
#' @param diagram StockFlowDiagram object
#' @return Character vector of parameter names
#' @export
extract_parameters <- function(diagram) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  param_names <- character(0)

  # Look for params$name or params["name"] in flow functions
  for (flow in diagram@flows) {
    func_text <- paste(deparse(flow@rate_function), collapse = " ")

    # Extract params$name
    matches <- gregexpr("params\\$([a-zA-Z_][a-zA-Z0-9_]*)", func_text, perl = TRUE)
    if (matches[[1]][1] != -1) {
      matched_text <- regmatches(func_text, matches)[[1]]
      names <- gsub("params\\$", "", matched_text)
      param_names <- c(param_names, names)
    }

    # Extract params["name"] or params['name']
    matches <- gregexpr('params\\[[\"\']([a-zA-Z_][a-zA-Z0-9_]*)[\"\']\\]', func_text, perl = TRUE)
    if (matches[[1]][1] != -1) {
      matched_text <- regmatches(func_text, matches)[[1]]
      names <- gsub('params\\[[\"\']|[\"\']\\]', "", matched_text)
      param_names <- c(param_names, names)
    }
  }

  unique(param_names)
}

#' Check if ODE system is well-posed
#'
#' Performs basic checks to ensure the ODE system can be solved.
#'
#' @param diagram StockFlowDiagram object
#' @param params Named numeric vector of parameters
#' @return TRUE if well-posed, otherwise throws error with details
#' @export
check_ode_wellposed <- function(diagram, params) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  # Check that all stocks have finite initial values
  state <- initial_state(diagram)
  if (any(!is.finite(state))) {
    stop("Some stocks have non-finite initial values")
  }

  # Extract required parameters
  required_params <- extract_parameters(diagram)

  # Check that all required parameters are provided
  missing_params <- setdiff(required_params, names(params))
  if (length(missing_params) > 0) {
    stop("Missing parameters: ", paste(missing_params, collapse = ", "))
  }

  # Try evaluating ODE at t=0
  ode_func <- generate_ode(diagram)
  tryCatch({
    result <- ode_func(0, state, params)
    if (!is.list(result) || length(result) != 1) {
      stop("ODE function returned unexpected format")
    }
    if (!is.numeric(result[[1]])) {
      stop("ODE derivatives are not numeric")
    }
    if (any(!is.finite(result[[1]]))) {
      stop("ODE derivatives contain non-finite values at t=0")
    }
  }, error = function(e) {
    stop("Error evaluating ODE at t=0: ", e$message)
  })

  TRUE
}

# Print Methods ==============================================================

#' @export
print.stockflow_solution <- function(x, ...) {
  cat("Stock-Flow ODE Solution\n")
  cat("  Time points:", nrow(x), "\n")
  cat("  Time range: [", min(x$time), ",", max(x$time), "]\n")
  cat("  Stocks:", paste(setdiff(names(x), "time"), collapse = ", "), "\n")

  if (!is.null(attr(x, "params"))) {
    cat("  Parameters:\n")
    params <- attr(x, "params")
    for (pname in names(params)) {
      cat("    ", pname, "=", params[[pname]], "\n")
    }
  }

  cat("\n")
  print(as.data.frame(x), ...)
  invisible(x)
}

#' @export
summary.stockflow_solution <- function(object, ...) {
  cat("Stock-Flow ODE Solution Summary\n\n")

  cat("Time range: [", min(object$time), ",", max(object$time), "]\n")
  cat("Time points:", nrow(object), "\n\n")

  # Summary statistics for each stock
  stock_cols <- setdiff(names(object), "time")

  for (sname in stock_cols) {
    values <- object[[sname]]
    cat("Stock:", sname, "\n")
    cat("  Initial:", values[1], "\n")
    cat("  Final:", values[length(values)], "\n")
    cat("  Min:", min(values), "\n")
    cat("  Max:", max(values), "\n")
    cat("  Mean:", mean(values), "\n\n")
  }

  invisible(object)
}
