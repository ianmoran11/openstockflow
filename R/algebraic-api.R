#' Algebraic API for Stock-Flow Diagrams
#'
#' Provides an algebraic notation for constructing stock-flow diagrams
#' using operators like %->%, inspired by categorical notation.
#'
#' @name algebraic-api
NULL

# Stock and Flow Wrappers ====================================================

#' Create a stock specification
#'
#' Creates a stock specification that can be used with the algebraic API.
#' This does not add the stock to a diagram yet.
#'
#' @param name Character name for the stock
#' @param initial Numeric initial value (default 0)
#' @return A list with class "stock_spec"
#'
#' @examples
#' \dontrun{
#' S <- stock("S", initial = 1000)
#' I <- stock("I", initial = 1)
#' R <- stock("R", initial = 0)
#' }
#'
#' @export
stock <- function(name, initial = 0) {
  structure(
    list(
      name = name,
      initial = initial,
      type = "stock"
    ),
    class = "stock_spec"
  )
}

#' Create a flow specification
#'
#' Creates a flow specification that can be used with the algebraic API.
#' This does not add the flow to a diagram yet.
#'
#' @param name Character name for the flow
#' @param rate Function computing flow rate with signature function(inputs, params)
#' @param from Character name of source stock (set later with %->% operator)
#' @param to Character name of target stock (set later with %->% operator)
#' @return A list with class "flow_spec"
#'
#' @examples
#' \dontrun{
#' infection <- flow("infection",
#'   rate = function(inputs, params) {
#'     params$beta * inputs$S * inputs$I / inputs$N
#'   }
#' )
#' }
#'
#' @export
flow <- function(name, rate, from = NULL, to = NULL) {
  if (!is.function(rate)) {
    stop("rate must be a function")
  }

  structure(
    list(
      name = name,
      rate = rate,
      from = from,
      to = to,
      type = "flow"
    ),
    class = "flow_spec"
  )
}

#' Create a variable specification
#'
#' Creates a variable specification for use with the algebraic API.
#'
#' @param name Character name for the variable
#' @param expression Function computing variable value
#' @return A list with class "variable_spec"
#'
#' @examples
#' \dontrun{
#' N <- sumvar("N", stocks = c("S", "E", "I", "R"))
#' }
#'
#' @export
variable <- function(name, expression) {
  if (!is.function(expression)) {
    stop("expression must be a function")
  }

  structure(
    list(
      name = name,
      expression = expression,
      type = "variable"
    ),
    class = "variable_spec"
  )
}

#' Create a sum variable specification
#'
#' Creates a sum variable specification for use with the algebraic API.
#'
#' @param name Character name for the sum variable
#' @param stocks Character vector of stock names to sum
#' @return A list with class "sumvar_spec"
#'
#' @examples
#' \dontrun{
#' N <- sumvar("N", stocks = c("S", "E", "I", "R"))
#' }
#'
#' @export
sumvar <- function(name, stocks) {
  structure(
    list(
      name = name,
      stocks = stocks,
      type = "sumvar"
    ),
    class = "sumvar_spec"
  )
}

# Connection Operator ========================================================

#' Connect stocks via flows using algebraic notation
#'
#' The %->% operator creates a flow connection between two stocks.
#' It can be chained: stock1 %->% flow1 %->% stock2
#'
#' As of version 0.2.0, stocks referenced in flow connections are automatically
#' included in the diagram when finalize() is called, eliminating the need to
#' explicitly add them with %+%. This makes the API more concise while remaining
#' fully backward compatible.
#'
#' @param lhs Left-hand side: stock_spec or flow_spec (or NULL for external inflow)
#' @param rhs Right-hand side: flow_spec or stock_spec (or NULL for external outflow)
#' @return A partial or complete flow connection
#'
#' @examples
#' \dontrun{
#' # Define stocks
#' S <- stock("S", initial = 1000)
#' I <- stock("I", initial = 1)
#'
#' # Define flow
#' infection <- flow("infection",
#'   rate = function(inputs, params) params$beta * inputs$S * inputs$I
#' )
#'
#' # Connect: S %->% infection %->% I
#' connection <- S %->% infection %->% I
#'
#' # Stocks are automatically included when finalized
#' diagram <- finalize(connection)  # S and I automatically added!
#'
#' # External flows use NULL
#' inflow <- NULL %->% some_flow %->% Stock
#' outflow <- Stock %->% some_flow %->% NULL
#' }
#'
#' @export
`%->%` <- function(lhs, rhs) {
  # Case 1: stock %->% flow
  if (inherits(lhs, "stock_spec") && inherits(rhs, "flow_spec")) {
    # Create a partial connection (flow with source set)
    rhs$from <- lhs$name
    rhs$from_spec <- lhs  # Store the stock object for automatic inclusion
    structure(
      rhs,
      class = c("flow_connection", "flow_spec")
    )
  }
  # Case 2: flow %->% stock (completing the connection)
  else if (inherits(lhs, c("flow_connection", "flow_spec")) &&
           inherits(rhs, "stock_spec")) {
    # Complete the connection
    lhs$to <- rhs$name
    lhs$to_spec <- rhs  # Store the stock object for automatic inclusion
    structure(
      lhs,
      class = c("flow_connection_complete", "flow_connection", "flow_spec")
    )
  }
  # Case 3: NULL %->% stock (inflow)
  else if (is.null(lhs) && inherits(rhs, "flow_spec")) {
    rhs$from <- NULL
    rhs$from_spec <- NULL  # No source stock (external inflow)
    structure(
      rhs,
      class = c("flow_connection", "flow_spec")
    )
  }
  # Case 4: flow %->% NULL (outflow)
  else if (inherits(lhs, c("flow_connection", "flow_spec")) && is.null(rhs)) {
    lhs$to <- NULL
    lhs$to_spec <- NULL  # No target stock (external outflow)
    structure(
      lhs,
      class = c("flow_connection_complete", "flow_connection", "flow_spec")
    )
  }
  else {
    stop("Invalid connection: use stock %->% flow %->% stock")
  }
}

# Build Diagram from Specs ===================================================

#' Build a diagram from specifications
#'
#' Takes a list of stock, flow, and variable specifications and
#' constructs a StockFlowDiagram. As of version 0.2.0, stocks referenced
#' in flow connections are automatically included, so explicit stock
#' specifications are optional if they appear in connections.
#'
#' @param ... Stock specs, flow connections, and variable specs
#' @param reset_ids Logical; if TRUE, reset ID counter (default TRUE)
#' @return StockFlowDiagram object
#'
#' @details
#' Stocks can be specified explicitly or implicitly:
#' - Explicit: Pass stock specs directly as arguments
#' - Implicit: Stocks referenced in flow connections are automatically added
#' - Isolated stocks (no flows) must be specified explicitly
#' - Explicit specifications take precedence over implicit
#'
#' @examples
#' \dontrun{
#' # Define components
#' S <- stock("S", initial = 1000)
#' I <- stock("I", initial = 1)
#' R <- stock("R", initial = 0)
#'
#' infection <- flow("infection",
#'   rate = function(inputs, params) params$beta * inputs$S * inputs$I
#' )
#'
#' recovery <- flow("recovery",
#'   rate = function(inputs, params) params$gamma * inputs$I
#' )
#'
#' # Old style (explicit stocks - still works)
#' sir1 <- build_diagram(
#'   S, I, R,
#'   S %->% infection %->% I,
#'   I %->% recovery %->% R
#' )
#'
#' # New style (automatic stock inclusion)
#' sir2 <- build_diagram(
#'   S %->% infection %->% I,
#'   I %->% recovery %->% R
#' )  # S, I, R automatically added!
#' }
#'
#' @export
build_diagram <- function(..., reset_ids = TRUE) {
  specs <- list(...)

  # Extract implicit stocks from flow connections
  # This allows stocks referenced in flows to be automatically included
  implicit_stocks <- list()
  for (spec in specs) {
    if (inherits(spec, "flow_connection_complete")) {
      # Extract source stock if present
      if (!is.null(spec$from_spec)) {
        implicit_stocks[[spec$from]] <- spec$from_spec
      }
      # Extract target stock if present
      if (!is.null(spec$to_spec)) {
        implicit_stocks[[spec$to]] <- spec$to_spec
      }
    }
  }

  # Merge implicit stocks with explicit stocks (explicit takes precedence)
  explicit_stock_names <- character(0)
  for (spec in specs) {
    if (inherits(spec, "stock_spec")) {
      explicit_stock_names <- c(explicit_stock_names, spec$name)
    }
  }

  # Add implicit stocks that aren't already explicit
  for (stock_name in names(implicit_stocks)) {
    if (!(stock_name %in% explicit_stock_names)) {
      # Add implicit stock to specs list
      specs <- c(specs, list(implicit_stocks[[stock_name]]))
    }
  }

  # Start with empty diagram
  diagram <- stock_flow_diagram(reset_ids = reset_ids)

  # First pass: add all stocks (now includes implicit stocks!)
  for (spec in specs) {
    if (inherits(spec, "stock_spec")) {
      diagram <- add_stock(diagram, spec$name, initial = spec$initial)
    }
  }

  # Second pass: add all flow connections
  for (spec in specs) {
    if (inherits(spec, "flow_connection_complete")) {
      diagram <- add_flow(diagram,
        name = spec$name,
        from = spec$from,
        to = spec$to,
        rate = spec$rate
      )
    }
  }

  # Third pass: add variables
  for (spec in specs) {
    if (inherits(spec, "variable_spec")) {
      diagram <- add_variable(diagram,
        name = spec$name,
        expression = spec$expression
      )
    } else if (inherits(spec, "sumvar_spec")) {
      diagram <- add_sum_variable(diagram,
        name = spec$name,
        stocks = spec$stocks
      )
    }
  }

  diagram
}

# Combination Operator ======================================================

#' Combine stocks and flows using %+% operator
#'
#' The %+% operator combines specifications and builds a diagram incrementally.
#' It can combine:
#' - stock_spec + stock_spec
#' - stock_spec + flow_connection
#' - diagram_spec + stock_spec
#' - diagram_spec + flow_connection
#'
#' @param lhs Left-hand side
#' @param rhs Right-hand side
#' @return A diagram_spec (collection of specs) or StockFlowDiagram
#'
#' @examples
#' \dontrun{
#' # Define components
#' S <- stock("S", initial = 999)
#' I <- stock("I", initial = 1)
#' R <- stock("R", initial = 0)
#'
#' infection <- flow("infection",
#'   rate = function(inputs, params) params$beta * inputs$S * inputs$I
#' )
#'
#' recovery <- flow("recovery",
#'   rate = function(inputs, params) params$gamma * inputs$I
#' )
#'
#' # Combine with %+%
#' sir <- S %+% I %+% R %+%
#'        (S %->% infection %->% I) %+%
#'        (I %->% recovery %->% R)
#' }
#'
#' @export
`%+%` <- function(lhs, rhs) {
  # Helper to extract specs from an object
  get_specs <- function(x) {
    if (inherits(x, "diagram_spec")) {
      x$specs
    } else {
      list(x)
    }
  }

  # Combine specs from both sides
  combined_specs <- c(get_specs(lhs), get_specs(rhs))

  # Wrap in diagram_spec
  structure(
    list(specs = combined_specs),
    class = "diagram_spec"
  )
}

#' Finalize a diagram specification
#'
#' Converts a diagram_spec into an actual StockFlowDiagram.
#' This is called automatically when printing or when explicitly requested.
#'
#' @param spec diagram_spec object
#' @param reset_ids Logical; if TRUE, reset ID counter (default TRUE)
#' @return StockFlowDiagram object
#'
#' @export
finalize <- function(spec, reset_ids = TRUE) {
  if (inherits(spec, "StockFlowDiagram")) {
    return(spec)  # Already finalized
  }

  if (!inherits(spec, "diagram_spec")) {
    # Try to build from single spec
    spec <- structure(list(specs = list(spec)), class = "diagram_spec")
  }

  do.call(build_diagram, c(spec$specs, list(reset_ids = reset_ids)))
}

# Alternative: Collect and Build ============================================

#' Start a diagram specification
#'
#' Alternative API that collects specifications and builds the diagram
#' at the end.
#'
#' @return A diagram builder object
#'
#' @examples
#' \dontrun{
#' sir <- model() +
#'   stock("S", initial = 1000) +
#'   stock("I", initial = 1) +
#'   stock("R", initial = 0) +
#'   (S %->% flow("infection", rate = ...) %->% I) +
#'   (I %->% flow("recovery", rate = ...) %->% R)
#' }
#'
#' @export
model <- function() {
  structure(
    list(specs = list()),
    class = "model_builder"
  )
}

#' Add specification to model builder
#'
#' @param builder model_builder object
#' @param spec Specification to add
#' @return Updated model_builder
#'
#' @export
`+.model_builder` <- function(builder, spec) {
  if (!inherits(builder, "model_builder")) {
    stop("Left side must be a model_builder")
  }

  builder$specs <- c(builder$specs, list(spec))
  builder
}

#' Compile model builder into diagram
#'
#' @param builder model_builder object
#' @param reset_ids Logical; if TRUE, reset ID counter
#' @return StockFlowDiagram object
#'
#' @export
compile <- function(builder, reset_ids = TRUE) {
  if (!inherits(builder, "model_builder")) {
    stop("builder must be a model_builder")
  }

  do.call(build_diagram, c(builder$specs, list(reset_ids = reset_ids)))
}

# Print Methods ==============================================================

#' @export
print.stock_spec <- function(x, ...) {
  cat("Stock:", x$name, "(initial =", x$initial, ")\n")
  invisible(x)
}

#' @export
print.flow_spec <- function(x, ...) {
  cat("Flow:", x$name)
  if (!is.null(x$from)) cat(", from:", x$from)
  if (!is.null(x$to)) cat(", to:", x$to)
  cat("\n")
  invisible(x)
}

#' @export
print.flow_connection <- function(x, ...) {
  cat("Flow connection:", x$name)
  if (!is.null(x$from)) cat(", from:", x$from)
  if (!is.null(x$to)) cat(", to:", x$to)
  cat("\n")
  invisible(x)
}

#' @export
print.variable_spec <- function(x, ...) {
  cat("Variable:", x$name, "\n")
  invisible(x)
}

#' @export
print.sumvar_spec <- function(x, ...) {
  cat("Sum variable:", x$name, "(", paste(x$stocks, collapse = " + "), ")\n")
  invisible(x)
}

#' @export
print.model_builder <- function(x, ...) {
  cat("Model builder with", length(x$specs), "specifications\n")
  invisible(x)
}

#' @export
print.diagram_spec <- function(x, ...) {
  # Automatically finalize and print the diagram
  diagram <- finalize(x)
  print(diagram)
  invisible(diagram)
}
