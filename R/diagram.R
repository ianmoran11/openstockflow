#' Stock-Flow Diagram Construction
#'
#' Functions for constructing and manipulating stock-flow diagrams.
#'
#' @name diagram-construction
NULL

# ID Management ==============================================================

#' ID generator environment
#'
#' @keywords internal
.id_env <- new.env(parent = emptyenv())
.id_env$counter <- 0L

#' Generate a unique ID
#'
#' @return Integer ID
#' @keywords internal
next_id <- function() {
  .id_env$counter <- .id_env$counter + 1L
  .id_env$counter
}

#' Reset ID counter (for testing)
#'
#' @keywords internal
reset_id_counter <- function() {
  .id_env$counter <- 0L
}

# Empty Diagram Constructor ==================================================

#' Create an empty stock-flow diagram
#'
#' @param reset_ids Logical; if TRUE, reset the ID counter (default FALSE)
#' @return Empty StockFlowDiagram object
#'
#' @examples
#' \dontrun{
#' diagram <- stock_flow_diagram()
#' }
#'
#' @export
stock_flow_diagram <- function(reset_ids = FALSE) {
  if (reset_ids) {
    reset_id_counter()
  }

  new("StockFlowDiagram")
}

# Add Stock ==================================================================

#' Add a stock to a diagram
#'
#' @param diagram StockFlowDiagram object (or NULL to create new)
#' @param name Character name for the stock
#' @param initial Numeric initial value (default 0)
#' @return Modified StockFlowDiagram object
#'
#' @examples
#' \dontrun{
#' diagram <- stock_flow_diagram() %>%
#'   add_stock("S", initial = 1000) %>%
#'   add_stock("I", initial = 1)
#' }
#'
#' @export
add_stock <- function(diagram = NULL, name, initial = 0) {
  # Allow piping from NULL
  if (is.null(diagram)) {
    diagram <- stock_flow_diagram()
  }

  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram or NULL")
  }

  # Check for duplicate names
  if (!is.null(get_stock_by_name(diagram, name))) {
    stop("Stock with name '", name, "' already exists")
  }

  # Generate ID
  stock_id <- next_id()

  # Create Stock object
  stock <- new("Stock",
    id = stock_id,
    name = name,
    initial_value = as.numeric(initial)
  )

  # Add to diagram
  diagram@stocks <- c(diagram@stocks, list(stock))
  diagram@stock_ids <- c(diagram@stock_ids, stock_id)

  diagram
}

# Add Flow ===================================================================

#' Add a flow to a diagram
#'
#' @param diagram StockFlowDiagram object
#' @param name Character name for the flow
#' @param from Character name of source stock (or NULL for inflow)
#' @param to Character name of target stock (or NULL for outflow)
#' @param rate Function computing flow rate, with signature
#'   function(inputs, params). Can also be a formula (converted to function).
#' @return Modified StockFlowDiagram object
#'
#' @examples
#' \dontrun{
#' diagram <- stock_flow_diagram() %>%
#'   add_stock("S", initial = 1000) %>%
#'   add_stock("I", initial = 1) %>%
#'   add_flow("infection",
#'     from = "S",
#'     to = "I",
#'     rate = function(inputs, params) {
#'       params$beta * inputs$S * inputs$I
#'     }
#'   )
#' }
#'
#' @export
add_flow <- function(diagram, name, from = NULL, to = NULL, rate) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  # Check that at least one endpoint is specified
  if (is.null(from) && is.null(to)) {
    stop("Flow must have at least one endpoint (from or to)")
  }

  # Check for duplicate names
  if (!is.null(get_flow_by_name(diagram, name))) {
    stop("Flow with name '", name, "' already exists")
  }

  # Resolve source stock ID
  if (is.null(from)) {
    source_id <- NA_integer_
  } else {
    source_stock <- get_stock_by_name(diagram, from)
    if (is.null(source_stock)) {
      stop("Source stock '", from, "' not found")
    }
    source_id <- source_stock@id
  }

  # Resolve target stock ID
  if (is.null(to)) {
    target_id <- NA_integer_
  } else {
    target_stock <- get_stock_by_name(diagram, to)
    if (is.null(target_stock)) {
      stop("Target stock '", to, "' not found")
    }
    target_id <- target_stock@id
  }

  # Convert rate to function if needed
  if (!is.function(rate)) {
    stop("rate must be a function with signature function(inputs, params)")
  }

  # Generate ID
  flow_id <- next_id()

  # Create Flow object
  flow <- new("Flow",
    id = flow_id,
    name = name,
    rate_function = rate,
    source = source_id,
    target = target_id
  )

  # Add to diagram
  diagram@flows <- c(diagram@flows, list(flow))
  diagram@flow_ids <- c(diagram@flow_ids, flow_id)
  diagram@u_map <- c(diagram@u_map, source_id)
  diagram@d_map <- c(diagram@d_map, target_id)

  # Auto-create links from source and target stocks to this flow
  # (links indicate which values the flow rate function depends on)
  if (!is.na(source_id)) {
    diagram <- add_link_internal(diagram,
      name = paste0(from, "_to_", name),
      from_id = source_id,
      from_type = "stock",
      to_flow = flow_id
    )
  }
  # Add target link only if target exists and is different from source
  if (!is.na(target_id)) {
    if (is.na(source_id) || target_id != source_id) {
      diagram <- add_link_internal(diagram,
        name = paste0(to, "_to_", name),
        from_id = target_id,
        from_type = "stock",
        to_flow = flow_id
      )
    }
  }

  diagram
}

# Add Link (Internal) ========================================================

#' Add a link to a diagram (internal function)
#'
#' @param diagram StockFlowDiagram object
#' @param name Character name for the link
#' @param from_id Integer source ID
#' @param from_type Character source type ("stock", "variable", "sumvar")
#' @param to_flow Integer target flow ID
#' @return Modified StockFlowDiagram object
#' @keywords internal
add_link_internal <- function(diagram, name, from_id, from_type, to_flow) {
  # Generate ID
  link_id <- next_id()

  # Create Link object
  link <- new("Link",
    id = link_id,
    name = name,
    source_id = from_id,
    source_type = from_type,
    target_flow = to_flow
  )

  # Add to diagram
  diagram@links <- c(diagram@links, list(link))
  diagram@link_ids <- c(diagram@link_ids, link_id)
  diagram@s_map <- c(diagram@s_map, from_id)
  diagram@t_map <- c(diagram@t_map, to_flow)

  diagram
}

#' Add a link from a variable to a flow
#'
#' Links indicate which quantities (stocks or variables) a flow's
#' rate function depends on.
#'
#' @param diagram StockFlowDiagram object
#' @param from Character name of source (stock or variable)
#' @param to Character name of target flow
#' @return Modified StockFlowDiagram object
#'
#' @examples
#' \dontrun{
#' diagram <- diagram %>%
#'   add_link(from = "N", to = "infection")
#' }
#'
#' @export
add_link <- function(diagram, from, to) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  # Find source (stock or variable)
  source_stock <- get_stock_by_name(diagram, from)
  source_var <- get_variable_by_name(diagram, from)

  if (!is.null(source_stock)) {
    from_id <- source_stock@id
    from_type <- "stock"
  } else if (!is.null(source_var)) {
    from_id <- source_var@id
    from_type <- if (inherits(source_var, "SumVariable")) "sumvar" else "variable"
  } else {
    stop("Source '", from, "' not found (must be a stock or variable)")
  }

  # Find target flow
  target_flow <- get_flow_by_name(diagram, to)
  if (is.null(target_flow)) {
    stop("Target flow '", to, "' not found")
  }

  # Add link
  add_link_internal(diagram,
    name = paste0(from, "_to_", to),
    from_id = from_id,
    from_type = from_type,
    to_flow = target_flow@id
  )
}

# Add Auxiliary Variable =====================================================

#' Add an auxiliary variable to a diagram
#'
#' Auxiliary variables are computed values that can be used in flow
#' rate functions. They appear as circles in stock-flow diagrams.
#'
#' @param diagram StockFlowDiagram object
#' @param name Character name for the variable
#' @param expression Function computing the variable value, with signature
#'   function(state, params, var_values)
#' @return Modified StockFlowDiagram object
#'
#' @examples
#' \dontrun{
#' diagram <- diagram %>%
#'   add_variable("force_of_infection",
#'     expression = function(state, params, var_values) {
#'       params$beta * state["I"] / sum(state)
#'     }
#'   )
#' }
#'
#' @export
add_variable <- function(diagram, name, expression) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  # Check for duplicate names
  if (!is.null(get_variable_by_name(diagram, name))) {
    stop("Variable with name '", name, "' already exists")
  }

  if (!is.function(expression)) {
    stop("expression must be a function")
  }

  # Generate ID
  var_id <- next_id()

  # Create AuxiliaryVariable object
  var <- new("AuxiliaryVariable",
    id = var_id,
    name = name,
    expression = expression
  )

  # Add to diagram
  diagram@variables <- c(diagram@variables, list(var))

  diagram
}

# Add Sum Variable ===========================================================

#' Add a sum variable to a diagram
#'
#' Sum variables compute the sum of multiple stocks. Common use case
#' is computing total population N = S + E + I + R.
#'
#' @param diagram StockFlowDiagram object
#' @param name Character name for the sum variable
#' @param stocks Character vector of stock names to sum
#' @return Modified StockFlowDiagram object
#'
#' @examples
#' \dontrun{
#' diagram <- diagram %>%
#'   add_sum_variable("N", stocks = c("S", "E", "I", "R"))
#' }
#'
#' @export
add_sum_variable <- function(diagram, name, stocks) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  # Check for duplicate names
  if (!is.null(get_variable_by_name(diagram, name))) {
    stop("Variable with name '", name, "' already exists")
  }

  # Resolve stock IDs
  stock_ids <- integer(length(stocks))
  for (i in seq_along(stocks)) {
    stock <- get_stock_by_name(diagram, stocks[i])
    if (is.null(stock)) {
      stop("Stock '", stocks[i], "' not found")
    }
    stock_ids[i] <- stock@id
  }

  # Generate ID
  var_id <- next_id()

  # Create SumVariable object
  var <- new("SumVariable",
    id = var_id,
    name = name,
    stock_ids = stock_ids
  )

  # Add to diagram
  diagram@sumvars <- c(diagram@sumvars, list(var))

  diagram
}

# Set Interface ==============================================================

#' Set the interface for composition
#'
#' The interface specifies which stocks are exposed for composition
#' with other diagrams.
#'
#' @param diagram StockFlowDiagram object
#' @param stocks Character vector of stock names to expose
#' @return Modified StockFlowDiagram object
#'
#' @examples
#' \dontrun{
#' diagram <- diagram %>%
#'   set_interface(c("S", "V"))
#' }
#'
#' @export
set_interface <- function(diagram, stocks) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  # Resolve stock IDs
  stock_ids <- integer(length(stocks))
  for (i in seq_along(stocks)) {
    stock <- get_stock_by_name(diagram, stocks[i])
    if (is.null(stock)) {
      stop("Stock '", stocks[i], "' not found")
    }
    stock_ids[i] <- stock@id
  }

  diagram@interface <- stock_ids
  diagram
}

# Diagram Summary Functions ==================================================

#' Get number of stocks in diagram
#'
#' @param diagram StockFlowDiagram object
#' @return Integer count
#' @export
n_stocks <- function(diagram) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  length(diagram@stocks)
}

#' Get number of flows in diagram
#'
#' @param diagram StockFlowDiagram object
#' @return Integer count
#' @export
n_flows <- function(diagram) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  length(diagram@flows)
}

#' Get number of links in diagram
#'
#' @param diagram StockFlowDiagram object
#' @return Integer count
#' @export
n_links <- function(diagram) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  length(diagram@links)
}

#' Get stock names
#'
#' @param diagram StockFlowDiagram object
#' @return Character vector of stock names
#' @export
stock_names <- function(diagram) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  sapply(diagram@stocks, function(s) s@name)
}

#' Get flow names
#'
#' @param diagram StockFlowDiagram object
#' @return Character vector of flow names
#' @export
flow_names <- function(diagram) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  sapply(diagram@flows, function(f) f@name)
}

#' Get initial state vector
#'
#' @param diagram StockFlowDiagram object
#' @return Named numeric vector of initial values
#' @export
initial_state <- function(diagram) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  if (length(diagram@stocks) == 0) {
    return(numeric(0))
  }

  state <- sapply(diagram@stocks, function(s) s@initial_value)
  names(state) <- sapply(diagram@stocks, function(s) s@name)
  state
}

# Validation =================================================================

#' Validate a stock-flow diagram
#'
#' Checks that the diagram is well-formed and satisfies categorical
#' properties.
#'
#' @param diagram StockFlowDiagram object
#' @param strict Logical; if TRUE, perform additional checks
#' @return TRUE if valid, otherwise throws an error
#' @export
validate_diagram <- function(diagram, strict = FALSE) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  # Check functor properties
  functor_check <- validate_functor(diagram)
  if (!isTRUE(functor_check)) {
    stop("Functor validation failed: ", functor_check)
  }

  # Check that all flow sources/targets exist
  for (i in seq_along(diagram@flows)) {
    flow <- diagram@flows[[i]]
    if (!is.na(flow@source) && is.null(get_stock(diagram, flow@source))) {
      stop("Flow '", flow@name, "' has invalid source stock ID")
    }
    if (!is.na(flow@target) && is.null(get_stock(diagram, flow@target))) {
      stop("Flow '", flow@name, "' has invalid target stock ID")
    }
  }

  # Check that all link sources exist
  for (i in seq_along(diagram@links)) {
    link <- diagram@links[[i]]
    if (link@source_type == "stock") {
      if (is.null(get_stock(diagram, link@source_id))) {
        stop("Link '", link@name, "' has invalid source stock ID")
      }
    }
    # Check that target flow exists
    if (is.null(get_flow(diagram, link@target_flow))) {
      stop("Link '", link@name, "' has invalid target flow ID")
    }
  }

  # Strict checks
  if (strict) {
    # Check that all flows have at least one link
    for (flow in diagram@flows) {
      links <- get_links_to_flow(diagram, flow@id)
      if (length(links) == 0) {
        warning("Flow '", flow@name, "' has no links (rate function inputs unclear)")
      }
    }
  }

  TRUE
}
