#' Primitive Category H for Stock-Flow Diagrams
#'
#' This file implements the primitive category H with objects {stock, flow, link}
#' and morphisms {u, d, s, t}, following the mathematical framework from
#' Baez et al. (2022).
#'
#' @name primitives
#' @keywords internal
NULL

#' Primitive Category H
#'
#' The primitive category H is defined as:
#' - Objects: stock, flow, link
#' - Morphisms:
#'   - u: flow → stock (upstream/source)
#'   - d: flow → stock (downstream/target)
#'   - s: link → stock/variable (source)
#'   - t: link → flow (target)
#'
#' A stock-flow diagram is a functor F: H → FinSet that maps:
#' - F(stock) = set of stock IDs
#' - F(flow) = set of flow IDs
#' - F(link) = set of link IDs
#' - F(u) = function mapping flows to source stocks
#' - F(d) = function mapping flows to target stocks
#' - F(s) = function mapping links to source stocks/variables
#' - F(t) = function mapping links to target flows
#'
#' @references
#' Baez, J. C., Li, X., Libkind, S., Osgood, N., & Redekopp, E. (2022).
#' Compositional Modeling with Stock and Flow Diagrams. arXiv:2205.08373
#'
#' @keywords internal
#' @name category-h
NULL

# Functor Validation =========================================================

#' Check if morphism maps are valid
#'
#' @param diagram StockFlowDiagram object
#' @return TRUE if valid, character string with error message otherwise
#' @keywords internal
validate_functor <- function(diagram) {
  # Check that u_map and d_map are functions from flows to stocks (or NA)
  valid_stock_ids <- c(NA_integer_, diagram@stock_ids)

  if (!all(diagram@u_map %in% valid_stock_ids)) {
    return("u_map (upstream) must map flows to stocks or NA")
  }

  if (!all(diagram@d_map %in% valid_stock_ids)) {
    return("d_map (downstream) must map flows to stocks or NA")
  }

  # Check that t_map is a function from links to flows
  if (!all(diagram@t_map %in% diagram@flow_ids)) {
    return("t_map (target) must map links to flows")
  }

  # Check that s_map maps links to stocks or variables
  # (we'll validate this exists in the diagram separately)
  if (any(is.na(diagram@s_map))) {
    return("s_map (source) cannot contain NA values")
  }

  TRUE
}

#' Check if a functor preserves composition
#'
#' In category H, there are no non-trivial compositions to check
#' (all morphisms are primitive), but this function is a placeholder
#' for potential future extensions.
#'
#' @param diagram StockFlowDiagram object
#' @return TRUE if valid
#' @keywords internal
validate_functor_composition <- function(diagram) {
  # Primitive category H has no composite morphisms
  # This is a placeholder for potential extensions
  TRUE
}

# Morphism Query Functions ===================================================

#' Get flows with given source stock
#'
#' @param diagram StockFlowDiagram object
#' @param stock_id Integer stock ID
#' @return Integer vector of flow IDs
#' @keywords internal
#' @export
get_flows_from_stock <- function(diagram, stock_id) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  diagram@flow_ids[diagram@u_map == stock_id]
}

#' Get flows with given target stock
#'
#' @param diagram StockFlowDiagram object
#' @param stock_id Integer stock ID
#' @return Integer vector of flow IDs
#' @keywords internal
#' @export
get_flows_to_stock <- function(diagram, stock_id) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  diagram@flow_ids[diagram@d_map == stock_id]
}

#' Get inflows (flows with NA source)
#'
#' @param diagram StockFlowDiagram object
#' @return Integer vector of flow IDs
#' @keywords internal
#' @export
get_inflows <- function(diagram) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  diagram@flow_ids[is.na(diagram@u_map)]
}

#' Get outflows (flows with NA target)
#'
#' @param diagram StockFlowDiagram object
#' @return Integer vector of flow IDs
#' @keywords internal
#' @export
get_outflows <- function(diagram) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  diagram@flow_ids[is.na(diagram@d_map)]
}

#' Get links targeting a flow
#'
#' @param diagram StockFlowDiagram object
#' @param flow_id Integer flow ID
#' @return Integer vector of link IDs
#' @keywords internal
#' @export
get_links_to_flow <- function(diagram, flow_id) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  diagram@link_ids[diagram@t_map == flow_id]
}

#' Get links from a source (stock or variable)
#'
#' @param diagram StockFlowDiagram object
#' @param source_id Integer source ID
#' @return Integer vector of link IDs
#' @keywords internal
#' @export
get_links_from_source <- function(diagram, source_id) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  diagram@link_ids[diagram@s_map == source_id]
}

# Element Lookup Functions ===================================================

#' Get stock by ID
#'
#' @param diagram StockFlowDiagram object
#' @param stock_id Integer stock ID
#' @return Stock object or NULL if not found
#' @keywords internal
#' @export
get_stock <- function(diagram, stock_id) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  idx <- which(diagram@stock_ids == stock_id)
  if (length(idx) == 0) return(NULL)
  diagram@stocks[[idx]]
}

#' Get stock by name
#'
#' @param diagram StockFlowDiagram object
#' @param name Character stock name
#' @return Stock object or NULL if not found
#' @keywords internal
#' @export
get_stock_by_name <- function(diagram, name) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  for (stock in diagram@stocks) {
    if (stock@name == name) return(stock)
  }
  NULL
}

#' Get flow by ID
#'
#' @param diagram StockFlowDiagram object
#' @param flow_id Integer flow ID
#' @return Flow object or NULL if not found
#' @keywords internal
#' @export
get_flow <- function(diagram, flow_id) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  idx <- which(diagram@flow_ids == flow_id)
  if (length(idx) == 0) return(NULL)
  diagram@flows[[idx]]
}

#' Get flow by name
#'
#' @param diagram StockFlowDiagram object
#' @param name Character flow name
#' @return Flow object or NULL if not found
#' @keywords internal
#' @export
get_flow_by_name <- function(diagram, name) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  for (flow in diagram@flows) {
    if (flow@name == name) return(flow)
  }
  NULL
}

#' Get link by ID
#'
#' @param diagram StockFlowDiagram object
#' @param link_id Integer link ID
#' @return Link object or NULL if not found
#' @keywords internal
#' @export
get_link <- function(diagram, link_id) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }
  idx <- which(diagram@link_ids == link_id)
  if (length(idx) == 0) return(NULL)
  diagram@links[[idx]]
}

#' Get variable by name
#'
#' @param diagram StockFlowDiagram object
#' @param name Character variable name
#' @return AuxiliaryVariable or SumVariable object, or NULL if not found
#' @keywords internal
#' @export
get_variable_by_name <- function(diagram, name) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  # Check auxiliary variables
  for (var in diagram@variables) {
    if (var@name == name) return(var)
  }

  # Check sum variables
  for (var in diagram@sumvars) {
    if (var@name == name) return(var)
  }

  NULL
}

# Functor Composition Helpers ================================================

#' Compose two morphisms
#'
#' @param f Integer vector representing first morphism
#' @param g Integer vector representing second morphism
#' @return Integer vector representing composed morphism g ∘ f
#' @keywords internal
compose_morphisms <- function(f, g) {
  # For each element i in domain of f, compute g(f(i))
  # Handle NA values (for inflows/outflows)
  result <- integer(length(f))
  for (i in seq_along(f)) {
    if (is.na(f[i])) {
      result[i] <- NA_integer_
    } else {
      # Find g(f[i])
      idx <- which(seq_along(g) == f[i])
      if (length(idx) > 0) {
        result[i] <- g[idx]
      } else {
        result[i] <- NA_integer_
      }
    }
  }
  result
}

#' Check if two diagrams have compatible interfaces
#'
#' @param left StockFlowDiagram or DecoratedCospan
#' @param right StockFlowDiagram or DecoratedCospan
#' @return Logical indicating compatibility
#' @keywords internal
check_interface_compatibility <- function(left, right) {
  if (inherits(left, "DecoratedCospan")) {
    left_interface <- left@right_foot
  } else if (inherits(left, "StockFlowDiagram")) {
    left_interface <- left@interface
  } else {
    stop("left must be a StockFlowDiagram or DecoratedCospan")
  }

  if (inherits(right, "DecoratedCospan")) {
    right_interface <- right@left_foot
  } else if (inherits(right, "StockFlowDiagram")) {
    right_interface <- right@interface
  } else {
    stop("right must be a StockFlowDiagram or DecoratedCospan")
  }

  # Interfaces are compatible if they have the same cardinality
  # (actual stock matching happens during composition)
  length(left_interface) == length(right_interface)
}
