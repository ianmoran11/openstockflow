#' S4 Classes for Stock-Flow Diagrams
#'
#' This file defines the core S4 classes for representing stock-flow diagrams
#' using categorical structures (functors, decorated cospans).
#'
#' @name classes
#' @keywords internal
NULL

# Base Class for Diagram Elements ============================================

#' Base class for all diagram elements
#'
#' @slot id Integer identifier (unique within a diagram)
#' @slot name Character name (human-readable label)
#'
#' @keywords internal
#' @export
setClass("DiagramElement",
  slots = c(
    id = "integer",
    name = "character"
  ),
  validity = function(object) {
    if (length(object@id) != 1)
      return("id must be a single integer")
    if (length(object@name) != 1)
      return("name must be a single character string")
    if (nchar(object@name) == 0)
      return("name cannot be empty")
    TRUE
  }
)

# Stock Class ================================================================

#' Stock (rectangle in stock-flow diagrams)
#'
#' Represents a stock (accumulation) in a stock-flow diagram. Stocks are
#' depicted as rectangles and represent quantities that accumulate over time.
#'
#' @slot id Integer identifier
#' @slot name Character name
#' @slot initial_value Numeric initial value for ODE integration
#'
#' @examples
#' \dontrun{
#' susceptible <- new("Stock",
#'   id = 1L,
#'   name = "S",
#'   initial_value = 1000
#' )
#' }
#'
#' @export
setClass("Stock",
  contains = "DiagramElement",
  slots = c(
    initial_value = "numeric"
  ),
  validity = function(object) {
    if (length(object@initial_value) != 1)
      return("initial_value must be a single numeric value")
    if (is.na(object@initial_value))
      return("initial_value cannot be NA")
    if (!is.finite(object@initial_value))
      return("initial_value must be finite")
    TRUE
  }
)

# Flow Class =================================================================

#' Flow (thick arrow in stock-flow diagrams)
#'
#' Represents a flow between stocks. Flows are depicted as thick arrows and
#' represent rates of change. A flow can have a source stock (upstream),
#' a target stock (downstream), both, or neither (for inflows/outflows).
#'
#' @slot id Integer identifier
#' @slot name Character name
#' @slot rate_function Function computing the flow rate: φ_f: R^n → R
#' @slot source Integer ID of source stock (NA_integer_ for inflow)
#' @slot target Integer ID of target stock (NA_integer_ for outflow)
#'
#' @details
#' The rate_function should have signature: function(inputs, params)
#' where inputs is a named list of values and params is a named vector
#' of parameters. It should return a single numeric value.
#'
#' @examples
#' \dontrun{
#' infection_flow <- new("Flow",
#'   id = 1L,
#'   name = "infection",
#'   rate_function = function(inputs, params) {
#'     params["beta"] * inputs$S * inputs$I / inputs$N
#'   },
#'   source = 1L,  # S
#'   target = 2L   # E
#' )
#' }
#'
#' @export
setClass("Flow",
  contains = "DiagramElement",
  slots = c(
    rate_function = "function",
    source = "integer",
    target = "integer"
  ),
  validity = function(object) {
    # Check source and target
    if (length(object@source) != 1)
      return("source must be a single integer or NA_integer_")
    if (length(object@target) != 1)
      return("target must be a single integer or NA_integer_")

    # At least one endpoint must be defined
    if (is.na(object@source) && is.na(object@target))
      return("Flow must have at least one endpoint (source or target)")

    # Check rate_function signature
    rate_args <- names(formals(object@rate_function))
    if (length(rate_args) < 2)
      return("rate_function must accept at least 2 arguments (inputs, params)")

    TRUE
  }
)

# Link Class =================================================================

#' Link (thin arrow in stock-flow diagrams)
#'
#' Represents a dependency link from a stock or variable to a flow.
#' Links are depicted as thin arrows and indicate which quantities
#' influence a flow's rate.
#'
#' @slot id Integer identifier
#' @slot name Character name
#' @slot source_id Integer ID of source (stock, variable, or sum variable)
#' @slot source_type Character type: "stock", "variable", or "sumvar"
#' @slot target_flow Integer ID of target flow
#'
#' @examples
#' \dontrun{
#' link_S_to_infection <- new("Link",
#'   id = 1L,
#'   name = "S_to_infection",
#'   source_id = 1L,
#'   source_type = "stock",
#'   target_flow = 1L
#' )
#' }
#'
#' @export
setClass("Link",
  contains = "DiagramElement",
  slots = c(
    source_id = "integer",
    source_type = "character",
    target_flow = "integer"
  ),
  validity = function(object) {
    if (length(object@source_id) != 1)
      return("source_id must be a single integer")
    if (is.na(object@source_id))
      return("source_id cannot be NA")

    if (length(object@source_type) != 1)
      return("source_type must be a single character string")
    valid_types <- c("stock", "variable", "sumvar")
    if (!object@source_type %in% valid_types)
      return(paste("source_type must be one of:", paste(valid_types, collapse = ", ")))

    if (length(object@target_flow) != 1)
      return("target_flow must be a single integer")
    if (is.na(object@target_flow))
      return("target_flow cannot be NA")

    TRUE
  }
)

# AuxiliaryVariable Class ====================================================

#' Auxiliary Variable (circle in stock-flow diagrams)
#'
#' Represents an auxiliary variable computed from stocks and other variables.
#' Depicted as circles in diagrams. Used to simplify flow rate expressions.
#'
#' @slot id Integer identifier
#' @slot name Character name
#' @slot expression Function computing the variable value
#'
#' @details
#' The expression function should have signature:
#' function(state, params, var_values)
#' where state is current stock values, params is parameters,
#' and var_values is a named list of already-computed variable values.
#'
#' @examples
#' \dontrun{
#' total_pop <- new("AuxiliaryVariable",
#'   id = 1L,
#'   name = "N",
#'   expression = function(state, params, var_values) {
#'     sum(state[c("S", "E", "I", "R")])
#'   }
#' )
#' }
#'
#' @export
setClass("AuxiliaryVariable",
  contains = "DiagramElement",
  slots = c(
    expression = "function"
  ),
  validity = function(object) {
    expr_args <- names(formals(object@expression))
    if (length(expr_args) < 3)
      return("expression must accept 3 arguments (state, params, var_values)")
    TRUE
  }
)

# SumVariable Class ==========================================================

#' Sum Variable (aggregates stocks)
#'
#' Represents a sum over multiple stocks. Special case of auxiliary variable
#' commonly used for computing totals (e.g., total population).
#'
#' @slot id Integer identifier
#' @slot name Character name
#' @slot stock_ids Integer vector of stock IDs to sum
#'
#' @examples
#' \dontrun{
#' total_pop <- new("SumVariable",
#'   id = 1L,
#'   name = "N",
#'   stock_ids = c(1L, 2L, 3L, 4L)  # S, E, I, R
#' )
#' }
#'
#' @export
setClass("SumVariable",
  contains = "DiagramElement",
  slots = c(
    stock_ids = "integer"
  ),
  validity = function(object) {
    if (length(object@stock_ids) == 0)
      return("stock_ids cannot be empty")
    if (any(is.na(object@stock_ids)))
      return("stock_ids cannot contain NA values")
    if (anyDuplicated(object@stock_ids) > 0)
      return("stock_ids cannot contain duplicates")
    TRUE
  }
)

# StockFlowDiagram Class =====================================================

#' Stock-Flow Diagram
#'
#' Represents a complete stock-flow diagram as a functor F: H → FinSet
#' from the primitive category H (with objects stock, flow, link and
#' morphisms u, d, s, t) to the category of finite sets.
#'
#' @slot stocks List of Stock objects
#' @slot flows List of Flow objects
#' @slot links List of Link objects
#' @slot variables List of AuxiliaryVariable objects
#' @slot sumvars List of SumVariable objects
#' @slot stock_ids Integer vector: F(stock) ⊆ ℕ
#' @slot flow_ids Integer vector: F(flow) ⊆ ℕ
#' @slot link_ids Integer vector: F(link) ⊆ ℕ
#' @slot u_map Integer vector: F(u): flow_ids → stock_ids ∪ {NA}
#' @slot d_map Integer vector: F(d): flow_ids → stock_ids ∪ {NA}
#' @slot s_map Integer vector: F(s): link_ids → stock_ids ∪ variable_ids
#' @slot t_map Integer vector: F(t): link_ids → flow_ids
#' @slot interface Integer vector of exposed stock IDs for composition
#' @slot metadata List of user-defined metadata
#'
#' @details
#' The diagram stores both rich objects (stocks, flows, etc.) and
#' the categorical functor representation (stock_ids, morphism maps).
#' The functor data enables categorical operations (composition, stratification)
#' while the rich objects provide user-friendly access.
#'
#' Morphisms:
#' - u_map: upstream morphism (flow → source stock)
#' - d_map: downstream morphism (flow → target stock)
#' - s_map: source morphism (link → stock/variable)
#' - t_map: target morphism (link → flow)
#'
#' @export
setClass("StockFlowDiagram",
  slots = c(
    # Rich object representation
    stocks = "list",
    flows = "list",
    links = "list",
    variables = "list",
    sumvars = "list",

    # Functor representation: objects
    stock_ids = "integer",
    flow_ids = "integer",
    link_ids = "integer",

    # Functor representation: morphisms
    u_map = "integer",  # F(u): flow → stock (upstream)
    d_map = "integer",  # F(d): flow → stock (downstream)
    s_map = "integer",  # F(s): link → stock/variable (source)
    t_map = "integer",  # F(t): link → flow (target)

    # Composition interface
    interface = "integer",

    # Metadata
    metadata = "list"
  ),
  prototype = list(
    stocks = list(),
    flows = list(),
    links = list(),
    variables = list(),
    sumvars = list(),
    stock_ids = integer(0),
    flow_ids = integer(0),
    link_ids = integer(0),
    u_map = integer(0),
    d_map = integer(0),
    s_map = integer(0),
    t_map = integer(0),
    interface = integer(0),
    metadata = list()
  ),
  validity = function(object) {
    # Check list contents
    if (length(object@stocks) > 0 && !all(sapply(object@stocks, inherits, "Stock")))
      return("stocks must contain only Stock objects")
    if (length(object@flows) > 0 && !all(sapply(object@flows, inherits, "Flow")))
      return("flows must contain only Flow objects")
    if (length(object@links) > 0 && !all(sapply(object@links, inherits, "Link")))
      return("links must contain only Link objects")
    if (length(object@variables) > 0 &&
        !all(sapply(object@variables, inherits, "AuxiliaryVariable")))
      return("variables must contain only AuxiliaryVariable objects")
    if (length(object@sumvars) > 0 &&
        !all(sapply(object@sumvars, inherits, "SumVariable")))
      return("sumvars must contain only SumVariable objects")

    # Check functor object consistency
    if (length(object@stock_ids) != length(object@stocks))
      return("stock_ids length must match stocks length")
    if (length(object@flow_ids) != length(object@flows))
      return("flow_ids length must match flows length")
    if (length(object@link_ids) != length(object@links))
      return("link_ids length must match links length")

    # Check morphism map lengths
    if (length(object@u_map) != length(object@flow_ids))
      return("u_map length must match flow_ids length")
    if (length(object@d_map) != length(object@flow_ids))
      return("d_map length must match flow_ids length")
    if (length(object@s_map) != length(object@link_ids))
      return("s_map length must match link_ids length")
    if (length(object@t_map) != length(object@link_ids))
      return("t_map length must match link_ids length")

    # Check morphism codomains
    valid_stock_ids <- c(NA_integer_, object@stock_ids)
    if (!all(object@u_map %in% valid_stock_ids))
      return("u_map values must be in stock_ids or NA")
    if (!all(object@d_map %in% valid_stock_ids))
      return("d_map values must be in stock_ids or NA")
    if (!all(object@t_map %in% object@flow_ids))
      return("t_map values must be in flow_ids")

    # Check interface
    if (!all(object@interface %in% object@stock_ids))
      return("interface stocks must be in stock_ids")

    TRUE
  }
)

# DecoratedCospan Class ======================================================

#' Decorated Cospan
#'
#' Represents a decorated cospan: L ← A → R where A is the apex diagram
#' and L, R are the feet (interfaces). Used for categorical composition.
#'
#' @slot apex StockFlowDiagram at the apex
#' @slot left_leg Integer vector mapping left foot to apex stocks
#' @slot right_leg Integer vector mapping right foot to apex stocks
#' @slot left_foot Integer vector of interface stock IDs (left side)
#' @slot right_foot Integer vector of interface stock IDs (right side)
#'
#' @details
#' A decorated cospan represents an "open" diagram with exposed interfaces.
#' The legs are morphisms (injective functions) from the feet to the apex.
#' Composition is achieved by computing pushouts.
#'
#' @export
setClass("DecoratedCospan",
  slots = c(
    apex = "StockFlowDiagram",
    left_leg = "integer",
    right_leg = "integer",
    left_foot = "integer",
    right_foot = "integer"
  ),
  validity = function(object) {
    # Check leg lengths match feet
    if (length(object@left_leg) != length(object@left_foot))
      return("left_leg length must match left_foot length")
    if (length(object@right_leg) != length(object@right_foot))
      return("right_leg length must match right_foot length")

    # Check legs map to apex stocks
    if (length(object@left_leg) > 0 &&
        !all(object@left_leg %in% object@apex@stock_ids))
      return("left_leg must map to apex stock_ids")
    if (length(object@right_leg) > 0 &&
        !all(object@right_leg %in% object@apex@stock_ids))
      return("right_leg must map to apex stock_ids")

    TRUE
  }
)

# OpenStockFlowDiagram Class =================================================

#' Open Stock-Flow Diagram
#'
#' A decorated cospan where the legs are injective (one-to-one).
#' This ensures the diagram can be composed with others via pushout.
#'
#' @slot apex StockFlowDiagram at the apex
#' @slot left_leg Integer vector (injective) mapping left foot to apex
#' @slot right_leg Integer vector (injective) mapping right foot to apex
#' @slot left_foot Integer vector of interface stock IDs
#' @slot right_foot Integer vector of interface stock IDs
#'
#' @export
setClass("OpenStockFlowDiagram",
  contains = "DecoratedCospan",
  validity = function(object) {
    # Check that legs are injective (no duplicates)
    if (length(object@left_leg) > 0 && anyDuplicated(object@left_leg) > 0)
      return("left_leg must be injective (no duplicates)")
    if (length(object@right_leg) > 0 && anyDuplicated(object@right_leg) > 0)
      return("right_leg must be injective (no duplicates)")

    TRUE
  }
)

# Show Methods ===============================================================

#' @export
setMethod("show", "Stock", function(object) {
  cat("Stock:", object@name, "\n")
  cat("  ID:", object@id, "\n")
  cat("  Initial value:", object@initial_value, "\n")
})

#' @export
setMethod("show", "Flow", function(object) {
  cat("Flow:", object@name, "\n")
  cat("  ID:", object@id, "\n")
  cat("  Source:", ifelse(is.na(object@source), "INFLOW", object@source), "\n")
  cat("  Target:", ifelse(is.na(object@target), "OUTFLOW", object@target), "\n")
})

#' @export
setMethod("show", "Link", function(object) {
  cat("Link:", object@name, "\n")
  cat("  From:", object@source_type, object@source_id, "\n")
  cat("  To: flow", object@target_flow, "\n")
})

#' @export
setMethod("show", "StockFlowDiagram", function(object) {
  cat("StockFlowDiagram\n")
  cat("  Stocks:", length(object@stocks), "\n")
  cat("  Flows:", length(object@flows), "\n")
  cat("  Links:", length(object@links), "\n")
  cat("  Variables:", length(object@variables), "\n")
  cat("  Sum variables:", length(object@sumvars), "\n")
  if (length(object@interface) > 0) {
    cat("  Interface:", paste(object@interface, collapse = ", "), "\n")
  }
})

#' @export
setMethod("show", "DecoratedCospan", function(object) {
  cat("DecoratedCospan\n")
  cat("  Left foot:", length(object@left_foot), "stocks\n")
  cat("  Right foot:", length(object@right_foot), "stocks\n")
  cat("  Apex:\n")
  show(object@apex)
})
