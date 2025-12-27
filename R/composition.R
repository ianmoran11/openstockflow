#' Composition via Decorated Cospans
#'
#' Functions for composing stock-flow diagrams using categorical pushout operations.
#' Enables building complex models by composing simpler component models through
#' shared interfaces.
#'
#' @name composition
NULL

# Conversion Functions =======================================================

#' Convert StockFlowDiagram to OpenStockFlowDiagram
#'
#' Creates an open diagram by specifying which stocks are exposed as interfaces.
#' Open diagrams can be composed via the compose() function.
#'
#' @param diagram StockFlowDiagram object
#' @param left_interface Character vector of stock names for left foot
#' @param right_interface Character vector of stock names for right foot
#' @return OpenStockFlowDiagram object
#'
#' @details
#' An open diagram is represented as a decorated cospan:
#' ```
#'      left_leg        right_leg
#' I -----------> Apex <----------- O
#' ```
#'
#' The left and right feet specify which stocks are exposed for composition.
#' Identity morphisms are created as legs (feet stocks map to themselves in apex).
#'
#' @examples
#' \dontrun{
#' sir <- stock_flow_diagram() %>%
#'   add_stock("S", initial = 999) %>%
#'   add_stock("I", initial = 1) %>%
#'   add_stock("R", initial = 0)
#'
#' # Expose I as right interface for composition
#' sir_open <- as_open_diagram(sir, right_interface = "I")
#' }
#'
#' @export
as_open_diagram <- function(diagram, left_interface = character(0),
                            right_interface = character(0)) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  # Resolve stock names to IDs
  left_ids <- integer(0)
  if (length(left_interface) > 0) {
    for (name in left_interface) {
      stock <- get_stock_by_name(diagram, name)
      if (is.null(stock)) {
        stop("Stock '", name, "' not found in diagram")
      }
      left_ids <- c(left_ids, stock@id)
    }
  }

  right_ids <- integer(0)
  if (length(right_interface) > 0) {
    for (name in right_interface) {
      stock <- get_stock_by_name(diagram, name)
      if (is.null(stock)) {
        stop("Stock '", name, "' not found in diagram")
      }
      right_ids <- c(right_ids, stock@id)
    }
  }

  # Create legs as identity morphisms (each foot stock maps to itself in apex)
  left_leg <- left_ids
  right_leg <- right_ids

  # Create OpenStockFlowDiagram
  new("OpenStockFlowDiagram",
    apex = diagram,
    left_foot = left_ids,
    right_foot = right_ids,
    left_leg = left_leg,
    right_leg = right_leg
  )
}

#' Close an OpenStockFlowDiagram
#'
#' Forgets the interface information and returns the underlying StockFlowDiagram.
#'
#' @param open_diagram OpenStockFlowDiagram object
#' @return StockFlowDiagram object (the apex)
#'
#' @examples
#' \dontrun{
#' open_sir <- as_open_diagram(sir, right_interface = "I")
#' closed_sir <- close_diagram(open_sir)  # Same as original sir
#' }
#'
#' @export
close_diagram <- function(open_diagram) {
  if (!inherits(open_diagram, "OpenStockFlowDiagram") &&
      !inherits(open_diagram, "DecoratedCospan")) {
    stop("open_diagram must be an OpenStockFlowDiagram or DecoratedCospan")
  }

  open_diagram@apex
}

# Pushout Computation ========================================================

#' Compute pushout of two diagrams (internal)
#'
#' Implements the categorical pushout operation for composing two stock-flow
#' diagrams. The interface stocks are identified, and the diagrams are merged.
#'
#' @param left_apex StockFlowDiagram (left diagram's apex)
#' @param right_apex StockFlowDiagram (right diagram's apex)
#' @param left_interface_ids Integer vector of stock IDs in left diagram
#' @param right_interface_ids Integer vector of stock IDs in right diagram
#' @return List with StockFlowDiagram (merged) and interface information
#' @keywords internal
compute_pushout <- function(left_apex, right_apex,
                           left_interface_ids, right_interface_ids) {
  # Validate interface compatibility
  if (length(left_interface_ids) != length(right_interface_ids)) {
    stop("Interface cardinalities must match: left has ", length(left_interface_ids),
         ", right has ", length(right_interface_ids))
  }

  # Step 1: Create ID mapping for right diagram
  # Offset right IDs to avoid conflicts
  max_left_id <- if (length(left_apex@stock_ids) > 0) {
    max(left_apex@stock_ids, left_apex@flow_ids, left_apex@link_ids, na.rm = TRUE)
  } else {
    0L
  }
  id_offset <- max_left_id + 1000L

  # Build ID mapping: right_id -> new_id_in_composed
  id_map <- integer(0)
  names_map <- character(0)

  # Interface stocks keep left diagram IDs
  for (i in seq_along(left_interface_ids)) {
    left_id <- left_interface_ids[i]
    right_id <- right_interface_ids[i]
    id_map[as.character(right_id)] <- left_id

    # Track names for later
    left_stock <- get_stock(left_apex, left_id)
    right_stock <- get_stock(right_apex, right_id)
    names_map[right_stock@name] <- left_stock@name
  }

  # Non-interface right stocks get new IDs
  for (stock in right_apex@stocks) {
    if (!(stock@id %in% right_interface_ids)) {
      new_id <- stock@id + id_offset
      id_map[as.character(stock@id)] <- new_id
    }
  }

  # Step 2: Merge stocks
  merged_stocks <- left_apex@stocks
  merged_stock_ids <- left_apex@stock_ids

  for (stock in right_apex@stocks) {
    if (!(stock@id %in% right_interface_ids)) {
      # Non-interface stock: add with new ID
      new_id <- id_map[as.character(stock@id)]
      new_stock <- new("Stock",
        id = new_id,
        name = stock@name,
        initial_value = stock@initial_value
      )
      merged_stocks <- c(merged_stocks, list(new_stock))
      merged_stock_ids <- c(merged_stock_ids, new_id)
    }
  }

  # Step 3: Merge flows
  # Track which flows might need rate merging
  flow_signatures <- list()  # Maps "source_id:target_id" to list of flows

  merged_flows <- list()
  merged_flow_ids <- integer(0)
  merged_u_map <- integer(0)
  merged_d_map <- integer(0)

  # Add left flows
  for (i in seq_along(left_apex@flows)) {
    flow <- left_apex@flows[[i]]
    merged_flows <- c(merged_flows, list(flow))
    merged_flow_ids <- c(merged_flow_ids, flow@id)
    merged_u_map <- c(merged_u_map, left_apex@u_map[i])
    merged_d_map <- c(merged_d_map, left_apex@d_map[i])

    # Track signature
    sig <- paste(left_apex@u_map[i], left_apex@d_map[i], sep = ":")
    if (is.null(flow_signatures[[sig]])) {
      flow_signatures[[sig]] <- list()
    }
    flow_signatures[[sig]] <- c(flow_signatures[[sig]], list(flow))
  }

  # Add right flows (with remapped IDs)
  for (i in seq_along(right_apex@flows)) {
    flow <- right_apex@flows[[i]]

    # Remap source and target via id_map
    new_source <- if (is.na(flow@source)) {
      NA_integer_
    } else {
      id_map[as.character(flow@source)]
    }

    new_target <- if (is.na(flow@target)) {
      NA_integer_
    } else {
      id_map[as.character(flow@target)]
    }

    # Check for equivalent flow (same source/target)
    sig <- paste(new_source, new_target, sep = ":")

    if (!is.null(flow_signatures[[sig]])) {
      # Equivalent flow exists - merge rates
      existing_flows <- flow_signatures[[sig]]

      # Find the flow in merged_flows to update
      for (j in seq_along(merged_flows)) {
        existing_flow <- merged_flows[[j]]
        if ((!is.na(existing_flow@source) && !is.na(new_source) && existing_flow@source == new_source ||
             is.na(existing_flow@source) && is.na(new_source)) &&
            (!is.na(existing_flow@target) && !is.na(new_target) && existing_flow@target == new_target ||
             is.na(existing_flow@target) && is.na(new_target))) {

          # Merge rates: create new rate function that sums
          original_rate1 <- existing_flow@rate_function
          original_rate2 <- flow@rate_function

          merged_rate <- function(inputs, params) {
            original_rate1(inputs, params) + original_rate2(inputs, params)
          }

          # Update the flow
          merged_flows[[j]]@rate_function <- merged_rate
          break
        }
      }
    } else {
      # New flow: add with new ID
      new_flow_id <- flow@id + id_offset
      new_flow <- new("Flow",
        id = new_flow_id,
        name = flow@name,
        rate_function = flow@rate_function,
        source = new_source,
        target = new_target
      )

      merged_flows <- c(merged_flows, list(new_flow))
      merged_flow_ids <- c(merged_flow_ids, new_flow_id)
      merged_u_map <- c(merged_u_map, new_source)
      merged_d_map <- c(merged_d_map, new_target)

      # Track signature
      if (is.null(flow_signatures[[sig]])) {
        flow_signatures[[sig]] <- list()
      }
      flow_signatures[[sig]] <- c(flow_signatures[[sig]], list(new_flow))
    }
  }

  # Step 4: Merge links
  merged_links <- list()
  merged_link_ids <- integer(0)
  merged_s_map <- integer(0)
  merged_t_map <- integer(0)

  # Add left links
  for (i in seq_along(left_apex@links)) {
    link <- left_apex@links[[i]]
    merged_links <- c(merged_links, list(link))
    merged_link_ids <- c(merged_link_ids, link@id)
    merged_s_map <- c(merged_s_map, left_apex@s_map[i])
    merged_t_map <- c(merged_t_map, left_apex@t_map[i])
  }

  # Add right links (with remapped IDs)
  for (i in seq_along(right_apex@links)) {
    link <- right_apex@links[[i]]

    # Remap source ID
    new_source_id <- if (link@source_type == "stock") {
      id_map[as.character(link@source_id)]
    } else {
      # Variable - keep original ID (variables not yet composed)
      link@source_id
    }

    # Remap target flow ID
    new_target_flow <- link@target_flow + id_offset

    # Check if target flow was merged (kept original ID)
    # For now, assume flow IDs are remapped
    # TODO: Handle merged flows properly

    new_link_id <- link@id + id_offset
    new_link <- new("Link",
      id = new_link_id,
      name = link@name,
      source_id = new_source_id,
      source_type = link@source_type,
      target_flow = new_target_flow
    )

    merged_links <- c(merged_links, list(new_link))
    merged_link_ids <- c(merged_link_ids, new_link_id)
    merged_s_map <- c(merged_s_map, new_source_id)
    merged_t_map <- c(merged_t_map, new_target_flow)
  }

  # Step 5: Create merged diagram
  merged_diagram <- new("StockFlowDiagram",
    stocks = merged_stocks,
    flows = merged_flows,
    links = merged_links,
    variables = list(),  # TODO: Merge variables
    sumvars = list(),    # TODO: Merge sum variables
    stock_ids = merged_stock_ids,
    flow_ids = merged_flow_ids,
    link_ids = merged_link_ids,
    u_map = merged_u_map,
    d_map = merged_d_map,
    s_map = merged_s_map,
    t_map = merged_t_map,
    interface = integer(0),
    metadata = list()
  )

  # Return merged diagram and interface info
  list(
    diagram = merged_diagram,
    left_interface = left_interface_ids,
    right_interface = id_map[as.character(right_interface_ids)],
    id_map = id_map  # Return id_map for remapping right diagram's right_leg
  )
}

# High-Level Composition API =================================================

#' Compose two stock-flow diagrams
#'
#' Composes two open stock-flow diagrams via categorical pushout.
#' The diagrams are glued along their shared interface, and flows connecting
#' to interface stocks are merged by summing their rates.
#'
#' @param left OpenStockFlowDiagram, StockFlowDiagram, or decorated cospan
#' @param right OpenStockFlowDiagram, StockFlowDiagram, or decorated cospan
#' @param interface Character vector of stock names to use as interface
#'   (optional if diagrams already have interfaces set)
#' @return OpenStockFlowDiagram with composition result
#'
#' @details
#' Composition follows the categorical pushout construction:
#' - The disjoint union of both diagrams is taken
#' - Stocks in the interface are identified (treated as the same stock)
#' - Flows connecting to interface stocks are preserved
#' - If multiple flows connect the same pair of stocks, their rates are summed
#'
#' The composition is associative: (A ∘ B) ∘ C = A ∘ (B ∘ C)
#'
#' @examples
#' \dontrun{
#' # Create two models
#' transmission <- stock_flow_diagram() %>%
#'   add_stock("S", initial = 999) %>%
#'   add_stock("I", initial = 1) %>%
#'   add_flow("infection", from = "S", to = "I", rate = ...)
#'
#' recovery <- stock_flow_diagram() %>%
#'   add_stock("I", initial = 1) %>%
#'   add_stock("R", initial = 0) %>%
#'   add_flow("recovery", from = "I", to = "R", rate = ...)
#'
#' # Compose via shared stock I
#' sir <- compose(transmission, recovery, interface = "I")
#' }
#'
#' @export
compose <- function(left, right, interface = NULL) {
  # Convert to OpenStockFlowDiagram if needed
  if (inherits(left, "StockFlowDiagram") && !inherits(left, "OpenStockFlowDiagram")) {
    if (is.null(interface)) {
      stop("interface must be specified when composing StockFlowDiagram objects")
    }
    left <- as_open_diagram(left, right_interface = interface)
  }

  if (inherits(right, "StockFlowDiagram") && !inherits(right, "OpenStockFlowDiagram")) {
    if (is.null(interface)) {
      stop("interface must be specified when composing StockFlowDiagram objects")
    }
    right <- as_open_diagram(right, left_interface = interface)
  }

  # Extract interface IDs
  left_interface_ids <- left@right_foot
  right_interface_ids <- right@left_foot

  # Validate compatibility
  if (!check_interface_compatibility(left, right)) {
    stop("Diagrams have incompatible interfaces. ",
         "Left has ", length(left_interface_ids), " stocks, ",
         "right has ", length(right_interface_ids), " stocks")
  }

  # Compute pushout
  result <- compute_pushout(left@apex, right@apex,
                           left_interface_ids, right_interface_ids)

  # Remap right diagram's right_leg through id_map
  remapped_right_leg <- integer(length(right@right_leg))
  for (i in seq_along(right@right_leg)) {
    stock_id <- right@right_leg[i]
    remapped_right_leg[i] <- result$id_map[as.character(stock_id)]
  }

  # Remap right diagram's right_foot through id_map
  remapped_right_foot <- integer(length(right@right_foot))
  for (i in seq_along(right@right_foot)) {
    stock_id <- right@right_foot[i]
    remapped_right_foot[i] <- result$id_map[as.character(stock_id)]
  }

  # Create composed open diagram
  # Left foot from left diagram, right foot from right diagram (remapped)
  composed <- new("OpenStockFlowDiagram",
    apex = result$diagram,
    left_foot = left@left_foot,
    right_foot = remapped_right_foot,
    left_leg = left@left_leg,
    right_leg = remapped_right_leg  # Remapped right diagram's right leg
  )

  composed
}

# Validation and Helper Functions ============================================

#' Extract interface from diagram
#'
#' Gets the interface stock IDs from a diagram.
#'
#' @param diagram StockFlowDiagram or OpenStockFlowDiagram
#' @return Integer vector of stock IDs
#' @keywords internal
extract_interface <- function(diagram) {
  if (inherits(diagram, "OpenStockFlowDiagram") ||
      inherits(diagram, "DecoratedCospan")) {
    # Use right foot as default interface
    return(diagram@right_foot)
  } else if (inherits(diagram, "StockFlowDiagram")) {
    return(diagram@interface)
  } else {
    return(integer(0))
  }
}
