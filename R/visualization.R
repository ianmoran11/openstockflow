#' Stock-Flow Diagram Visualization
#'
#' Functions for visualizing stock-flow diagrams and their solutions.
#'
#' @name visualization
NULL

# Plot Method for StockFlowDiagram ==========================================

#' Plot a stock-flow diagram
#'
#' Creates a visual representation of the diagram structure.
#' If ggraph is available, uses ggraph for layout, otherwise falls back
#' to basic plotting.
#'
#' @param x StockFlowDiagram object
#' @param layout Character specifying layout algorithm. Options:
#'   - "auto" (default): choose automatically
#'   - "sugiyama": hierarchical layout (good for stock-flow diagrams)
#'   - "fr": Fruchterman-Reingold force-directed
#'   - "kk": Kamada-Kawai force-directed
#'   - "tree": tree layout
#'   - "circle": circular layout
#' @param node_size Numeric node size (default 8)
#' @param edge_width Numeric edge width (default 1)
#' @param node_labels Logical; show node labels (default TRUE)
#' @param edge_labels Logical; show edge labels (default FALSE)
#' @param ... Additional arguments passed to plotting functions
#' @return A ggplot object (if ggraph available) or invisible NULL
#'
#' @examples
#' \dontrun{
#' sir <- stock_flow_diagram() %>%
#'   add_stock("S", initial = 999) %>%
#'   add_stock("I", initial = 1) %>%
#'   add_stock("R", initial = 0) %>%
#'   add_flow("infection", from = "S", to = "I",
#'     rate = function(inputs, params) params$beta * inputs$S * inputs$I)
#'
#' plot(sir)
#' plot(sir, layout = "sugiyama")
#' }
#'
#' @export
plot.StockFlowDiagram <- function(x, layout = "auto",
                                   node_size = 8, edge_width = 1,
                                   node_labels = TRUE, edge_labels = FALSE,
                                   ...) {
  # Check if ggraph is available
  has_ggraph <- requireNamespace("ggraph", quietly = TRUE) &&
                requireNamespace("ggplot2", quietly = TRUE)

  if (has_ggraph) {
    plot_diagram_ggraph(x, layout, node_size, edge_width,
                       node_labels, edge_labels, ...)
  } else {
    plot_diagram_basic(x, ...)
  }
}

# ggraph Visualization =======================================================

#' Plot diagram using ggraph
#'
#' @keywords internal
plot_diagram_ggraph <- function(diagram, layout = "auto",
                               node_size = 8, edge_width = 1,
                               node_labels = TRUE, edge_labels = FALSE,
                               ...) {
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("ggraph package required for this function")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package required for this function")
  }

  # Convert to tbl_graph
  graph <- as_tbl_graph(diagram)

  # Choose layout
  if (layout == "auto") {
    # For stock-flow diagrams, sugiyama (hierarchical) often works well
    layout <- "sugiyama"
  }

  # Create base plot
  p <- ggraph::ggraph(graph, layout = layout)

  # Add edges
  p <- p + ggraph::geom_edge_link(
    arrow = ggplot2::arrow(length = ggplot2::unit(3, "mm")),
    end_cap = ggraph::circle(3, "mm"),
    width = edge_width,
    alpha = 0.6
  )

  # Add nodes with different shapes for stocks vs flows
  p <- p + ggraph::geom_node_point(
    ggplot2::aes(color = .data$type, shape = .data$type),
    size = node_size
  )

  # Add node labels
  if (node_labels) {
    p <- p + ggraph::geom_node_text(
      ggplot2::aes(label = .data$name),
      repel = TRUE,
      size = 3
    )
  }

  # Styling
  p <- p +
    ggplot2::scale_color_manual(
      values = c(stock = "#4285F4", flow = "#EA4335", link_bundle = "#FBBC04"),
      name = "Type"
    ) +
    ggplot2::scale_shape_manual(
      values = c(stock = 15, flow = 16, link_bundle = 17),  # square, circle, triangle
      name = "Type"
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold")
    ) +
    ggplot2::labs(
      title = "Stock-Flow Diagram",
      subtitle = paste(n_stocks(diagram), "stocks,", n_flows(diagram), "flows")
    )

  p
}

#' Plot diagram using ggraph with custom styling
#'
#' More control over appearance than basic plot method.
#'
#' @param diagram StockFlowDiagram object
#' @param layout Layout algorithm (default "sugiyama")
#' @param stock_color Color for stock nodes (default "#4285F4")
#' @param flow_color Color for flow nodes (default "#EA4335")
#' @param stock_shape Shape for stock nodes (default 15 = square)
#' @param flow_shape Shape for flow nodes (default 16 = circle)
#' @param ... Additional arguments
#' @return ggplot object
#'
#' @export
plot_diagram <- function(diagram, layout = "sugiyama",
                        stock_color = "#4285F4",
                        flow_color = "#EA4335",
                        stock_shape = 15,
                        flow_shape = 16,
                        ...) {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  plot(diagram, layout = layout, ...)
}

# Basic Plotting =============================================================

#' Plot diagram using base graphics
#'
#' @keywords internal
plot_diagram_basic <- function(diagram, ...) {
  # Simple text-based representation when ggraph not available
  cat("Stock-Flow Diagram\n")
  cat("==================\n\n")

  cat("Stocks (", n_stocks(diagram), "):\n", sep = "")
  for (stock in diagram@stocks) {
    cat("  [", stock@name, "] initial =", stock@initial_value, "\n")
  }

  cat("\nFlows (", n_flows(diagram), "):\n", sep = "")
  for (flow in diagram@flows) {
    source_name <- if (is.na(flow@source)) "INFLOW" else {
      get_stock(diagram, flow@source)@name
    }
    target_name <- if (is.na(flow@target)) "OUTFLOW" else {
      get_stock(diagram, flow@target)@name
    }
    cat("  ", source_name, " --[", flow@name, "]--> ", target_name, "\n", sep = "")
  }

  if (length(diagram@variables) > 0) {
    cat("\nAuxiliary Variables (", length(diagram@variables), "):\n", sep = "")
    for (var in diagram@variables) {
      cat("  (", var@name, ")\n", sep = "")
    }
  }

  if (length(diagram@sumvars) > 0) {
    cat("\nSum Variables (", length(diagram@sumvars), "):\n", sep = "")
    for (var in diagram@sumvars) {
      stock_names <- sapply(var@stock_ids, function(id) {
        get_stock(diagram, id)@name
      })
      cat("  ", var@name, " = ", paste(stock_names, collapse = " + "), "\n", sep = "")
    }
  }

  invisible(NULL)
}

# Graphviz DOT Export ========================================================

#' Export diagram to Graphviz DOT format
#'
#' Creates a DOT language representation that can be rendered with Graphviz.
#'
#' @param diagram StockFlowDiagram object
#' @param file Character file path to write (optional)
#' @param rankdir Character direction: "LR" (left-right) or "TB" (top-bottom)
#' @return Character string with DOT code
#'
#' @examples
#' \dontrun{
#' sir <- stock_flow_diagram() %>%
#'   add_stock("S", initial = 999) %>%
#'   add_stock("I", initial = 1) %>%
#'   add_flow("infection", from = "S", to = "I",
#'     rate = function(inputs, params) params$beta * inputs$S * inputs$I)
#'
#' # Get DOT string
#' dot_str <- to_graphviz(sir)
#' cat(dot_str)
#'
#' # Write to file
#' to_graphviz(sir, file = "sir.dot")
#' # Then: dot -Tpng sir.dot -o sir.png
#' }
#'
#' @export
to_graphviz <- function(diagram, file = NULL, rankdir = "LR") {
  if (!inherits(diagram, "StockFlowDiagram")) {
    stop("diagram must be a StockFlowDiagram")
  }

  lines <- character(0)

  # Header
  lines <- c(lines, "digraph StockFlow {")
  lines <- c(lines, paste0("  rankdir=", rankdir, ";"))
  lines <- c(lines, "  node [fontname=\"Helvetica\"];")
  lines <- c(lines, "")

  # Stocks (rectangles)
  lines <- c(lines, "  // Stocks")
  for (stock in diagram@stocks) {
    label <- paste0(stock@name, "\\n(", stock@initial_value, ")")
    lines <- c(lines, sprintf('  "%s" [shape=box, label="%s", style=filled, fillcolor="#E3F2FD"];',
                              stock@name, label))
  }
  lines <- c(lines, "")

  # Flows (circles)
  lines <- c(lines, "  // Flows")
  for (flow in diagram@flows) {
    lines <- c(lines, sprintf('  "%s" [shape=circle, label="%s", style=filled, fillcolor="#FFEBEE"];',
                              flow@name, flow@name))
  }
  lines <- c(lines, "")

  # Variables (ovals)
  if (length(diagram@variables) > 0 || length(diagram@sumvars) > 0) {
    lines <- c(lines, "  // Variables")
    for (var in diagram@variables) {
      lines <- c(lines, sprintf('  "%s" [shape=oval, label="%s", style=filled, fillcolor="#FFF9C4"];',
                                var@name, var@name))
    }
    for (var in diagram@sumvars) {
      stock_names <- sapply(var@stock_ids, function(id) get_stock(diagram, id)@name)
      label <- paste0(var@name, "\\n=", paste(stock_names, collapse = "+"))
      lines <- c(lines, sprintf('  "%s" [shape=diamond, label="%s", style=filled, fillcolor="#FFF9C4"];',
                                var@name, label))
    }
    lines <- c(lines, "")
  }

  # Edges: stock -> flow -> stock
  lines <- c(lines, "  // Flow connections")
  for (flow in diagram@flows) {
    # Source -> Flow
    if (!is.na(flow@source)) {
      source_stock <- get_stock(diagram, flow@source)
      lines <- c(lines, sprintf('  "%s" -> "%s" [penwidth=2];',
                                source_stock@name, flow@name))
    }

    # Flow -> Target
    if (!is.na(flow@target)) {
      target_stock <- get_stock(diagram, flow@target)
      lines <- c(lines, sprintf('  "%s" -> "%s" [penwidth=2];',
                                flow@name, target_stock@name))
    }
  }
  lines <- c(lines, "")

  # Links (thin arrows)
  if (n_links(diagram) > 0) {
    lines <- c(lines, "  // Links (dependencies)")
    for (link in diagram@links) {
      flow <- get_flow(diagram, link@target_flow)
      if (is.null(flow)) next

      if (link@source_type == "stock") {
        source <- get_stock(diagram, link@source_id)
        if (!is.null(source)) {
          lines <- c(lines, sprintf('  "%s" -> "%s" [style=dashed, color=gray];',
                                    source@name, flow@name))
        }
      } else if (link@source_type == "variable" || link@source_type == "sumvar") {
        # Find variable by ID
        var_name <- NULL
        for (v in c(diagram@variables, diagram@sumvars)) {
          if (v@id == link@source_id) {
            var_name <- v@name
            break
          }
        }
        if (!is.null(var_name)) {
          lines <- c(lines, sprintf('  "%s" -> "%s" [style=dashed, color=gray];',
                                    var_name, flow@name))
        }
      }
    }
  }

  lines <- c(lines, "}")

  # Combine into string
  dot_str <- paste(lines, collapse = "\n")

  # Write to file if specified
  if (!is.null(file)) {
    writeLines(dot_str, file)
    message("Wrote Graphviz DOT file to: ", file)
    message("Render with: dot -Tpng ", file, " -o output.png")
  }

  invisible(dot_str)
}

# Solution Plotting ==========================================================

#' Plot ODE solution time series
#'
#' Creates a time series plot of stock values over time.
#'
#' @param x stockflow_solution object (from solve_diagram)
#' @param stocks Character vector of stock names to plot (default: all)
#' @param style Character: "line" (default) or "area"
#' @param ... Additional arguments passed to plotting functions
#' @return ggplot object if ggplot2 available, otherwise base plot
#'
#' @examples
#' \dontrun{
#' result <- solve_diagram(sir, times = seq(0, 100, by = 1),
#'                        params = c(beta = 0.5, gamma = 0.1))
#' plot_solution(result)
#' plot_solution(result, stocks = c("S", "I"))
#' plot_solution(result, style = "area")
#' }
#'
#' @export
plot_solution <- function(x, stocks = NULL, style = "line", ...) {
  if (!inherits(x, "stockflow_solution")) {
    stop("x must be a stockflow_solution object from solve_diagram()")
  }

  # Get stock names
  all_stocks <- setdiff(names(x), "time")

  if (is.null(stocks)) {
    stocks <- all_stocks
  } else {
    # Check that requested stocks exist
    missing <- setdiff(stocks, all_stocks)
    if (length(missing) > 0) {
      stop("Stocks not found: ", paste(missing, collapse = ", "))
    }
  }

  # Check if ggplot2 is available
  has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

  if (has_ggplot2) {
    plot_solution_ggplot(x, stocks, style, ...)
  } else {
    plot_solution_base(x, stocks, ...)
  }
}

#' Plot solution using ggplot2
#'
#' @keywords internal
plot_solution_ggplot <- function(solution, stocks, style = "line", ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 required")
  }

  # Reshape data to long format
  data_long <- data.frame(
    time = rep(solution$time, length(stocks)),
    stock = rep(stocks, each = nrow(solution)),
    value = unlist(solution[, stocks])
  )

  # Create plot
  p <- ggplot2::ggplot(data_long, ggplot2::aes(x = .data$time, y = .data$value,
                                                color = .data$stock, fill = .data$stock))

  if (style == "area") {
    p <- p + ggplot2::geom_area(alpha = 0.6, position = "identity")
  } else {
    p <- p + ggplot2::geom_line(size = 1)
  }

  p <- p +
    ggplot2::labs(
      title = "Stock-Flow Model Solution",
      x = "Time",
      y = "Value",
      color = "Stock",
      fill = "Stock"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    )

  p
}

#' Plot solution using base graphics
#'
#' @keywords internal
plot_solution_base <- function(solution, stocks, ...) {
  # Set up colors
  colors <- rainbow(length(stocks))

  # Determine y-axis range
  all_values <- unlist(solution[, stocks])
  ylim <- range(all_values, na.rm = TRUE)

  # Create plot
  plot(solution$time, solution[[stocks[1]]],
       type = "l", col = colors[1], lwd = 2,
       xlab = "Time", ylab = "Value",
       main = "Stock-Flow Model Solution",
       ylim = ylim, ...)

  # Add additional stocks
  if (length(stocks) > 1) {
    for (i in 2:length(stocks)) {
      lines(solution$time, solution[[stocks[i]]],
            col = colors[i], lwd = 2)
    }
  }

  # Add legend
  legend("topright", legend = stocks, col = colors, lwd = 2, bty = "n")

  invisible(NULL)
}

# Additional Visualization Helpers ===========================================

#' Create a phase plot of two stocks
#'
#' Plots the trajectory in state space (stock1 vs stock2).
#'
#' @param solution stockflow_solution object
#' @param stock1 Character name of first stock (x-axis)
#' @param stock2 Character name of second stock (y-axis)
#' @param ... Additional arguments
#' @return ggplot object or base plot
#'
#' @examples
#' \dontrun{
#' result <- solve_diagram(sir, ...)
#' phase_plot(result, "S", "I")
#' }
#'
#' @export
phase_plot <- function(solution, stock1, stock2, ...) {
  if (!inherits(solution, "stockflow_solution")) {
    stop("solution must be a stockflow_solution object")
  }

  if (!stock1 %in% names(solution) || !stock2 %in% names(solution)) {
    stop("Stocks not found in solution")
  }

  has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

  if (has_ggplot2) {
    p <- ggplot2::ggplot(solution, ggplot2::aes(x = .data[[stock1]], y = .data[[stock2]])) +
      ggplot2::geom_path(color = "#4285F4", size = 1) +
      ggplot2::geom_point(data = solution[1, ], color = "green", size = 3) +
      ggplot2::geom_point(data = solution[nrow(solution), ], color = "red", size = 3) +
      ggplot2::labs(
        title = "Phase Plot",
        subtitle = paste("Green = start, Red = end"),
        x = stock1,
        y = stock2
      ) +
      ggplot2::theme_minimal()
    p
  } else {
    plot(solution[[stock1]], solution[[stock2]],
         type = "l", col = "#4285F4", lwd = 2,
         xlab = stock1, ylab = stock2,
         main = "Phase Plot", ...)
    points(solution[[stock1]][1], solution[[stock2]][1],
           col = "green", pch = 19, cex = 2)
    points(solution[[stock1]][nrow(solution)], solution[[stock2]][nrow(solution)],
           col = "red", pch = 19, cex = 2)
    invisible(NULL)
  }
}

#' Summary visualization of solution
#'
#' Creates a multi-panel plot showing time series and phase plots.
#'
#' @param solution stockflow_solution object
#' @param ... Additional arguments
#' @return Combined plot (if patchwork available) or list of plots
#'
#' @export
summary_plot <- function(solution, ...) {
  if (!inherits(solution, "stockflow_solution")) {
    stop("solution must be a stockflow_solution object")
  }

  stocks <- setdiff(names(solution), "time")

  # Time series plot
  p1 <- plot_solution(solution, stocks = stocks, ...)

  # If we have enough stocks, add phase plots
  if (length(stocks) >= 2) {
    p2 <- phase_plot(solution, stocks[1], stocks[2], ...)

    # Try to combine with patchwork if available
    if (requireNamespace("patchwork", quietly = TRUE)) {
      return(p1 / p2)
    } else {
      return(list(timeseries = p1, phase = p2))
    }
  }

  p1
}
