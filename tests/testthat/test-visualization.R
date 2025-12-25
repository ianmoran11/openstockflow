# Test visualization functions

# Basic Plot Method ==========================================================

test_that("plot method exists for StockFlowDiagram", {
  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I / 1000
    ) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) params$gamma * inputs$I
    )

  # Should not error
  expect_no_error(plot(sir))
})

test_that("plot method handles empty diagrams", {
  empty <- stock_flow_diagram(reset_ids = TRUE)
  expect_no_error(plot(empty))
})

test_that("plot method handles diagrams with only stocks", {
  stocks_only <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("A", initial = 10) %>%
    add_stock("B", initial = 20)

  expect_no_error(plot(stocks_only))
})

test_that("plot method handles inflows and outflows", {
  tank <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("Water", initial = 100) %>%
    add_flow("fill",
      from = NULL,
      to = "Water",
      rate = function(inputs, params) params$inflow_rate
    ) %>%
    add_flow("drain",
      from = "Water",
      to = NULL,
      rate = function(inputs, params) params$outflow_rate * inputs$Water
    )

  expect_no_error(plot(tank))
})

# Graphviz DOT Export ========================================================

test_that("to_graphviz generates valid DOT format", {
  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I
    ) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) params$gamma * inputs$I
    )

  dot_str <- to_graphviz(sir)

  expect_type(dot_str, "character")
  expect_match(dot_str, "digraph StockFlow")
  expect_match(dot_str, "rankdir=LR")

  # Check stocks are defined as boxes
  expect_match(dot_str, '"S".*shape=box')
  expect_match(dot_str, '"I".*shape=box')
  expect_match(dot_str, '"R".*shape=box')

  # Check flows are defined as circles
  expect_match(dot_str, '"infection".*shape=circle')
  expect_match(dot_str, '"recovery".*shape=circle')

  # Check connections exist
  expect_match(dot_str, '"S" -> "infection"')
  expect_match(dot_str, '"infection" -> "I"')
  expect_match(dot_str, '"I" -> "recovery"')
  expect_match(dot_str, '"recovery" -> "R"')
})

test_that("to_graphviz respects rankdir parameter", {
  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1)

  dot_lr <- to_graphviz(sir, rankdir = "LR")
  dot_tb <- to_graphviz(sir, rankdir = "TB")

  expect_match(dot_lr, "rankdir=LR")
  expect_match(dot_tb, "rankdir=TB")
})

test_that("to_graphviz can write to file", {
  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1)

  tmp_file <- tempfile(fileext = ".dot")
  to_graphviz(sir, file = tmp_file)

  expect_true(file.exists(tmp_file))

  content <- readLines(tmp_file)
  expect_match(paste(content, collapse = "\n"), "digraph StockFlow")

  unlink(tmp_file)
})

test_that("to_graphviz handles inflows and outflows", {
  tank <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("Water", initial = 100) %>%
    add_flow("fill",
      from = NULL,
      to = "Water",
      rate = function(inputs, params) params$inflow_rate
    ) %>%
    add_flow("drain",
      from = "Water",
      to = NULL,
      rate = function(inputs, params) params$outflow_rate * inputs$Water
    )

  dot_str <- to_graphviz(tank)

  # Should include both flows
  expect_match(dot_str, '"fill"')
  expect_match(dot_str, '"drain"')

  # Inflow: connection from fill to Water
  expect_match(dot_str, '"fill" -> "Water"')

  # Outflow: connection from Water to drain
  expect_match(dot_str, '"Water" -> "drain"')
})

test_that("to_graphviz handles links from variables", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_sum_variable("N", stocks = c("S", "I")) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I / inputs$N
    ) %>%
    add_link(from = "N", to = "infection")

  dot_str <- to_graphviz(diagram)

  # Sum variable should be defined as diamond
  expect_match(dot_str, '"N".*shape=diamond')

  # Link from N to infection (dashed)
  expect_match(dot_str, '"N" -> "infection".*style=dashed')
})

# Solution Plotting ==========================================================

test_that("plot_solution works with stockflow_solution", {
  skip_if_not_installed("deSolve")

  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I / 1000
    ) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) params$gamma * inputs$I
    )

  result <- solve_diagram(
    sir,
    times = seq(0, 50, by = 1),
    params = c(beta = 0.5, gamma = 0.1)
  )

  # Should not error (will use base or ggplot depending on availability)
  expect_no_error(plot_solution(result))
})

test_that("plot_solution can select specific stocks", {
  skip_if_not_installed("deSolve")

  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I / 1000
    ) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) params$gamma * inputs$I
    )

  result <- solve_diagram(
    sir,
    times = seq(0, 50, by = 1),
    params = c(beta = 0.5, gamma = 0.1)
  )

  # Should not error when plotting only I
  expect_no_error(plot_solution(result, stocks = "I"))

  # Should not error when plotting S and I
  expect_no_error(plot_solution(result, stocks = c("S", "I")))
})

test_that("plot_solution handles area style", {
  skip_if_not_installed("deSolve")

  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I / 1000
    ) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) params$gamma * inputs$I
    )

  result <- solve_diagram(
    sir,
    times = seq(0, 50, by = 1),
    params = c(beta = 0.5, gamma = 0.1)
  )

  expect_no_error(plot_solution(result, style = "area"))
})

# Phase Plot =================================================================

test_that("phase_plot works with stockflow_solution", {
  skip_if_not_installed("deSolve")

  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I / 1000
    ) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) params$gamma * inputs$I
    )

  result <- solve_diagram(
    sir,
    times = seq(0, 100, by = 1),
    params = c(beta = 0.5, gamma = 0.1)
  )

  expect_no_error(phase_plot(result, "S", "I"))
})

test_that("phase_plot validates stock names", {
  skip_if_not_installed("deSolve")

  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I / 1000
    ) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) params$gamma * inputs$I
    )

  result <- solve_diagram(
    sir,
    times = seq(0, 50, by = 1),
    params = c(beta = 0.5, gamma = 0.1)
  )

  expect_error(phase_plot(result, "S", "X"), "not found in solution")
  expect_error(phase_plot(result, "Y", "I"), "not found in solution")
})

# Summary Plot ===============================================================

test_that("summary_plot works with stockflow_solution", {
  skip_if_not_installed("deSolve")

  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I / 1000
    ) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) params$gamma * inputs$I
    )

  result <- solve_diagram(
    sir,
    times = seq(0, 100, by = 1),
    params = c(beta = 0.5, gamma = 0.1)
  )

  expect_no_error(summary_plot(result))
})

test_that("summary_plot can select phase plot stocks", {
  skip_if_not_installed("deSolve")

  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I / 1000
    ) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) params$gamma * inputs$I
    )

  result <- solve_diagram(
    sir,
    times = seq(0, 100, by = 1),
    params = c(beta = 0.5, gamma = 0.1)
  )

  expect_no_error(summary_plot(result, phase_stocks = c("I", "R")))
})

# Edge Cases =================================================================

test_that("visualization handles single stock diagram", {
  single <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("X", initial = 10)

  expect_no_error(plot(single))
  expect_no_error(to_graphviz(single))
})

test_that("visualization handles complex SEIR model", {
  seir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 1000) %>%
    add_stock("E", initial = 0) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_sum_variable("N", stocks = c("S", "E", "I", "R")) %>%
    add_flow("infection",
      from = "S",
      to = "E",
      rate = function(inputs, params) {
        params$beta * inputs$S * inputs$I / inputs$N
      }
    ) %>%
    add_flow("progression",
      from = "E",
      to = "I",
      rate = function(inputs, params) params$sigma * inputs$E
    ) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) params$gamma * inputs$I
    ) %>%
    add_link(from = "N", to = "infection")

  expect_no_error(plot(seir))

  dot_str <- to_graphviz(seir)
  expect_match(dot_str, '"N"')
  expect_match(dot_str, '"infection"')
  expect_match(dot_str, '"N" -> "infection"')
})
