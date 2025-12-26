# Tests for Algebraic API
# Tests stock(), flow(), %->%, %+%, finalize(), and automatic stock inclusion

test_that("stock() creates a stock specification", {
  s <- stock("S", initial = 100)

  expect_s3_class(s, "stock_spec")
  expect_equal(s$name, "S")
  expect_equal(s$initial, 100)
  expect_equal(s$type, "stock")
})

test_that("flow() creates a flow specification", {
  f <- flow("f", rate = function(inputs, params) params$k)

  expect_s3_class(f, "flow_spec")
  expect_equal(f$name, "f")
  expect_type(f$rate, "closure")
  expect_equal(f$type, "flow")
})

test_that("%->% connects stock to flow", {
  S <- stock("S", initial = 100)
  f <- flow("f", rate = function(inputs, params) params$k)

  conn <- S %->% f

  expect_s3_class(conn, "flow_connection")
  expect_s3_class(conn, "flow_spec")
  expect_equal(conn$from, "S")
  expect_equal(conn$from_spec$name, "S")  # Stock object stored
})

test_that("%->% completes flow connection", {
  S <- stock("S", initial = 100)
  I <- stock("I", initial = 1)
  f <- flow("f", rate = function(inputs, params) params$k)

  conn <- S %->% f %->% I

  expect_s3_class(conn, "flow_connection_complete")
  expect_equal(conn$from, "S")
  expect_equal(conn$to, "I")
  expect_equal(conn$from_spec$name, "S")  # Source stock stored
  expect_equal(conn$to_spec$name, "I")    # Target stock stored
})

test_that("%->% handles NULL for external flows", {
  X <- stock("X", initial = 10)
  inflow <- flow("inflow", rate = function(inputs, params) params$rate)
  outflow <- flow("outflow", rate = function(inputs, params) params$rate)

  # NULL -> flow (inflow)
  conn1 <- NULL %->% inflow
  expect_s3_class(conn1, "flow_connection")
  expect_null(conn1$from)
  expect_null(conn1$from_spec)

  # flow -> NULL (outflow)
  conn2 <- X %->% outflow %->% NULL
  expect_s3_class(conn2, "flow_connection_complete")
  expect_null(conn2$to)
  expect_null(conn2$to_spec)
})

# Automatic Stock Inclusion Tests ============================================

test_that("Stocks are automatically included from flow connections", {
  reset_id_counter()

  S <- stock("S", initial = 999)
  I <- stock("I", initial = 1)
  R <- stock("R", initial = 0)

  infection <- flow("infection",
    rate = function(inputs, params) params$beta * inputs$S * inputs$I / 1000
  )

  recovery <- flow("recovery",
    rate = function(inputs, params) params$gamma * inputs$I
  )

  # NEW: Stocks automatically included from connections (no explicit S %+% I %+% R)
  sir <- (S %->% infection %->% I) %+% (I %->% recovery %->% R)
  diagram <- finalize(sir, reset_ids = TRUE)

  expect_s4_class(diagram, "StockFlowDiagram")
  expect_equal(n_stocks(diagram), 3)
  expect_equal(n_flows(diagram), 2)
  expect_setequal(stock_names(diagram), c("S", "I", "R"))
  expect_setequal(flow_names(diagram), c("infection", "recovery"))
})

test_that("Backward compatibility: explicit stocks still work", {
  reset_id_counter()

  S <- stock("S", initial = 999)
  I <- stock("I", initial = 1)
  R <- stock("R", initial = 0)

  infection <- flow("infection",
    rate = function(inputs, params) params$beta
  )

  recovery <- flow("recovery",
    rate = function(inputs, params) params$gamma
  )

  # OLD: Explicit stock inclusion still works
  sir <- S %+% I %+% R %+%
    (S %->% infection %->% I) %+%
    (I %->% recovery %->% R)

  diagram <- finalize(sir, reset_ids = TRUE)

  expect_equal(n_stocks(diagram), 3)
  expect_equal(n_flows(diagram), 2)
})

test_that("Mixed explicit and implicit stocks work correctly", {
  reset_id_counter()

  A <- stock("A", initial = 100)
  B <- stock("B", initial = 50)
  C <- stock("C", initial = 25)

  f1 <- flow("f1", rate = function(inputs, params) params$k1)
  f2 <- flow("f2", rate = function(inputs, params) params$k2)

  # Only include A explicitly, B and C should be auto-included
  model <- A %+%
    (A %->% f1 %->% B) %+%
    (B %->% f2 %->% C)

  diagram <- finalize(model, reset_ids = TRUE)

  expect_equal(n_stocks(diagram), 3)
  expect_setequal(stock_names(diagram), c("A", "B", "C"))
})

test_that("Explicit stocks take precedence over implicit", {
  reset_id_counter()

  # Define S twice with different initial values
  S_explicit <- stock("S", initial = 999)
  S_implicit <- stock("S", initial = 500)

  I <- stock("I", initial = 1)

  f <- flow("f", rate = function(inputs, params) params$beta)

  # Explicit S should take precedence
  model <- S_explicit %+% (S_implicit %->% f %->% I)
  diagram <- finalize(model, reset_ids = TRUE)

  expect_equal(n_stocks(diagram), 2)

  # Check that explicit initial value (999) is used, not implicit (500)
  s_stock <- get_stock_by_name(diagram, "S")
  expect_equal(s_stock@initial_value, 999)
})

test_that("Isolated stocks require explicit inclusion", {
  reset_id_counter()

  A <- stock("A", initial = 100)
  B <- stock("B", initial = 50)
  C <- stock("C", initial = 25)  # Isolated stock with no flows

  f <- flow("f", rate = function(inputs, params) params$k)

  # A and B connected by flow, C isolated
  model <- C %+% (A %->% f %->% B)  # C must be explicit
  diagram <- finalize(model, reset_ids = TRUE)

  expect_equal(n_stocks(diagram), 3)
  expect_true("C" %in% stock_names(diagram))
})

test_that("External inflows and outflows work with automatic inclusion", {
  reset_id_counter()

  X <- stock("X", initial = 10)

  inflow <- flow("inflow",
    rate = function(inputs, params) params$rate_in
  )

  outflow <- flow("outflow",
    rate = function(inputs, params) params$rate_out * inputs$X
  )

  # X should be automatically included from connections
  model <- (NULL %->% inflow %->% X) %+% (X %->% outflow %->% NULL)
  diagram <- finalize(model, reset_ids = TRUE)

  expect_equal(n_stocks(diagram), 1)
  expect_equal(n_flows(diagram), 2)
  expect_equal(stock_names(diagram), "X")
})

test_that("Complex model with automatic inclusion", {
  reset_id_counter()

  # Create a more complex model
  S <- stock("S", initial = 999)
  E <- stock("E", initial = 0)
  I <- stock("I", initial = 1)
  R <- stock("R", initial = 0)

  infection <- flow("infection",
    rate = function(inputs, params) params$beta * inputs$S * inputs$I
  )

  progression <- flow("progression",
    rate = function(inputs, params) params$sigma * inputs$E
  )

  recovery <- flow("recovery",
    rate = function(inputs, params) params$gamma * inputs$I
  )

  # All stocks automatically included from flow connections
  seir <- (S %->% infection %->% E) %+%
          (E %->% progression %->% I) %+%
          (I %->% recovery %->% R)

  diagram <- finalize(seir, reset_ids = TRUE)

  expect_equal(n_stocks(diagram), 4)
  expect_equal(n_flows(diagram), 3)
  expect_setequal(stock_names(diagram), c("S", "E", "I", "R"))
})

test_that("Single connection works with automatic inclusion", {
  reset_id_counter()

  A <- stock("A", initial = 100)
  B <- stock("B", initial = 0)

  f <- flow("f", rate = function(inputs, params) params$k * inputs$A)

  # Single connection
  model <- A %->% f %->% B
  diagram <- finalize(model, reset_ids = TRUE)

  expect_equal(n_stocks(diagram), 2)
  expect_equal(n_flows(diagram), 1)
})

test_that("Automatic inclusion works with %+% operator", {
  reset_id_counter()

  X <- stock("X", initial = 10)
  Y <- stock("Y", initial = 20)
  Z <- stock("Z", initial = 30)

  f1 <- flow("f1", rate = function(inputs, params) params$k1)
  f2 <- flow("f2", rate = function(inputs, params) params$k2)

  # Build incrementally with %+%
  model <- (X %->% f1 %->% Y)
  model <- model %+% (Y %->% f2 %->% Z)

  diagram <- finalize(model, reset_ids = TRUE)

  expect_equal(n_stocks(diagram), 3)
  expect_equal(n_flows(diagram), 2)
})

# Edge Cases =================================================================

test_that("Error if undefined stock used in flow", {
  # This should error because UndefinedStock is not a stock_spec
  f <- flow("f", rate = function(inputs, params) params$k)

  expect_error(
    UndefinedStock %->% f,
    "object 'UndefinedStock' not found"
  )
})

test_that("Empty diagram_spec works", {
  reset_id_counter()

  model <- structure(list(specs = list()), class = "diagram_spec")
  diagram <- finalize(model, reset_ids = TRUE)

  expect_s4_class(diagram, "StockFlowDiagram")
  expect_equal(n_stocks(diagram), 0)
  expect_equal(n_flows(diagram), 0)
})
