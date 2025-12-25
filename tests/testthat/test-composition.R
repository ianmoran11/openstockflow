# Test Composition via Decorated Cospans

# Conversion Tests ===========================================================

test_that("Can convert StockFlowDiagram to OpenStockFlowDiagram", {
  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0)

  open_sir <- as_open_diagram(sir, right_interface = c("I"))

  expect_s4_class(open_sir, "OpenStockFlowDiagram")
  expect_s4_class(open_sir@apex, "StockFlowDiagram")
  expect_equal(length(open_sir@right_foot), 1)
  expect_equal(length(open_sir@left_foot), 0)
})

test_that("as_open_diagram validates stock names", {
  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999)

  expect_error(
    as_open_diagram(sir, right_interface = "X"),
    "Stock 'X' not found"
  )
})

test_that("Can close an OpenStockFlowDiagram", {
  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1)

  open_sir <- as_open_diagram(sir, right_interface = "I")
  closed <- close_diagram(open_sir)

  expect_s4_class(closed, "StockFlowDiagram")
  expect_equal(length(closed@stocks), 2)
  expect_equal(closed@stocks[[1]]@name, "S")
})

test_that("as_open_diagram creates identity legs", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("A", initial = 10) %>%
    add_stock("B", initial = 20)

  open_diag <- as_open_diagram(diagram,
                                left_interface = "A",
                                right_interface = "B")

  # Legs should map to the same stocks (identity morphisms)
  expect_equal(length(open_diag@left_leg), 1)
  expect_equal(length(open_diag@right_leg), 1)

  # Left leg maps A to itself
  stock_a <- get_stock_by_name(diagram, "A")
  expect_equal(open_diag@left_leg[1], stock_a@id)
})

# Simple Composition Tests ===================================================

test_that("Can compose two single-stock diagrams", {
  left_diag <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("X", initial = 10)

  right_diag <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("X", initial = 10)

  left_open <- as_open_diagram(left_diag, right_interface = "X")
  right_open <- as_open_diagram(right_diag, left_interface = "X")

  composed <- compose(left_open, right_open)

  expect_s4_class(composed, "OpenStockFlowDiagram")
  expect_s4_class(composed@apex, "StockFlowDiagram")

  # Should have only 1 stock (X is identified)
  expect_equal(length(composed@apex@stocks), 1)
})

test_that("Compose with disjoint stocks adds both", {
  left_diag <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("A", initial = 10) %>%
    add_stock("B", initial = 20)

  right_diag <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("B", initial = 20) %>%
    add_stock("C", initial = 30)

  left_open <- as_open_diagram(left_diag, right_interface = "B")
  right_open <- as_open_diagram(right_diag, left_interface = "B")

  composed <- compose(left_open, right_open)

  # Should have 3 stocks: A, B, C (B is shared)
  expect_equal(length(composed@apex@stocks), 3)

  stock_names <- sapply(composed@apex@stocks, function(s) s@name)
  expect_true("A" %in% stock_names)
  expect_true("B" %in% stock_names)
  expect_true("C" %in% stock_names)
})

test_that("Composition validates interface compatibility", {
  left_diag <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("A", initial = 10)

  right_diag <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("B", initial = 20) %>%
    add_stock("C", initial = 30)

  # Left has 1 interface stock, right has 2 - incompatible
  left_open <- as_open_diagram(left_diag, right_interface = "A")
  right_open <- as_open_diagram(right_diag, left_interface = c("B", "C"))

  expect_error(
    compose(left_open, right_open),
    "incompatible interfaces"
  )
})

test_that("Can compose with convenience interface parameter", {
  left_diag <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("X", initial = 10) %>%
    add_stock("Y", initial = 20)

  right_diag <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("Y", initial = 20) %>%
    add_stock("Z", initial = 30)

  # Use interface parameter for convenience
  composed <- compose(left_diag, right_diag, interface = "Y")

  expect_s4_class(composed, "OpenStockFlowDiagram")
  expect_equal(length(composed@apex@stocks), 3)
})

# SIR Model Composition ======================================================

test_that("Can decompose and recompose SIR model", {
  # Original SIR model
  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) {
        params$beta * inputs$S * inputs$I / 1000
      }
    ) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) {
        params$gamma * inputs$I
      }
    )

  # Split into transmission (S -> I) and recovery (I -> R)
  transmission <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) {
        params$beta * inputs$S * inputs$I / 1000
      }
    )

  recovery <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) {
        params$gamma * inputs$I
      }
    )

  # Compose via shared stock I
  sir_composed <- compose(transmission, recovery, interface = "I")

  # Check structure
  expect_equal(length(sir_composed@apex@stocks), 3)
  expect_equal(length(sir_composed@apex@flows), 2)

  # Check stock names
  stock_names <- sapply(sir_composed@apex@stocks, function(s) s@name)
  expect_true(all(c("S", "I", "R") %in% stock_names))
})

test_that("Composed SIR generates valid ODE", {
  transmission <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) {
        params$beta * inputs$S * inputs$I / 1000
      }
    )

  recovery <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_flow("recovery",
      from = "I",
      to = "R",
      rate = function(inputs, params) {
        params$gamma * inputs$I
      }
    )

  sir_composed <- compose(transmission, recovery, interface = "I")
  closed_sir <- close_diagram(sir_composed)

  # Should be able to generate ODE
  ode_func <- generate_ode(closed_sir)
  expect_type(ode_func, "closure")

  # Should evaluate without error
  state <- initial_state(closed_sir)
  params <- c(beta = 0.5, gamma = 0.1)

  deriv <- ode_func(0, state, params)
  expect_type(deriv, "list")
  expect_length(deriv[[1]], 3)
})

# Flow Rate Merging Tests ====================================================

test_that("Flow rates are summed when flows merge", {
  # Two diagrams with flows INTO the same stock
  left_diag <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("Source1", initial = 100) %>%
    add_stock("Target", initial = 0) %>%
    add_flow("flow1",
      from = "Source1",
      to = "Target",
      rate = function(inputs, params) {
        params$rate1
      }
    )

  right_diag <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("Source2", initial = 100) %>%
    add_stock("Target", initial = 0) %>%
    add_flow("flow2",
      from = "Source2",
      to = "Target",
      rate = function(inputs, params) {
        params$rate2
      }
    )

  left_open <- as_open_diagram(left_diag, right_interface = "Target")
  right_open <- as_open_diagram(right_diag, left_interface = "Target")

  composed <- compose(left_open, right_open)
  closed <- close_diagram(composed)

  # Generate ODE and check that rates sum
  ode_func <- generate_ode(closed)
  state <- initial_state(closed)
  params <- c(rate1 = 5, rate2 = 3)

  deriv <- ode_func(0, state, params)

  # dTarget/dt should be rate1 + rate2 = 8
  target_idx <- which(sapply(closed@stocks, function(s) s@name == "Target"))
  expect_equal(as.numeric(deriv[[1]][target_idx]), 8)
})

test_that("Merged flow rates work with complex functions", {
  # Flows with more complex rate functions
  left_diag <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("A", initial = 100) %>%
    add_stock("B", initial = 50) %>%
    add_flow("flow_A_to_B",
      from = "A",
      to = "B",
      rate = function(inputs, params) {
        params$k1 * inputs$A
      }
    )

  right_diag <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("B", initial = 50) %>%
    add_stock("C", initial = 10) %>%
    add_flow("flow_C_to_B",
      from = "C",
      to = "B",
      rate = function(inputs, params) {
        params$k2 * inputs$C
      }
    )

  composed <- compose(left_diag, right_diag, interface = "B")
  closed <- close_diagram(composed)

  # Evaluate ODE
  ode_func <- generate_ode(closed)
  state <- initial_state(closed)
  params <- c(k1 = 0.1, k2 = 0.2)

  deriv <- ode_func(0, state, params)

  # dB/dt should include contributions from both flows
  # Inflow from A: k1 * A = 0.1 * 100 = 10
  # Inflow from C: k2 * C = 0.2 * 10 = 2
  # Total: 12
  b_idx <- which(names(state) == "B")
  expect_equal(as.numeric(deriv[[1]][b_idx]), 12)
})

# Associativity Tests ========================================================

test_that("Composition is associative (structure)", {
  # Create three simple diagrams
  a <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("X", initial = 10) %>%
    add_stock("Y", initial = 20)

  b <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("Y", initial = 20) %>%
    add_stock("Z", initial = 30)

  c <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("Z", initial = 30) %>%
    add_stock("W", initial = 40)

  # Compute (A ∘ B) ∘ C
  ab <- compose(a, b, interface = "Y")
  abc_left <- compose(close_diagram(ab), c, interface = "Z")

  # Compute A ∘ (B ∘ C)
  bc <- compose(b, c, interface = "Z")
  abc_right <- compose(a, close_diagram(bc), interface = "Y")

  # Both should have same number of stocks
  expect_equal(
    length(abc_left@apex@stocks),
    length(abc_right@apex@stocks)
  )

  # Both should have all stocks X, Y, Z, W
  left_names <- sapply(abc_left@apex@stocks, function(s) s@name)
  right_names <- sapply(abc_right@apex@stocks, function(s) s@name)

  expect_setequal(left_names, c("X", "Y", "Z", "W"))
  expect_setequal(right_names, c("X", "Y", "Z", "W"))
})

test_that("Composition is associative (semantics)", {
  # Three diagrams with flows
  a <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("X", initial = 100) %>%
    add_stock("Y", initial = 0) %>%
    add_flow("f1", from = "X", to = "Y",
             rate = function(inputs, params) params$k1 * inputs$X)

  b <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("Y", initial = 0) %>%
    add_stock("Z", initial = 0) %>%
    add_flow("f2", from = "Y", to = "Z",
             rate = function(inputs, params) params$k2 * inputs$Y)

  c <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("Z", initial = 0) %>%
    add_stock("W", initial = 0) %>%
    add_flow("f3", from = "Z", to = "W",
             rate = function(inputs, params) params$k3 * inputs$Z)

  # (A ∘ B) ∘ C
  ab <- compose(a, b, interface = "Y")
  abc_left <- compose(close_diagram(ab), c, interface = "Z")
  left_diagram <- close_diagram(abc_left)

  # A ∘ (B ∘ C)
  bc <- compose(b, c, interface = "Z")
  abc_right <- compose(a, close_diagram(bc), interface = "Y")
  right_diagram <- close_diagram(abc_right)

  # Both should generate same ODE (up to stock ordering)
  ode_left <- generate_ode(left_diagram)
  ode_right <- generate_ode(right_diagram)

  state_left <- initial_state(left_diagram)
  state_right <- initial_state(right_diagram)
  params <- c(k1 = 0.1, k2 = 0.2, k3 = 0.3)

  deriv_left <- ode_left(0, state_left, params)
  deriv_right <- ode_right(0, state_right, params)

  # Should have same stocks (though possibly different order/IDs)
  expect_setequal(names(deriv_left[[1]]), names(deriv_right[[1]]))
})

# Edge Cases =================================================================

test_that("Can compose diagrams with no flows", {
  left <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("A", initial = 10) %>%
    add_stock("B", initial = 20)

  right <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("B", initial = 20) %>%
    add_stock("C", initial = 30)

  composed <- compose(left, right, interface = "B")

  expect_s4_class(composed, "OpenStockFlowDiagram")
  expect_equal(length(composed@apex@stocks), 3)
  expect_equal(length(composed@apex@flows), 0)
})

test_that("Composition preserves initial values from left diagram", {
  left <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("X", initial = 100)

  right <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("X", initial = 50)  # Different initial value

  left_open <- as_open_diagram(left, right_interface = "X")
  right_open <- as_open_diagram(right, left_interface = "X")

  composed <- compose(left_open, right_open)

  # Should preserve left diagram's initial value
  x_stock <- composed@apex@stocks[[1]]
  expect_equal(x_stock@initial_value, 100)
})

test_that("Can compose with external flows (inflows/outflows)", {
  left <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("X", initial = 10) %>%
    add_flow("inflow", from = NULL, to = "X",
             rate = function(inputs, params) params$rate_in)

  right <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("X", initial = 10) %>%
    add_flow("outflow", from = "X", to = NULL,
             rate = function(inputs, params) params$rate_out * inputs$X)

  composed <- compose(left, right, interface = "X")
  closed <- close_diagram(composed)

  # Should have both flows
  expect_equal(length(closed@flows), 2)

  # ODE should work
  ode_func <- generate_ode(closed)
  state <- initial_state(closed)
  params <- c(rate_in = 5, rate_out = 0.1)

  deriv <- ode_func(0, state, params)

  # dX/dt = rate_in - rate_out * X = 5 - 0.1*10 = 4
  expect_equal(as.numeric(deriv[[1]][1]), 4)
})
