# Test ODE generation and deSolve integration

# Basic ODE Generation =======================================================

test_that("Can generate ODE function from diagram", {
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

  ode_func <- generate_ode(sir)

  expect_type(ode_func, "closure")
  expect_equal(length(formals(ode_func)), 3)  # time, state, params
})

test_that("ODE function has correct signature for deSolve", {
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

  ode_func <- generate_ode(sir)

  state <- initial_state(sir)
  params <- c(beta = 0.5, gamma = 0.1)

  # Call ODE function
  result <- ode_func(0, state, params)

  # Check return format
  expect_type(result, "list")
  expect_length(result, 1)
  expect_type(result[[1]], "double")
  expect_length(result[[1]], 3)  # S, I, R
  expect_named(result[[1]], c("S", "I", "R"))
})

test_that("Simple flow equations are correct", {
  # Simple tank draining: dS/dt = -outflow
  tank <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("Water", initial = 100) %>%
    add_flow("drain",
      from = "Water",
      to = NULL,
      rate = function(inputs, params) params$drain_rate * inputs$Water
    )

  ode_func <- generate_ode(tank)

  state <- c(Water = 100)
  params <- c(drain_rate = 0.1)

  deriv <- ode_func(0, state, params)[[1]]

  # dWater/dt should be -0.1 * 100 = -10
  expect_equal(deriv["Water"], c(Water = -10))
})

test_that("Inflow and outflow balance correctly", {
  # Tank with constant inflow and proportional outflow
  # At equilibrium: inflow_rate = outflow_rate * Water
  # So Water_eq = inflow_rate / outflow_rate
  tank <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("Water", initial = 50) %>%
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

  ode_func <- generate_ode(tank)

  # At equilibrium Water = 10 / 0.1 = 100
  state <- c(Water = 100)
  params <- c(inflow_rate = 10, outflow_rate = 0.1)

  deriv <- ode_func(0, state, params)[[1]]

  # Should be near zero at equilibrium
  expect_lt(abs(deriv["Water"]), 1e-10)
})

# Variable Evaluation ========================================================

test_that("Sum variables are computed correctly", {
  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_sum_variable("N", stocks = c("S", "I", "R")) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I / inputs$N
    ) %>%
    add_link(from = "N", to = "infection")

  ode_func <- generate_ode(sir)

  state <- c(S = 999, I = 1, R = 0)
  params <- c(beta = 0.5)

  # Should not error (N is computed correctly)
  expect_no_error(ode_func(0, state, params))

  deriv <- ode_func(0, state, params)[[1]]

  # dS/dt = -beta * S * I / N = -0.5 * 999 * 1 / 1000 = -0.4995
  expect_equal(deriv["S"], c(S = -0.4995), tolerance = 1e-6)
})

test_that("Auxiliary variables are evaluated in correct order", {
  # Create a diagram where var2 depends on var1
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("X", initial = 10) %>%
    add_variable("var1",
      expression = function(state, params, var_values) {
        state["X"] * 2
      }
    ) %>%
    add_variable("var2",
      expression = function(state, params, var_values) {
        var_values$var1 * 3
      }
    ) %>%
    add_flow("outflow",
      from = "X",
      to = NULL,
      rate = function(inputs, params) inputs$var2 * 0.1
    )

  # Manually add links for variables
  diagram <- diagram %>%
    add_link(from = "var2", to = "outflow")

  ode_func <- generate_ode(diagram)

  state <- c(X = 10)
  params <- numeric(0)

  # var1 = 10 * 2 = 20
  # var2 = 20 * 3 = 60
  # dX/dt = -60 * 0.1 = -6
  deriv <- ode_func(0, state, params)[[1]]

  expect_equal(deriv["X"], c(X = -6))
})

# deSolve Integration ========================================================

test_that("Can solve simple stock-flow model", {
  skip_if_not_installed("deSolve")

  # Exponential decay: dX/dt = -k*X, solution: X(t) = X0 * exp(-k*t)
  decay <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("X", initial = 100) %>%
    add_flow("decay",
      from = "X",
      to = NULL,
      rate = function(inputs, params) params$k * inputs$X
    )

  result <- solve_diagram(
    decay,
    times = seq(0, 10, by = 1),
    params = c(k = 0.1)
  )

  expect_s3_class(result, "stockflow_solution")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 11)  # 0, 1, ..., 10
  expect_true("X" %in% names(result))
  expect_true("time" %in% names(result))

  # Check analytical solution at t=10: X = 100 * exp(-0.1*10) = 36.788
  expect_equal(result$X[result$time == 10], 100 * exp(-1), tolerance = 1e-3)
})

test_that("SIR model produces expected dynamics", {
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

  # Basic sanity checks
  expect_equal(nrow(result), 101)

  # Total population should be conserved
  total <- result$S + result$I + result$R
  expect_true(all(abs(total - 1000) < 1e-6))

  # S should decrease monotonically
  expect_true(all(diff(result$S) <= 0))

  # R should increase monotonically
  expect_true(all(diff(result$R) >= 0))

  # I should first increase then decrease
  expect_true(max(result$I) > result$I[1])
  peak_idx <- which.max(result$I)
  expect_true(peak_idx > 1 && peak_idx < nrow(result))
})

test_that("SEIR model produces expected dynamics", {
  skip_if_not_installed("deSolve")

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
    )

  seir <- seir %>% add_link(from = "N", to = "infection")

  result <- solve_diagram(
    seir,
    times = seq(0, 200, by = 1),
    params = c(beta = 0.5, sigma = 0.2, gamma = 0.1)
  )

  # Population conservation
  total <- result$S + result$E + result$I + result$R
  expect_true(all(abs(total - 1001) < 1e-6))

  # S decreases
  expect_true(result$S[nrow(result)] < result$S[1])

  # R increases
  expect_true(result$R[nrow(result)] > result$R[1])

  # E and I have peaks
  expect_true(max(result$E) > result$E[1])
  expect_true(max(result$I) > result$I[1])
})

# Helper Functions ===========================================================

test_that("extract_parameters works", {
  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I
    ) %>%
    add_flow("recovery",
      from = "I",
      to = NULL,
      rate = function(inputs, params) params$gamma * inputs$I
    )

  params <- extract_parameters(sir)

  expect_true("beta" %in% params)
  expect_true("gamma" %in% params)
})

test_that("check_ode_wellposed catches missing parameters", {
  sir <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 999) %>%
    add_stock("I", initial = 1) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I
    )

  expect_error(
    check_ode_wellposed(sir, c()),  # No parameters
    "Missing parameters"
  )

  expect_error(
    check_ode_wellposed(sir, c(wrong_param = 1)),
    "Missing parameters"
  )

  # Should not error with correct parameters
  expect_true(check_ode_wellposed(sir, c(beta = 0.5)))
})

test_that("check_ode_wellposed catches non-finite initial values", {
  # Stock class validation catches Inf during construction
  expect_error(
    stock_flow_diagram(reset_ids = TRUE) %>%
      add_stock("X", initial = Inf),
    "initial_value must be finite"
  )
})
