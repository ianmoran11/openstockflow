# Test diagram construction and manipulation

# Basic Construction =========================================================

test_that("Empty diagram can be created", {
  diagram <- stock_flow_diagram()

  expect_s4_class(diagram, "StockFlowDiagram")
  expect_equal(n_stocks(diagram), 0)
  expect_equal(n_flows(diagram), 0)
  expect_equal(n_links(diagram), 0)
})

test_that("Can add stocks to diagram", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 1000) %>%
    add_stock("I", initial = 1)

  expect_equal(n_stocks(diagram), 2)
  expect_equal(stock_names(diagram), c("S", "I"))

  state <- initial_state(diagram)
  expect_equal(state["S"], c(S = 1000))
  expect_equal(state["I"], c(I = 1))
})

test_that("Can add flows to diagram", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 1000) %>%
    add_stock("I", initial = 1) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I
    )

  expect_equal(n_flows(diagram), 1)
  expect_equal(flow_names(diagram), "infection")

  # Check flow properties
  flow <- get_flow_by_name(diagram, "infection")
  expect_false(is.na(flow@source))
  expect_false(is.na(flow@target))
})

test_that("Can add inflow (NA source)", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 0) %>%
    add_flow("birth",
      from = NULL,
      to = "S",
      rate = function(inputs, params) params$birth_rate
    )

  flow <- get_flow_by_name(diagram, "birth")
  expect_true(is.na(flow@source))
  expect_false(is.na(flow@target))

  inflows <- get_inflows(diagram)
  expect_length(inflows, 1)
})

test_that("Can add outflow (NA target)", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("I", initial = 10) %>%
    add_flow("death",
      from = "I",
      to = NULL,
      rate = function(inputs, params) params$death_rate * inputs$I
    )

  flow <- get_flow_by_name(diagram, "death")
  expect_false(is.na(flow@source))
  expect_true(is.na(flow@target))

  outflows <- get_outflows(diagram)
  expect_length(outflows, 1)
})

test_that("Flows automatically create links", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 1000) %>%
    add_stock("I", initial = 1) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I
    )

  # Should have auto-created links from S and I to the flow
  expect_true(n_links(diagram) >= 2)

  flow <- get_flow_by_name(diagram, "infection")
  links <- get_links_to_flow(diagram, flow@id)
  expect_true(length(links) >= 2)
})

test_that("Can add auxiliary variables", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 1000) %>%
    add_stock("I", initial = 1) %>%
    add_variable("force_of_infection",
      expression = function(state, params, var_values) {
        params$beta * state["I"]
      }
    )

  var <- get_variable_by_name(diagram, "force_of_infection")
  expect_s4_class(var, "AuxiliaryVariable")
  expect_equal(var@name, "force_of_infection")
})

test_that("Can add sum variables", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 1000) %>%
    add_stock("E", initial = 0) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    add_sum_variable("N", stocks = c("S", "E", "I", "R"))

  var <- get_variable_by_name(diagram, "N")
  expect_s4_class(var, "SumVariable")
  expect_equal(var@name, "N")
  expect_length(var@stock_ids, 4)
})

test_that("Can manually add links from variables to flows", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 1000) %>%
    add_stock("I", initial = 1) %>%
    add_sum_variable("N", stocks = c("S", "I")) %>%
    add_flow("dummy",
      from = "S",
      to = "I",
      rate = function(inputs, params) 0
    ) %>%
    add_link(from = "N", to = "dummy")

  flow <- get_flow_by_name(diagram, "dummy")
  links <- get_links_to_flow(diagram, flow@id)

  # Should have links from S, I (auto-created) and N (manual)
  expect_true(length(links) >= 3)
})

# Error Handling =============================================================

test_that("add_stock catches duplicate names", {
  expect_error(
    stock_flow_diagram(reset_ids = TRUE) %>%
      add_stock("S", initial = 100) %>%
      add_stock("S", initial = 200),
    "already exists"
  )
})

test_that("add_flow catches nonexistent stocks", {
  expect_error(
    stock_flow_diagram(reset_ids = TRUE) %>%
      add_stock("S", initial = 100) %>%
      add_flow("invalid",
        from = "S",
        to = "NONEXISTENT",
        rate = function(inputs, params) 0
      ),
    "not found"
  )
})

test_that("add_flow requires at least one endpoint", {
  expect_error(
    stock_flow_diagram(reset_ids = TRUE) %>%
      add_flow("invalid",
        from = NULL,
        to = NULL,
        rate = function(inputs, params) 0
      ),
    "at least one endpoint"
  )
})

test_that("add_sum_variable catches nonexistent stocks", {
  expect_error(
    stock_flow_diagram(reset_ids = TRUE) %>%
      add_stock("S", initial = 100) %>%
      add_sum_variable("N", stocks = c("S", "NONEXISTENT")),
    "not found"
  )
})

test_that("add_link catches nonexistent source", {
  expect_error(
    stock_flow_diagram(reset_ids = TRUE) %>%
      add_stock("S", initial = 100) %>%
      add_flow("dummy",
        from = NULL,
        to = "S",
        rate = function(inputs, params) 1
      ) %>%
      add_link(from = "NONEXISTENT", to = "dummy"),
    "not found"
  )
})

test_that("add_link catches nonexistent target", {
  expect_error(
    stock_flow_diagram(reset_ids = TRUE) %>%
      add_stock("S", initial = 100) %>%
      add_link(from = "S", to = "NONEXISTENT"),
    "not found"
  )
})

# Query Functions ============================================================

test_that("get_flows_from_stock works", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 1000) %>%
    add_stock("I", initial = 1) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I
    )

  stock_S <- get_stock_by_name(diagram, "S")
  flows <- get_flows_from_stock(diagram, stock_S@id)
  expect_length(flows, 1)
})

test_that("get_flows_to_stock works", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 1000) %>%
    add_stock("I", initial = 1) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I
    )

  stock_I <- get_stock_by_name(diagram, "I")
  flows <- get_flows_to_stock(diagram, stock_I@id)
  expect_length(flows, 1)
})

test_that("get_stock_by_name returns NULL for nonexistent", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 100)

  stock <- get_stock_by_name(diagram, "NONEXISTENT")
  expect_null(stock)
})

# Interface ==================================================================

test_that("Can set interface", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 1000) %>%
    add_stock("I", initial = 1) %>%
    add_stock("R", initial = 0) %>%
    set_interface(c("S", "R"))

  expect_length(diagram@interface, 2)

  # Check that interface contains correct stock IDs
  stock_S <- get_stock_by_name(diagram, "S")
  stock_R <- get_stock_by_name(diagram, "R")
  expect_true(stock_S@id %in% diagram@interface)
  expect_true(stock_R@id %in% diagram@interface)
})

# Validation =================================================================

test_that("validate_diagram passes for valid diagrams", {
  diagram <- stock_flow_diagram(reset_ids = TRUE) %>%
    add_stock("S", initial = 1000) %>%
    add_stock("I", initial = 1) %>%
    add_flow("infection",
      from = "S",
      to = "I",
      rate = function(inputs, params) params$beta * inputs$S * inputs$I
    )

  expect_true(validate_diagram(diagram))
})

# Complete Example ===========================================================

test_that("Can build complete SEIR model", {
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

  # Add links from N to infection flow
  seir <- seir %>% add_link(from = "N", to = "infection")

  expect_equal(n_stocks(seir), 4)
  expect_equal(n_flows(seir), 3)
  expect_true(validate_diagram(seir))

  # Check initial state
  state <- initial_state(seir)
  expect_equal(sum(state), 1001)
})
