# Test S4 class definitions

# Stock Class Tests ==========================================================

test_that("Stock class can be created with valid parameters", {
  stock <- new("Stock",
    id = 1L,
    name = "S",
    initial_value = 1000
  )

  expect_s4_class(stock, "Stock")
  expect_equal(stock@id, 1L)
  expect_equal(stock@name, "S")
  expect_equal(stock@initial_value, 1000)
})

test_that("Stock class validation catches invalid initial_value", {
  expect_error(
    new("Stock", id = 1L, name = "S", initial_value = c(1, 2)),
    "initial_value must be a single numeric value"
  )

  expect_error(
    new("Stock", id = 1L, name = "S", initial_value = NA_real_),
    "initial_value cannot be NA"
  )

  expect_error(
    new("Stock", id = 1L, name = "S", initial_value = Inf),
    "initial_value must be finite"
  )
})

test_that("Stock class validation catches invalid id/name", {
  expect_error(
    new("Stock", id = c(1L, 2L), name = "S", initial_value = 100),
    "id must be a single integer"
  )

  expect_error(
    new("Stock", id = 1L, name = "", initial_value = 100),
    "name cannot be empty"
  )
})

# Flow Class Tests ===========================================================

test_that("Flow class can be created with valid parameters", {
  flow <- new("Flow",
    id = 1L,
    name = "infection",
    rate_function = function(inputs, params) {
      params["beta"] * inputs$S * inputs$I
    },
    source = 1L,
    target = 2L
  )

  expect_s4_class(flow, "Flow")
  expect_equal(flow@id, 1L)
  expect_equal(flow@name, "infection")
  expect_equal(flow@source, 1L)
  expect_equal(flow@target, 2L)
  expect_type(flow@rate_function, "closure")
})

test_that("Flow class allows inflows (NA source)", {
  inflow <- new("Flow",
    id = 1L,
    name = "birth",
    rate_function = function(inputs, params) params["birth_rate"],
    source = NA_integer_,
    target = 1L
  )

  expect_s4_class(inflow, "Flow")
  expect_true(is.na(inflow@source))
  expect_equal(inflow@target, 1L)
})

test_that("Flow class allows outflows (NA target)", {
  outflow <- new("Flow",
    id = 1L,
    name = "death",
    rate_function = function(inputs, params) params["death_rate"] * inputs$I,
    source = 1L,
    target = NA_integer_
  )

  expect_s4_class(outflow, "Flow")
  expect_equal(outflow@source, 1L)
  expect_true(is.na(outflow@target))
})

test_that("Flow class validation catches missing endpoints", {
  expect_error(
    new("Flow",
      id = 1L,
      name = "invalid",
      rate_function = function(inputs, params) 0,
      source = NA_integer_,
      target = NA_integer_
    ),
    "Flow must have at least one endpoint"
  )
})

test_that("Flow class validation checks rate_function signature", {
  expect_error(
    new("Flow",
      id = 1L,
      name = "invalid",
      rate_function = function(x) x,  # Only 1 argument
      source = 1L,
      target = 2L
    ),
    "rate_function must accept at least 2 arguments"
  )
})

# Link Class Tests ===========================================================

test_that("Link class can be created with valid parameters", {
  link <- new("Link",
    id = 1L,
    name = "S_to_infection",
    source_id = 1L,
    source_type = "stock",
    target_flow = 1L
  )

  expect_s4_class(link, "Link")
  expect_equal(link@source_id, 1L)
  expect_equal(link@source_type, "stock")
  expect_equal(link@target_flow, 1L)
})

test_that("Link class accepts all valid source_types", {
  for (type in c("stock", "variable", "sumvar")) {
    link <- new("Link",
      id = 1L,
      name = "test_link",
      source_id = 1L,
      source_type = type,
      target_flow = 1L
    )
    expect_equal(link@source_type, type)
  }
})

test_that("Link class validation catches invalid source_type", {
  expect_error(
    new("Link",
      id = 1L,
      name = "invalid",
      source_id = 1L,
      source_type = "invalid_type",
      target_flow = 1L
    ),
    "source_type must be one of"
  )
})

# AuxiliaryVariable Class Tests ==============================================

test_that("AuxiliaryVariable class can be created", {
  var <- new("AuxiliaryVariable",
    id = 1L,
    name = "force_of_infection",
    expression = function(state, params, var_values) {
      params["beta"] * state["I"] / sum(state)
    }
  )

  expect_s4_class(var, "AuxiliaryVariable")
  expect_equal(var@name, "force_of_infection")
})

test_that("AuxiliaryVariable validation checks expression signature", {
  expect_error(
    new("AuxiliaryVariable",
      id = 1L,
      name = "invalid",
      expression = function(x, y) x + y  # Only 2 arguments
    ),
    "expression must accept 3 arguments"
  )
})

# SumVariable Class Tests ====================================================

test_that("SumVariable class can be created", {
  sumvar <- new("SumVariable",
    id = 1L,
    name = "N",
    stock_ids = c(1L, 2L, 3L, 4L)
  )

  expect_s4_class(sumvar, "SumVariable")
  expect_equal(sumvar@name, "N")
  expect_equal(sumvar@stock_ids, c(1L, 2L, 3L, 4L))
})

test_that("SumVariable validation catches empty stock_ids", {
  expect_error(
    new("SumVariable", id = 1L, name = "N", stock_ids = integer(0)),
    "stock_ids cannot be empty"
  )
})

test_that("SumVariable validation catches duplicates", {
  expect_error(
    new("SumVariable", id = 1L, name = "N", stock_ids = c(1L, 2L, 1L)),
    "stock_ids cannot contain duplicates"
  )
})

# StockFlowDiagram Class Tests ===============================================

test_that("Empty StockFlowDiagram can be created", {
  diagram <- new("StockFlowDiagram")

  expect_s4_class(diagram, "StockFlowDiagram")
  expect_length(diagram@stocks, 0)
  expect_length(diagram@flows, 0)
  expect_length(diagram@links, 0)
})

test_that("StockFlowDiagram with stocks can be created", {
  stock1 <- new("Stock", id = 1L, name = "S", initial_value = 1000)
  stock2 <- new("Stock", id = 2L, name = "I", initial_value = 1)

  diagram <- new("StockFlowDiagram",
    stocks = list(stock1, stock2),
    stock_ids = c(1L, 2L)
  )

  expect_s4_class(diagram, "StockFlowDiagram")
  expect_length(diagram@stocks, 2)
  expect_equal(diagram@stock_ids, c(1L, 2L))
})

test_that("StockFlowDiagram validation checks consistency", {
  stock1 <- new("Stock", id = 1L, name = "S", initial_value = 1000)

  expect_error(
    new("StockFlowDiagram",
      stocks = list(stock1),
      stock_ids = c(1L, 2L)  # Mismatched length
    ),
    "stock_ids length must match stocks length"
  )
})

# DecoratedCospan Class Tests ================================================

test_that("DecoratedCospan can be created", {
  diagram <- new("StockFlowDiagram",
    stocks = list(
      new("Stock", id = 1L, name = "S", initial_value = 100),
      new("Stock", id = 2L, name = "I", initial_value = 1)
    ),
    stock_ids = c(1L, 2L)
  )

  cospan <- new("DecoratedCospan",
    apex = diagram,
    left_leg = c(1L),
    right_leg = c(2L),
    left_foot = c(10L),
    right_foot = c(20L)
  )

  expect_s4_class(cospan, "DecoratedCospan")
  expect_equal(cospan@left_foot, c(10L))
  expect_equal(cospan@right_foot, c(20L))
})

test_that("OpenStockFlowDiagram validation checks injectivity", {
  diagram <- new("StockFlowDiagram",
    stocks = list(
      new("Stock", id = 1L, name = "S", initial_value = 100)
    ),
    stock_ids = c(1L)
  )

  expect_error(
    new("OpenStockFlowDiagram",
      apex = diagram,
      left_leg = c(1L, 1L),  # Not injective!
      right_leg = integer(0),
      left_foot = c(10L, 11L),
      right_foot = integer(0)
    ),
    "left_leg must be injective"
  )
})
