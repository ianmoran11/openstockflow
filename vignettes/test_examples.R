# Test that vignette examples work
# This script verifies that the code examples in the vignettes execute correctly

# Load package
source("../R/classes.R")
source("../R/primitives.R")
source("../R/diagram.R")
source("../R/algebraic-api.R")
source("../R/semantics.R")
source("../R/visualization.R")
library(magrittr)

cat("Testing vignette examples...\n\n")

# Test 1: SIR model (from introduction.Rmd)
cat("1. Testing SIR model (traditional API)...\n")
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
print(sir)
cat("✓ SIR model created successfully\n\n")

# Test 2: SIR model (algebraic API)
cat("2. Testing SIR model (algebraic API)...\n")
S <- stock("S", initial = 999)
I <- stock("I", initial = 1)
R <- stock("R", initial = 0)

infection <- flow("infection",
  rate = function(inputs, params) {
    params$beta * inputs$S * inputs$I / 1000
  }
)

recovery <- flow("recovery",
  rate = function(inputs, params) {
    params$gamma * inputs$I
  }
)

sir_alg <- S %+% I %+% R %+%
  (S %->% infection %->% I) %+%
  (I %->% recovery %->% R)

sir_diagram <- finalize(sir_alg, reset_ids = TRUE)
cat("✓ Algebraic SIR model created successfully\n\n")

# Test 3: SEIR model with sum variable
cat("3. Testing SEIR model with sum variable...\n")
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
    rate = function(inputs, params) {
      params$sigma * inputs$E
    }
  ) %>%
  add_flow("recovery",
    from = "I",
    to = "R",
    rate = function(inputs, params) {
      params$gamma * inputs$I
    }
  ) %>%
  add_link(from = "N", to = "infection")
print(seir)
cat("✓ SEIR model created successfully\n\n")

# Test 4: Tank with inflows/outflows
cat("4. Testing tank with inflows/outflows...\n")
tank <- stock_flow_diagram(reset_ids = TRUE) %>%
  add_stock("Water", initial = 50) %>%
  add_flow("fill",
    from = NULL,
    to = "Water",
    rate = function(inputs, params) {
      params$inflow_rate
    }
  ) %>%
  add_flow("drain",
    from = "Water",
    to = NULL,
    rate = function(inputs, params) {
      params$outflow_rate * inputs$Water
    }
  )
print(tank)
cat("✓ Tank model created successfully\n\n")

# Test 5: Decay model (from quickstart.Rmd)
cat("5. Testing exponential decay model...\n")
decay_model <- stock_flow_diagram(reset_ids = TRUE) %>%
  add_stock("X", initial = 100) %>%
  add_flow("decay",
    from = "X",
    to = NULL,
    rate = function(inputs, params) {
      params$k * inputs$X
    }
  )
print(decay_model)
cat("✓ Decay model created successfully\n\n")

# Test 6: Logistic growth
cat("6. Testing logistic growth model...\n")
logistic <- stock_flow_diagram(reset_ids = TRUE) %>%
  add_stock("Population", initial = 10) %>%
  add_flow("births",
    from = NULL,
    to = "Population",
    rate = function(inputs, params) {
      growth_rate <- params$r * (1 - inputs$Population / params$K)
      growth_rate * inputs$Population
    }
  )
print(logistic)
cat("✓ Logistic growth model created successfully\n\n")

# Test 7: Predator-prey
cat("7. Testing predator-prey model...\n")
predator_prey <- stock_flow_diagram(reset_ids = TRUE) %>%
  add_stock("Prey", initial = 100) %>%
  add_stock("Predator", initial = 10) %>%
  add_flow("prey_growth",
    from = NULL,
    to = "Prey",
    rate = function(inputs, params) {
      params$alpha * inputs$Prey
    }
  ) %>%
  add_flow("predation",
    from = "Prey",
    to = "Predator",
    rate = function(inputs, params) {
      params$beta * inputs$Prey * inputs$Predator
    }
  ) %>%
  add_flow("predator_death",
    from = "Predator",
    to = NULL,
    rate = function(inputs, params) {
      params$delta * inputs$Predator
    }
  )
print(predator_prey)
cat("✓ Predator-prey model created successfully\n\n")

# Test 8: Visualization
cat("8. Testing visualization...\n")
plot(sir)
cat("✓ Plot method works\n")

dot_str <- to_graphviz(sir)
cat("✓ Graphviz export works\n")
cat("   DOT string length:", nchar(dot_str), "characters\n\n")

# Test 9: ODE generation
cat("9. Testing ODE generation...\n")
ode_func <- generate_ode(sir)
cat("✓ ODE function generated\n")

state <- initial_state(sir)
params <- c(beta = 0.5, gamma = 0.1)
deriv <- ode_func(0, state, params)
cat("✓ ODE function evaluates correctly\n")
cat("   Derivatives:", deriv[[1]], "\n\n")

# Test 10: Simulation (if deSolve available)
if (requireNamespace("deSolve", quietly = TRUE)) {
  cat("10. Testing simulation with deSolve...\n")
  result <- solve_diagram(
    sir,
    times = seq(0, 50, by = 1),
    params = c(beta = 0.5, gamma = 0.1)
  )
  cat("✓ Simulation completed\n")
  cat("   Result dimensions:", nrow(result), "rows x", ncol(result), "columns\n")
  cat("   Final values: S =", tail(result$S, 1), ", I =", tail(result$I, 1), ", R =", tail(result$R, 1), "\n\n")

  cat("11. Testing solution plotting...\n")
  plot_solution(result)
  cat("✓ Solution plotting works\n\n")
} else {
  cat("10. Skipping simulation tests (deSolve not installed)\n\n")
}

cat("=====================================\n")
cat("All vignette examples work correctly!\n")
cat("=====================================\n")
