# openstockflow

<!-- badges: start -->
<!-- badges: end -->

**Categorical Stock-Flow Modeling with Compositional Diagrams**

`openstockflow` is the first R package to implement categorical stock-flow modeling using decorated cospans and functorial semantics. This approach enables compositional model building where complex systems are assembled from simpler components through categorical operations.

## Features

- **Compositional Modeling**: Combine stock-flow diagrams via decorated cospans
- **Stratification**: Create stratified models via pullbacks (e.g., by age, sex, risk group)
- **Functorial Semantics**: Formal separation of syntax (diagram structure) from semantics (ODE generation)
- **ODE Integration**: Seamless integration with deSolve for numerical simulation
- **tidyverse Integration**: Convert diagrams to tidygraph for visualization and analysis

## Mathematical Foundation

Based on the categorical framework described in:

- Baez, J. C., Li, X., Libkind, S., Osgood, N., & Redekopp, E. (2022). "Compositional Modeling with Stock and Flow Diagrams". *arXiv:2205.08373*
- Baez, J. C., Li, X., Libkind, S., Osgood, N., & Redekopp, E. (2022). "A Categorical Framework for Modeling with Stock and Flow Diagrams". *arXiv:2211.01290*

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("ianmoran/openstockflow")
```

## Quick Start

### Build a Simple SEIR Model

```r
library(openstockflow)

# Create SEIR epidemic model
seir <- stock_flow_diagram() %>%
  add_stock("S", initial = 1000) %>%
  add_stock("E", initial = 0) %>%
  add_stock("I", initial = 1) %>%
  add_stock("R", initial = 0) %>%
  add_flow("infection", from = "S", to = "E",
           rate = function(inputs, params) {
             params$beta * inputs$S * inputs$I / inputs$N
           }) %>%
  add_flow("progression", from = "E", to = "I",
           rate = function(inputs, params) params$sigma * inputs$E) %>%
  add_flow("recovery", from = "I", to = "R",
           rate = function(inputs, params) params$gamma * inputs$I) %>%
  add_sum_variable("N", stocks = c("S", "E", "I", "R"))

# Solve the model
result <- solve_diagram(
  seir,
  times = seq(0, 365, by = 1),
  parameters = c(beta = 0.5, sigma = 0.2, gamma = 0.1)
)

# Plot results
plot_solution(result)
```

### Compose Models

```r
# Compose two models via shared interface
combined_model <- compose(
  disease_spread,
  vaccine_rollout,
  interface = c("S", "V")
)
```

### Stratify by Demographics

```r
# Stratify by age groups
seir_age <- stratify(
  seir,
  by = "age",
  levels = c("young" = "0-17", "old" = "18+")
)
```

## Documentation

### Vignettes

The package includes comprehensive vignettes:

- **Quick Start Guide** (`vignette("quickstart", "openstockflow")`): 5-minute introduction with simple examples
- **Introduction** (`vignette("introduction", "openstockflow")`): Comprehensive tutorial with SIR, SEIR, and tank models
- **Mathematical Foundations** (`vignette("mathematical-foundations", "openstockflow")`): Category theory and functorial semantics

## Project Status

**Current Phase**: Phase 1 (MVP) - COMPLETE ✓

- [x] Package skeleton created
- [x] S4 class definitions with validation
- [x] Primitive category H implementation
- [x] Diagram construction API (traditional + algebraic)
- [x] ODE generation and deSolve integration
- [x] Visualization (plot, ggraph, Graphviz DOT export)
- [x] Comprehensive test suite (144 tests passing)
- [x] Vignettes and documentation

**Next Phase**: Phase 2 - Composition & tidygraph integration

See the [implementation plan](/.claude/plans/) for full development roadmap.

## Related Projects

- [StockFlow.jl](https://github.com/AlgebraicJulia/StockFlow.jl) - Julia implementation using AlgebraicJulia
- [tidygraph](https://tidygraph.data-imaginist.com/) - Tidy API for graph manipulation in R

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please open an issue or submit a pull request.

## Citation

If you use this package in your research, please cite:

```
@software{openstockflow,
  author = {Moran, Ian},
  title = {openstockflow: Categorical Stock-Flow Modeling in R},
  year = {2025},
  url = {https://github.com/ianmoran/openstockflow}
}
```
