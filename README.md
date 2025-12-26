# openstockflow

<!-- badges: start -->
[![R-CMD-check](https://github.com/ianmoran11/openstockflow/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ianmoran11/openstockflow/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

**openstockflow** brings categorical stock-flow modeling to R, enabling you to build compositional dynamical systems using decorated cospans and functorial semantics.

## Overview

Stock-flow diagrams are a visual and mathematical framework for modeling systems with:
- **Stocks** (state variables) — quantities that accumulate over time
- **Flows** (rate processes) — transfers between stocks
- **Compositional structure** — build complex models from simpler components

### Why openstockflow?

- 🎯 **Two APIs**: Choose algebraic operators (`%->%`, `%+%`) or pipe-based syntax
- 🔬 **Rigorous math**: Based on category theory and functorial semantics
- 🧩 **Compositional**: Assemble models from reusable components
- 📊 **Integrated**: Works with `deSolve`, `tidyverse`, `ggplot2`, `ggraph`
- 🎓 **Well-documented**: Comprehensive vignettes and examples

## Installation

You can install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("ianmoran11/openstockflow")
```

## Quick Start

### Algebraic API (v0.2.0+)

Build an SIR epidemic model with automatic stock inclusion:

```r
library(openstockflow)

# Define stocks
S <- stock("S", initial = 999)
I <- stock("I", initial = 1)
R <- stock("R", initial = 0)

# Define flows
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

# Build model (stocks automatically included!)
sir <- (S %->% infection %->% I) %+%
       (I %->% recovery %->% R)

diagram <- finalize(sir)

# Solve the ODE system
result <- solve_diagram(
  diagram,
  times = seq(0, 100, by = 1),
  params = c(beta = 0.5, gamma = 0.1)
)

# Visualize
plot_solution(result)
```

### Pipe-Based API

Alternatively, use the pipe-based syntax:

```r
library(openstockflow)
library(magrittr)

diagram <- stock_flow_diagram() %>%
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

result <- solve_diagram(
  diagram,
  times = seq(0, 100, by = 1),
  params = c(beta = 0.5, gamma = 0.1)
)

plot_solution(result)
```

## Key Features

### Compositional Modeling

Compose models using categorical operations:

```r
# Create open diagrams with exposed interfaces
sir_open <- open_diagram(sir_diagram, interface = c("S", "R"))
seir_open <- open_diagram(seir_diagram, interface = c("S", "R"))

# Compose via pushout
combined <- compose_diagrams(sir_open, seir_open)
```

### Stratification

Stratify models by age, sex, or risk groups using pullbacks:

```r
# Stratify by age groups
age_groups <- c("young", "middle", "old")
age_stratified <- stratify_diagram(diagram,
  types = age_groups,
  type_assignment = list(S = "young", I = "middle", R = "old")
)
```

### Rich Visualization

```r
# Diagram structure
plot(diagram)

# Graphviz export
dot_str <- to_graphviz(diagram)

# With ggraph
library(ggraph)
plot(diagram, layout = "sugiyama")

# Solution plots
plot_solution(result)
phase_plot(result, "S", "I")
```

## Mathematical Foundation

Stock-flow diagrams are **functors** F: H → **FinSet** from a primitive category H to finite sets, where:

- **Objects**: {stock, flow, link}
- **Morphisms**: u, d: flow → stock (upstream/downstream)

The **ODE semantics** for a stock σ is:

dσ/dt = Σ(inflows) - Σ(outflows)

See the [Mathematical Foundations vignette](https://ianmoran11.github.io/openstockflow/articles/mathematical-foundations-algebraic.html) for details.

## Documentation

### Getting Started
- [Quick Start (Algebraic API)](https://ianmoran11.github.io/openstockflow/articles/quickstart-algebraic.html)
- [Quick Start (Pipe-Based API)](https://ianmoran11.github.io/openstockflow/articles/quickstart.html)

### Detailed Guides
- [Introduction (Algebraic API)](https://ianmoran11.github.io/openstockflow/articles/introduction-algebraic.html)
- [Introduction (Pipe-Based API)](https://ianmoran11.github.io/openstockflow/articles/introduction.html)
- [Compositional Modeling (Algebraic API)](https://ianmoran11.github.io/openstockflow/articles/composition-algebraic.html)
- [Compositional Modeling (Pipe-Based API)](https://ianmoran11.github.io/openstockflow/articles/composition.html)

### Advanced Topics
- [Mathematical Foundations (Algebraic API)](https://ianmoran11.github.io/openstockflow/articles/mathematical-foundations-algebraic.html)
- [Mathematical Foundations (Pipe-Based API)](https://ianmoran11.github.io/openstockflow/articles/mathematical-foundations.html)

## Examples

### SIR Epidemic Model
Classic compartmental model for disease spread.

### SEIR with Population Conservation
Adds exposed compartment with conserved total population.

### Predator-Prey Dynamics
Lotka-Volterra equations via stock-flow formalism.

### Logistic Growth
Population growth with carrying capacity.

### Tank with Inflows/Outflows
Physical system reaching equilibrium.

See the [vignettes](https://ianmoran11.github.io/openstockflow/articles/) for complete examples.

## What's New in v0.2.0

### Automatic Stock Inclusion

Stocks referenced in flow connections are now automatically included:

```r
# Before (verbose)
sir <- S %+% I %+% R %+%
  (S %->% infection %->% I) %+%
  (I %->% recovery %->% R)

# After (simplified)
sir <- (S %->% infection %->% I) %+%
       (I %->% recovery %->% R)
```

Fully backward compatible — explicit syntax still works!

## Project Status

**Current Status**: v0.2.0 - Automatic Stock Inclusion ✓

### Completed Features

- ✅ S4 class definitions with validation
- ✅ Primitive category H implementation
- ✅ Dual API (pipe-based + algebraic)
- ✅ ODE generation and deSolve integration
- ✅ Compositional modeling via decorated cospans
- ✅ Stratification via pullbacks
- ✅ Visualization (plot, ggraph, Graphviz)
- ✅ Automatic stock inclusion (v0.2.0)
- ✅ Comprehensive test suite (233 tests passing)
- ✅ 8 comprehensive vignettes

## Related Work

This package is inspired by:

- **[StockFlow.jl](https://github.com/AlgebraicJulia/StockFlow.jl)** — Julia implementation
- **[AlgebraicJulia](https://www.algebraicjulia.org)** — Applied category theory ecosystem

### Key Papers

1. **Baez, J. C., Li, X., Libkind, S., Osgood, N., & Redekopp, E. (2022).** "Compositional Modeling with Stock and Flow Diagrams." *arXiv:2205.08373v3*

2. **Libkind, S., Baas, A., Halter, M., Patterson, E., & Fairbanks, J. (2022).** "A Categorical Framework for Modeling with Stock and Flow Diagrams." *arXiv:2211.01290v3*

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT © Ian Moran

## Citation

If you use this package in your research, please cite:

```
@software{openstockflow,
  author = {Moran, Ian},
  title = {openstockflow: Categorical Stock-Flow Modeling in R},
  year = {2025},
  url = {https://github.com/ianmoran11/openstockflow}
}
```

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
