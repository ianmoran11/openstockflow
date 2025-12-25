# openstockflow Vignettes

This directory contains comprehensive documentation for the **openstockflow** package.

## Available Vignettes

The package provides vignettes in two flavors: **pipe-based API** and **algebraic API**. Both APIs create identical models - choose based on your preference.

### Pipe-Based API Vignettes

#### 1. Quick Start Guide (`quickstart.Rmd`)

**Target Audience**: New users who want to get started quickly

**Contents**:
- 5-minute tutorial with simple examples
- Exponential decay model
- Logistic growth model
- Predator-prey dynamics
- Brief introduction to algebraic API
- Common patterns and tips

**Access**:
```r
vignette("quickstart", "openstockflow")
```

#### 2. Introduction (`introduction.Rmd`)

**Target Audience**: Users who want comprehensive understanding

**Contents**:
- Overview of categorical stock-flow modeling
- SIR epidemic model (both APIs shown)
- SEIR model with sum variables
- Tank model with inflows/outflows
- Auxiliary variables example
- Workflow summary
- Design principles
- Performance considerations

**Access**:
```r
vignette("introduction", "openstockflow")
```

#### 3. Mathematical Foundations (`mathematical-foundations.Rmd`)

**Target Audience**: Users interested in the theoretical foundations

**Contents**:
- Primitive category H
- Stock-flow diagrams as functors
- Functorial semantics
- ODE generation algorithm
- Variables and dependency analysis
- Implementation details (S4 classes, functor representation)
- Categorical composition (decorated cospans)
- Stratification (pullbacks)
- Properties and guarantees
- Comparison to other approaches
- References and further reading

**Access**:
```r
vignette("mathematical-foundations", "openstockflow")
```

#### 4. Compositional Modeling (`composition.Rmd`)

**Target Audience**: Users building complex models from components

**Contents**:
- Introduction to composition via decorated cospans
- Converting diagrams to open form
- Simple composition examples (connected tanks)
- SIR model decomposition and recomposition
- Flow rate merging
- Associativity of composition
- Design patterns for reusable components
- Stratified and modular models

**Access**:
```r
vignette("composition", "openstockflow")
```

### Algebraic API Vignettes

The following vignettes demonstrate the same content using the algebraic API (`stock()`, `flow()`, `%->%`, `%+%`, `finalize()`):

#### 5. Quick Start Guide - Algebraic (`quickstart-algebraic.Rmd`)

Same content as quickstart.Rmd, but exclusively using algebraic API syntax.

**Access**:
```r
vignette("quickstart-algebraic", "openstockflow")
```

#### 6. Introduction - Algebraic (`introduction-algebraic.Rmd`)

Same content as introduction.Rmd, but exclusively using algebraic API syntax.

**Access**:
```r
vignette("introduction-algebraic", "openstockflow")
```

#### 7. Mathematical Foundations - Algebraic (`mathematical-foundations-algebraic.Rmd`)

Same theoretical content as mathematical-foundations.Rmd with algebraic API examples.

**Access**:
```r
vignette("mathematical-foundations-algebraic", "openstockflow")
```

#### 8. Compositional Modeling - Algebraic (`composition-algebraic.Rmd`)

Same content as composition.Rmd, but exclusively using algebraic API syntax.

**Access**:
```r
vignette("composition-algebraic", "openstockflow")
```

## Building the Vignettes

### Requirements

The vignettes require:
- `knitr` (>= 1.40)
- `rmarkdown` (>= 2.0)
- `deSolve` (>= 1.35) - optional, for running ODE examples

### Build All Vignettes

From the package root directory:

```r
devtools::build_vignettes()
```

Or build a single vignette:

```r
rmarkdown::render("vignettes/quickstart.Rmd")
```

## Testing Examples

A test script is provided to verify all vignette examples work:

```bash
cd vignettes
Rscript test_examples.R
```

This runs all code examples from the vignettes and verifies they execute correctly.

## Vignette Structure

Each vignette follows a consistent structure:

1. **YAML Header**: Metadata and vignette directives
2. **Setup Chunk**: Package loading and knitr options
3. **Content**: Narrative with interspersed code chunks
4. **Examples**: Executable R code demonstrating features
5. **Session Info**: R version and package versions

## Code Chunks

Code chunks use these options:
- `eval = TRUE`: Execute the code (default)
- `eval = FALSE`: Show code but don't run (for illustration)
- `eval = requireNamespace("pkg", quietly = TRUE)`: Conditional execution
- `include = FALSE`: Run but don't show in output
- `collapse = TRUE`: Collapse output with code

## Examples by Topic

### Basic Model Construction
- **quickstart.Rmd**: Decay, logistic growth
- **introduction.Rmd**: SIR, SEIR, tank models

### API Variants
- **introduction.Rmd**: Traditional vs. algebraic API
- **quickstart.Rmd**: Algebraic operators (%->%, %+%)

### Advanced Features
- **introduction.Rmd**: Sum variables, auxiliary variables
- **mathematical-foundations.Rmd**: Dependency graphs, composition

### Visualization
- **introduction.Rmd**: plot(), to_graphviz()
- **quickstart.Rmd**: plot_solution(), phase_plot()

### Theory
- **mathematical-foundations.Rmd**: Category theory, functors, semantics

## Contributing

When adding new vignettes:

1. Use consistent YAML header format
2. Include `VignetteIndexEntry`, `VignetteEngine`, and `VignetteEncoding`
3. Load package via `source()` in setup chunk (for development)
4. Use `requireNamespace()` for conditional features
5. Include session info at end
6. Test examples with `test_examples.R`
7. Update this README

## Future Vignettes

Planned for future phases:

- **Stratification**: Age-structured and multi-group models via pullbacks
- **tidygraph Integration**: Working with tbl_graph objects
- **Performance Optimization**: Compiled ODEs and large-scale models
- **Case Studies**: Real-world applications (COVID-19, climate, economics)
- **Advanced Composition**: Multi-way composition and composition patterns

## References

All vignettes cite these foundational papers:

1. Baez et al. (2022). "Compositional Modeling with Stock and Flow Diagrams." arXiv:2205.08373
2. Libkind et al. (2022). "A Categorical Framework for Modeling with Stock and Flow Diagrams." arXiv:2211.01290

See individual vignettes for complete reference lists.

---

Last updated: December 2025
