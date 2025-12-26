#!/usr/bin/env Rscript
# Setup pkgdown for openstockflow

# Install pkgdown if needed
if (!requireNamespace("pkgdown", quietly = TRUE)) {
  install.packages("pkgdown", repos = "https://cloud.r-project.org")
}

# Load pkgdown
library(pkgdown)

# Initialize pkgdown configuration
usethis::use_pkgdown()

# Build the site
build_site()
