---
title: "cori.data.fcc"
description: "R package for accessing and analyzing FCC broadband data"
date: 2025-11-20
categories: ["R Package", "Broadband"]
tags: ["FCC", "Data Processing", "R"]
featured: true
packageName: "cori.data.fcc"
githubUrl: "https://github.com/ruralinnovation/cori.data.fcc"
installCommand: 'remotes::install_github("ruralinnovation/cori.data.fcc")'
status: "stable"
version: "0.2.1"
maintainer: "CORI MDA Team"
---

## Overview

The `cori.data.fcc` package provides tools for downloading, processing, and analyzing FCC broadband data in R. It simplifies access to the National Broadband Map and other FCC data sources.

## Installation

Install the package from GitHub:

```r
# Install remotes if you haven't already
install.packages("remotes")

# Install cori.data.fcc
remotes::install_github("ruralinnovation/cori.data.fcc")
```

## Quick Start

```r
library(cori.data.fcc)

# Download broadband availability data for a state
data <- get_broadband_data(state = "VT")

# Analyze coverage by technology type
coverage_summary <- summarize_coverage(data, by = "technology")

# Create coverage map
plot_coverage(data, metric = "download_speed")
```

## Key Functions

- `get_broadband_data()` - Download FCC broadband data
- `summarize_coverage()` - Aggregate coverage statistics
- `plot_coverage()` - Create coverage visualizations
- `calculate_gaps()` - Identify coverage gaps

## Use Cases

This package is used in:

- [Broadband Equity Analysis Project](/projects/broadband-equity/)
- [FCC Data Overview Blog Post](/stories/fcc-data-overview/)
- [Estimating Broadband Adoption Impact](/stories/estimating-adoption-impact/)

## Documentation

- [GitHub Repository](https://github.com/ruralinnovation/cori.data.fcc)
- [Function Reference](https://github.com/ruralinnovation/cori.data.fcc/wiki)
- [Example Vignettes](https://github.com/ruralinnovation/cori.data.fcc/tree/main/vignettes)

## Related Datasets

- [FCC National Broadband Map](/datasets/fcc-broadband/)
