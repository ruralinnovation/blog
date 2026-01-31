---
title: "Geographic Crosswalks"
description: "Reference guide for converting between different geographic boundary systems"
date: 2025-09-10
categories: ["Geography", "Reference"]
tags: ["Crosswalks", "Census", "USDA"]
type: "guide"
featured: true
---

## Overview

Geographic crosswalks allow you to convert data between different geographic boundary systems. This is essential when combining datasets that use different geographic classifications.

## Common Crosswalks

### County to Metro/Micro Classification

Convert county FIPS codes to metropolitan/micropolitan statistical areas.

**Source:** Office of Management and Budget (OMB)
**Update Frequency:** Periodic (every ~10 years)
**Download:** [OMB Delineation Files](https://www.census.gov/geographies/reference-files/time-series/demo/metro-micro/delineation-files.html)

### Census Tract to RUCA Codes

Convert census tracts to Rural-Urban Commuting Area (RUCA) codes.

**Source:** USDA Economic Research Service
**Update Frequency:** Decennial (based on Census)
**Download:** [USDA RUCA Codes](https://www.ers.usda.gov/data-products/rural-urban-commuting-area-codes/)

### ZIP Code to County

Crosswalk between ZIP Code Tabulation Areas (ZCTAs) and counties.

**Source:** HUD USPS ZIP Code Crosswalk Files
**Update Frequency:** Quarterly
**Download:** [HUD Crosswalk Files](https://www.huduser.gov/portal/datasets/usps_crosswalk.html)

### Census Block to County Subdivision

Link census blocks to county subdivisions (towns, townships).

**Source:** US Census Bureau TIGER/Line Files
**Update Frequency:** Annual
**Download:** [TIGER/Line Files](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)

## Usage in R

### Using Geocorr (Missouri Census Data Center)

```r
# Install geocorr package
# remotes::install_github("DSPG-Young-Scholars-Program/geocorr")

library(geocorr)

# County to Metro/Micro
crosswalk <- get_crosswalk(
  from = "county",
  to = "cbsa",
  year = 2020
)
```

### Using tigris Package

```r
library(tigris)

# Get county boundaries
counties <- counties(year = 2020)

# Get tract boundaries
tracts <- tracts(state = "VT", year = 2020)

# Spatial join to create crosswalk
```

## Best Practices

1. **Always cite the source and vintage** of your crosswalk
2. **Check for many-to-many relationships** (e.g., ZIP codes can span counties)
3. **Weight by population or area** when aggregating data
4. **Document assumptions** when boundaries don't align perfectly

## Related Resources

- [Rural Definitions Guide](/resources/rural-definitions/)
- [CORI Blog: Tips for Mapping Rural Data](/stories/mapping-rural-tips/)

## References

- US Census Bureau TIGER/Line Shapefiles
- USDA Economic Research Service Rural-Urban Continuum Codes
- OMB Bulletin on Metropolitan Statistical Areas
- HUD USPS ZIP Code Crosswalk Files
