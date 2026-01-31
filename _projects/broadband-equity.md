---
title: "Broadband Equity Analysis"
description: "Analyzing broadband access disparities in rural communities across the United States"
date: 2025-10-01
categories: ["Broadband", "Equity", "Infrastructure"]
tags: ["Policy", "Rural Development"]
featured: true
projectUrl: "https://broadband-equity.ruralinnovation.us"
status: "active"
team: ["drew-rosebush", "camden-blatchly", "olivier-leroy"]
usesDatasets: ["fcc-broadband"]
usesPackages: ["cori-fcc"]
usesResources: ["geographic-crosswalks"]
---

## Project Overview

The Broadband Equity Analysis project examines disparities in broadband access and adoption across rural America. By combining FCC broadband data with socioeconomic indicators, we identify communities most in need of infrastructure investment.

## Key Questions

1. Which rural communities lack adequate broadband access?
2. How does broadband availability correlate with economic outcomes?
3. What is the potential impact of broadband infrastructure investment?

## Methodology

Our analysis combines:

- FCC National Broadband Map data (census block level)
- Census demographic and economic data
- Geographic classifications (metro/non-metro, RUCA codes)
- Statistical modeling to estimate impact

## Key Findings

- XX% of rural census blocks lack access to 25/3 Mbps broadband
- Broadband access correlates with XX% higher median household income
- Closing the broadband gap could impact XX rural communities

## Data & Tools Used

**Datasets:**
- [FCC National Broadband Map](/datasets/fcc-broadband/)
- Census ACS Demographic Data

**R Packages:**
- [cori.fcc](/packages/cori-fcc/)
- tidycensus
- sf (spatial analysis)

**Resources:**
- [Geographic Crosswalks](/resources/geographic-crosswalks/)

## Outputs

- [Interactive Dashboard](https://broadband-equity.ruralinnovation.us)
- [Research Report](https://ruralinnovation.us/reports/broadband-equity)
- [Blog Post: Estimating Broadband Adoption Impact](/stories/estimating-adoption-impact/)

## Team

- Drew Rosebush - Project Lead
- Camden Blatchly - Data Analysis
- Olivier Leroy - Spatial Analysis

## GitHub Repository

[View Code](https://github.com/ruralinnovation/broadband-equity)
