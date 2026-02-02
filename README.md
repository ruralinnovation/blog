# Rural Dataverse

**A Jekyll/Quarto hybrid data hub for the Center on Rural Innovation's Mapping and Data Analytics team.**

This site showcases datasets, R packages, projects, blog posts, and resources for rural innovation research.

**Architecture:** Jekyll for site structure, Quarto for content rendering (Phase 1). See [QUARTO-MIGRATION.md](QUARTO-MIGRATION.md) for details.

## Quick Start

### Prerequisites

- **Quarto CLI** - [Install Quarto](https://quarto.org/docs/get-started/)
- **R and RStudio** (optional, for R code execution)
- **Ruby and Jekyll** (optional, for local preview with Jekyll styling)

### Local Development

**Current (Phase 1): Jekyll with Quarto content**
```bash
bundle exec jekyll serve
```
View at: http://localhost:4000

**Future (Phase 2): Full Quarto website**
```bash
quarto preview
```
View at: http://localhost:4444

See [WORKFLOW.md](WORKFLOW.md) for detailed development instructions.

## Content Structure

All content lives in `posts/` organized by type:

```
posts/
├── datasets/          # Dataset documentation pages
│   └── qcew-employment-wages/
│       └── index.qmd
├── packages/          # R package documentation
│   └── ruraldefinitions/
│       └── index.qmd
├── projects/          # Project portfolios
│   └── rural-economic-outlook/
│       └── index.qmd
├── blog/              # Blog posts (stories)
│   └── 01_post_name/
│       └── index.qmd
└── resources/         # Guides and tools
    └── resource-name/
        └── index.qmd
```

## Adding Content

### Adding a Dataset

1. Create folder: `posts/datasets/dataset-name/`
2. Create file: `index.qmd` with YAML frontmatter:

```yaml
---
title: "Dataset Name"
description: "Brief description"
date: "2026-02-02"
categories: ["Dataset Category"]
tags: ["tag1", "tag2"]
dataset:
  source: "Source Organization"
  sourceUrl: "https://example.com"
  updateFrequency: "Annual"
---
```

3. Write content using Quarto markdown
4. Render: `quarto render posts/datasets/dataset-name/index.qmd`

### Adding an R Package

Same structure as datasets, but use `posts/packages/package-name/index.qmd`

### Adding a Project

Use `posts/projects/project-name/index.qmd` with:

```yaml
usesDatasets: ["dataset-slug1", "dataset-slug2"]
usesPackages: ["package-slug1"]
```

### Adding a Blog Post

Use `posts/blog/NN_post-slug/index.qmd` where NN is next number.

## Quarto Features

The site uses Quarto for all content, enabling:

- **Executable R code** - Code blocks that run and show output
- **Callouts** - Highlighted boxes (note, warning, tip, important)
- **Tabs** - Tabbed panels for organizing content
- **Diagrams** - Mermaid diagrams and flowcharts
- **Tables** - Auto-formatted tables from R dataframes
- **Citations** - Bibliography management
- **Cross-references** - Link to figures, tables, sections

See the [Contributor Guide](CONTRIBUTOR-GUIDE.md) for detailed instructions.

## Deployment

### GitHub Pages

The site deploys automatically to GitHub Pages from the `main` branch.

Configuration for production:
```yaml
baseurl: "/blog"  # For ruralinnovation.github.io/blog
```

Use `_config_prod.yml` for production builds if needed.

## Documentation

- **[WORKFLOW.md](WORKFLOW.md)** - Current development workflow (start here!)
- **[QUARTO-MIGRATION.md](QUARTO-MIGRATION.md)** - Migration details and future plans
- **[CONTRIBUTOR-GUIDE.md](CONTRIBUTOR-GUIDE.md)** - Comprehensive guide for contributors
- **[DEVELOPMENT.md](DEVELOPMENT.md)** - Technical development documentation

## Team

CORI Mapping and Data Analytics Team

## License

Content and code licensed for CORI use.
