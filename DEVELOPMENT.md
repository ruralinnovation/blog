# Rural Dataverse - Development Guide

## Quick Start

This site uses **Jekyll** (for site structure) + **Quarto** (for blog posts with R code).

### Prerequisites

1. **Ruby** (2.7 or higher) - [Install Ruby](https://www.ruby-lang.org/en/downloads/)
2. **Bundler** - Install with `gem install bundler`
3. **R and RStudio** (optional, for blog posts) - [Install R](https://www.r-project.org/)
4. **Quarto** (optional, for blog posts) - [Install Quarto](https://quarto.org/docs/get-started/)

### Installation

```bash
# 1. Clone the repository
git clone https://github.com/ruralinnovation/blog.git
cd blog

# 2. Install Ruby dependencies
bundle install

# 3. Start the development server
bundle exec jekyll serve

# 4. Open in browser
# Visit: http://localhost:4000/blog/
```

That's it! The site will auto-reload when you make changes.

---

## Development Workflow

### Adding Content

#### 1. Add a Dataset

Create a new file in `_datasets/`:

```bash
# Create file: _datasets/my-dataset.md
---
title: "My Dataset Name"
description: "Brief description of the dataset"
date: 2026-01-31
categories: ["Category1", "Category2"]
tags: ["tag1", "tag2"]
featured: true  # Optional: shows on homepage
downloadUrl: "https://link-to-data.com"
githubUrl: "https://github.com/..."
dataFormat: "CSV, JSON"
updateFrequency: "Monthly"
coverage: "United States"
---

## Overview
Your content here...
```

#### 2. Add an R Package

Create a new file in `_packages/`:

```bash
# Create file: _packages/my-package.md
---
title: "my.package"
description: "What the package does"
date: 2026-01-31
categories: ["R Package"]
packageName: "my.package"
githubUrl: "https://github.com/ruralinnovation/my.package"
installCommand: 'remotes::install_github("ruralinnovation/my.package")'
status: "stable"  # or "beta", "development"
version: "1.0.0"
---

## Installation
Your content here...
```

#### 3. Add a Blog Post (Story)

**Option A: Simple Markdown (no code execution)**

```bash
# Create file: _stories/my-post.md
---
title: "My Blog Post"
description: "Brief description"
date: 2026-01-31
author: "Your Name"
categories: ["Category"]
tags: ["tag1"]
---

Your content here...
```

**Option B: Quarto with R Code**

```bash
# Create file: _stories/my-analysis.qmd
---
title: "My Analysis"
description: "Brief description"
date: 2026-01-31
author: "Your Name"
categories: ["Data Analysis"]
freeze: true  # Cache code execution
---

Your content with R code chunks...

```{r}
library(tidyverse)
data <- read_csv("data.csv")
plot(data)
```
```

Then in RStudio:
1. Open the `.qmd` file
2. Click "Render"
3. Quarto generates `.md` file
4. Jekyll picks up the `.md` file automatically

#### 4. Add a Project

Create a new file in `_projects/`:

```bash
# Create file: _projects/my-project.md
---
title: "My Project"
description: "Project description"
date: 2026-01-31
categories: ["Category"]
status: "active"  # or "completed"
projectUrl: "https://project-url.com"
team: ["person1", "person2"]
usesDatasets: ["dataset-slug"]  # Links to datasets
usesPackages: ["package-slug"]  # Links to packages
---

Your content here...
```

#### 5. Add a Resource

Create a new file in `_resources/`:

```bash
# Create file: _resources/my-resource.md
---
title: "My Resource"
description: "Resource description"
date: 2026-01-31
type: "guide"  # or "tool", "publication"
categories: ["Category"]
---

Your content here...
```

---

## Deployment

### Simple Deployment (Current Setup)

**Local Build → Commit → Push**

```bash
# 1. Build the site locally
bundle exec jekyll build

# 2. This generates the site in docs/ folder
# (configured in _config.yml: output-dir: docs)

# 3. Commit and push to GitHub
git add .
git commit -m "Update site content"
git push origin main

# 4. GitHub Pages automatically serves from docs/ folder
```

**That's it!** Your changes are live in ~1 minute.

---

### Improved Deployment (GitHub Actions)

For automatic builds on push, create `.github/workflows/deploy.yml`:

```yaml
name: Deploy Jekyll Site

on:
  push:
    branches: [main]
  workflow_dispatch:

jobs:
  build-deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup Ruby
        uses: ruby/setup-ruby@v1
        with:
          ruby-version: '3.2'
          bundler-cache: true

      - name: Build site
        run: bundle exec jekyll build

      - name: Deploy to GitHub Pages
        uses: peaceiris/actions-gh-pages@v3
        with:
          github_token: ${{ secrets.GITHUB_TOKEN }}
          publish_dir: ./docs
```

**Benefits:**
- Auto-builds on every push to main
- No need to commit the `docs/` folder
- Cleaner git history

---

## File Structure

```
/blog/
├── _config.yml           # Jekyll configuration
├── _quarto.yml           # Quarto configuration
├── Gemfile               # Ruby dependencies
├── package.json          # NPM scripts (optional)
│
├── Collections (Content):
│   ├── _datasets/        # Data catalog
│   ├── _packages/        # R packages
│   ├── _projects/        # Projects
│   ├── _stories/         # Blog posts
│   └── _resources/       # Resources
│
├── _layouts/             # Page templates
│   ├── default.html
│   ├── dataset.html
│   ├── package.html
│   ├── project.html
│   ├── story.html
│   └── resource.html
│
├── _includes/            # Reusable components
│   ├── nav.html
│   ├── footer.html
│   └── components/
│       └── card.html
│
├── _data/                # Data files (YAML)
│
├── assets/               # Static assets
│   ├── css/
│   ├── images/
│   └── fonts/
│
├── Pages (Top-level):
│   ├── index.html        # Homepage
│   ├── datasets.html     # Datasets listing
│   ├── packages.html     # Packages listing
│   ├── projects.html     # Projects listing
│   ├── stories.html      # Blog listing
│   ├── resources.html    # Resources listing
│   └── about.html        # About page
│
└── docs/                 # Built site (generated by Jekyll)
```

---

## Common Tasks

### Preview Changes Locally

```bash
bundle exec jekyll serve
# Visit: http://localhost:4000/blog/
# Press Ctrl+C to stop
```

### Build for Production

```bash
bundle exec jekyll build
# Output: docs/ folder
```

### Clear Cache (if changes aren't showing)

```bash
rm -rf _site .jekyll-cache
bundle exec jekyll serve
```

### Update Dependencies

```bash
bundle update
```

---

## Workflow for Quick Iteration

### Daily Development Loop:

1. **Start dev server** (once per session):
   ```bash
   bundle exec jekyll serve
   ```

2. **Make changes**:
   - Edit markdown files in collections
   - Edit layouts/includes
   - Edit CSS

3. **Auto-reload**: Site rebuilds automatically, refresh browser

4. **Commit when ready**:
   ```bash
   git add .
   git commit -m "Added new dataset"
   git push origin main
   ```

5. **Site updates**: GitHub Pages rebuilds (1-2 minutes)

### No build step needed for most edits!

Jekyll auto-rebuilds on file changes during development. Just save and refresh your browser.

---

## Migrating Existing Blog Posts

To move posts from `posts/` to `_stories/`:

```bash
# Move a post folder
cp -r posts/01_awesomejq _stories/awesome-jq

# The .qmd file works as-is!
# Jekyll will render the Quarto-generated .md file
```

---

## Tips

1. **Use descriptive filenames**: `fcc-broadband.md` not `dataset1.md`
2. **Always set `date`**: Used for sorting
3. **Use `featured: true`**: Shows on homepage
4. **Link between content**: Use relative URLs like `/datasets/fcc-broadband/`
5. **Test locally first**: Always preview before pushing

---

## Troubleshooting

### Site not building?

```bash
# Check for YAML errors
bundle exec jekyll build --verbose

# Check Ruby version
ruby --version  # Should be 2.7+

# Reinstall dependencies
rm Gemfile.lock
bundle install
```

### Quarto not rendering?

```bash
# Check Quarto installation
quarto check

# Render manually
quarto render _stories/my-post.qmd
```

### Changes not showing on GitHub Pages?

1. Check Actions tab on GitHub for build errors
2. Ensure GitHub Pages is enabled in repo settings
3. Verify source is set to `docs/` folder from main branch
4. Wait 1-2 minutes for build to complete

---

## Resources

- [Jekyll Documentation](https://jekyllrb.com/docs/)
- [Quarto Documentation](https://quarto.org/docs/)
- [Liquid Templating](https://shopify.github.io/liquid/)
- [GitHub Pages](https://docs.github.com/en/pages)

---

## Questions?

Reach out to the CORI MDA team or open an issue on GitHub.
