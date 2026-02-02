# Rural Dataverse Contributor Guide

**A Comprehensive Training Document for Data Analysts Contributing to the Rural Dataverse Blog**

**Version:** 2.0 (Quarto Migration)
**Last Updated:** February 2, 2026
**Audience:** Data analysts and R users with minimal web development experience

---

## 🎉 Important: Site Migrated to Quarto!

**As of February 2, 2026, the Rural Dataverse has migrated from Jekyll collections to a Quarto-first architecture.**

### What Changed

**Before (Jekyll Collections):**
- Datasets, packages, projects: Plain markdown files in `_datasets/`, `_packages/`, etc.
- Limited features: Just text, links, images
- Separate workflow from blog posts

**Now (Quarto Unified):**
- **Everything is Quarto:** Datasets, packages, projects, blog posts all use `.qmd` files
- **All live in `posts/`:** Organized by type (`posts/datasets/`, `posts/packages/`, etc.)
- **Rich features:** Executable R code, callouts, tabs, diagrams, interactive visualizations
- **Unified workflow:** Same authoring process for all content types

### Migration Status

- ✅ Architecture updated
- ✅ Sample dataset created (QCEW in Quarto format)
- 📦 Old markdown files backed up to `_backup/pre-quarto-migration/`
- 🔄 Remaining content to be migrated as needed

### For New Contributors

Follow the Quarto workflows in this guide. The core concepts (Git, structure, etc.) remain the same, but all content creation now uses Quarto's `.qmd` format with enhanced capabilities.

---

## Table of Contents

1. [Introduction](#introduction)
2. [Understanding the System: Concepts First](#understanding-the-system-concepts-first)
3. [Prerequisites and Setup](#prerequisites-and-setup)
4. [Understanding the File Structure](#understanding-the-file-structure)
5. [Git and GitHub Basics](#git-and-github-basics)
6. [Markdown and YAML Syntax](#markdown-and-yaml-syntax)
7. [Contributing Content: Step-by-Step Workflows](#contributing-content-step-by-step-workflows)
8. [R Code Blocks and Quarto Features](#r-code-blocks-and-quarto-features)
9. [Testing Before Publishing](#testing-before-publishing)
10. [Collaboration and Code Review](#collaboration-and-code-review)
11. [Common Gotchas and Troubleshooting](#common-gotchas-and-troubleshooting)
12. [Quick Reference](#quick-reference)

---

## Introduction

Welcome to the Rural Dataverse contributor guide! This document will help you contribute datasets, R packages, blog posts, projects, and resources to the CORI Mapping and Data Analytics team's blog.

**What is the Rural Dataverse?**

The Rural Dataverse is a hybrid static website that serves as a comprehensive hub for:
- **Data Catalog** - Documenting datasets we use
- **R Package Documentation** - Showcasing our R packages
- **Blog Posts** - Data analysis articles with executable R code
- **Project Portfolio** - Highlighting our work
- **Resource Library** - Guides and publications

**Who is this guide for?**

This guide is designed for data analysts and R users who are comfortable with R and RStudio but may be unfamiliar with web development, Jekyll, Quarto, or Git workflows.

---

## Understanding the System: Concepts First

Before diving into the "how," let's understand the "why" and "what" of our system.

### What is a Static Site Generator?

Think of a static site generator as a **recipe book that creates a finished cookbook**. You write individual recipes (your content in markdown files), and the generator assembles them into a complete, published cookbook (HTML website) that readers can browse.

**Benefits:**
- Fast loading times
- Secure (no database to hack)
- Version controlled (every change is tracked)
- Easy to host (just HTML files)

### What is Jekyll?

**Jekyll** is our primary static site generator. Written in Ruby, it takes markdown files and templates and generates a complete website.

**Think of Jekyll as:** A filing system and organizer for different types of content.

**Jekyll's job in our system:**
1. **Organizes content** into collections (datasets, packages, projects, stories, resources)
2. **Applies templates** to make all pages look consistent
3. **Generates navigation** menus automatically
4. **Creates the final HTML** website from your markdown files

**Key Jekyll concepts:**
- **Collections:** Groups of similar content (like chapters in a book). Our collections are `_datasets/`, `_packages/`, `_projects/`, `_stories/`, and `_resources/`
- **Layouts:** HTML templates that wrap your content (like page designs in a book template)
- **Front Matter:** YAML metadata at the top of files (like catalog cards describing each item)
- **Config File:** `_config.yml` controls site-wide settings

### What is Quarto?

**Quarto** is a scientific publishing system that can execute R code and create documents with data visualizations.

**Think of Quarto as:** A chef who can follow recipes (R code) and create finished dishes (rendered documents with plots and results).

**Quarto's job in our system:**
1. **Executes R code** in blog posts
2. **Generates plots and visualizations** from that code
3. **Creates markdown output** that Jekyll can then process
4. **Manages citations and references** in scientific documents

**Key Quarto concepts:**
- **QMD files:** Quarto Markdown files (`.qmd`) that contain text + R code
- **Code chunks:** Blocks of R code that execute when rendering
- **YAML header:** Configuration at the top of QMD files (similar to Jekyll front matter)
- **Rendering:** The process of running R code and generating output

### How Jekyll and Quarto Work Together

Our system is a **two-stage pipeline**:

```
Stage 1 (Quarto):          Stage 2 (Jekyll):
QMD files with R code  →   Markdown files    →   HTML website
     (in posts/)              (in _stories/)       (in docs/)
```

**The workflow:**

1. **You write** a blog post in Quarto (`.qmd` file) with R code
2. **Quarto runs** your R code and creates markdown with results
3. **Jekyll reads** that markdown and applies website styling
4. **Jekyll generates** the final HTML website

**Why this hybrid approach?**

- **Quarto** gives us the power to include executable R code in blog posts
- **Jekyll** gives us the structure to organize multiple content types (not just blog posts)
- Together, they create a **data-driven blog** with organized content collections

**Mental model:**
- Quarto = Kitchen where R code is cooked into results
- Jekyll = Publishing house that organizes and formats everything into a finished book

### Source Files vs. Generated Files

This is crucial to understand:

| Type | Location | What It Is | Do You Edit It? |
|------|----------|-----------|----------------|
| **Source** | `posts/*.qmd` | Original blog posts with R code | YES |
| **Generated** | `_stories/*.md` | Markdown output from Quarto | NO - Auto-generated |
| **Source** | `_packages/*.md` | R package documentation | YES |
| **Generated** | `docs/` | Final HTML website | NO - Auto-generated |
| **Generated** | `_site/` | Development build folder | NO - Auto-generated |

**Golden Rule:** Only edit source files. Never edit generated files - they'll be overwritten.

---

## Prerequisites and Setup

Before contributing, you need to set up your development environment. This section walks you through installing everything you need.

### What You'll Need

- **R and RStudio** - For creating content with R code
- **Ruby and Jekyll** - For building the website
- **Git** - For version control and collaboration
- **A GitHub account** - For accessing the repository
- **A text editor** - RStudio or VS Code work great

### Installing R and RStudio

**If you already have R and RStudio installed, skip to the next section.**

1. **Install R:**
   - Visit [https://cran.r-project.org/](https://cran.r-project.org/)
   - Download R for your operating system (Mac, Windows, Linux)
   - Run the installer and follow the prompts
   - Default settings are fine

2. **Install RStudio:**
   - Visit [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
   - Download RStudio Desktop (free version)
   - Run the installer and follow the prompts

3. **Verify installation:**
   - Open RStudio
   - In the Console, type: `R.version.string`
   - You should see your R version (e.g., "R version 4.3.2")

### Installing Ruby and Jekyll

Jekyll requires Ruby. Installing Ruby and Jekyll can be tricky depending on your operating system.

#### macOS

macOS comes with Ruby, but we need a newer version for Jekyll:

1. **Install Homebrew** (if you don't have it):
   ```bash
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
   ```

2. **Install Ruby via Homebrew:**
   ```bash
   brew install ruby
   ```

3. **Add Ruby to your PATH** (add these lines to `~/.zshrc` or `~/.bash_profile`):
   ```bash
   export PATH="/usr/local/opt/ruby/bin:$PATH"
   export PATH="$HOME/.gem/ruby/X.X.0/bin:$PATH"
   ```
   (Replace X.X with your Ruby version, e.g., 3.2)

4. **Install Jekyll and Bundler:**
   ```bash
   gem install jekyll bundler
   ```

5. **Verify installation:**
   ```bash
   jekyll -v
   ```
   You should see the Jekyll version (e.g., "jekyll 4.3.2")

#### Windows

1. **Install Ruby via RubyInstaller:**
   - Visit [https://rubyinstaller.org/](https://rubyinstaller.org/)
   - Download Ruby+Devkit (latest version with MSYS2)
   - Run the installer
   - At the end, run `ridk install` and choose option 3 (MSYS2 and MINGW)

2. **Install Jekyll and Bundler:**
   ```bash
   gem install jekyll bundler
   ```

3. **Verify installation:**
   ```bash
   jekyll -v
   ```

#### Linux (Ubuntu/Debian)

```bash
sudo apt-get update
sudo apt-get install ruby-full build-essential zlib1g-dev

# Add to ~/.bashrc
echo '# Install Ruby Gems to ~/gems' >> ~/.bashrc
echo 'export GEM_HOME="$HOME/gems"' >> ~/.bashrc
echo 'export PATH="$HOME/gems/bin:$PATH"' >> ~/.bashrc
source ~/.bashrc

gem install jekyll bundler
```

**Common Installation Issues:**

- **"Permission denied" errors:** Never use `sudo` with gem commands. Install to your home directory instead.
- **"Failed to build gem native extension":** You may be missing development tools. Install Xcode Command Line Tools (Mac) or build-essential (Linux).

### Installing Git

#### macOS
Git is usually pre-installed. Verify by opening Terminal and typing:
```bash
git --version
```

If not installed:
```bash
brew install git
```

#### Windows
1. Download from [https://git-scm.com/download/win](https://git-scm.com/download/win)
2. Run the installer (default settings are fine)
3. Choose "Git from the command line and also from 3rd-party software"

#### Linux
```bash
sudo apt-get install git
```

### Installing GitHub Desktop (Optional)

If you prefer a graphical interface for Git:

1. Visit [https://desktop.github.com/](https://desktop.github.com/)
2. Download and install for your OS
3. Sign in with your GitHub account

**Note:** This guide will show both command-line Git and GitHub Desktop approaches.

### Cloning the Repository

Once all tools are installed, get a copy of the project:

#### Using Command Line:

```bash
# Navigate to where you want the project
cd ~/Documents/GitHub

# Clone the repository
git clone https://github.com/ruralinnovation/blog.git

# Navigate into the project
cd blog

# Install Ruby dependencies
bundle install
```

#### Using GitHub Desktop:

1. Open GitHub Desktop
2. File → Clone Repository
3. Select "ruralinnovation/blog" or enter the URL
4. Choose where to save it locally
5. Click "Clone"

**After cloning, install dependencies:**

Open Terminal, navigate to the project folder, and run:
```bash
bundle install
```

This installs all the Ruby gems (packages) the project needs.

---

## Understanding the File Structure

Let's explore the project structure so you know where everything goes.

### Top-Level Overview

```
blog/
├── _config.yml          # Jekyll configuration (site settings)
├── _quarto.yml          # Quarto configuration (blog settings)
├── Gemfile              # Ruby dependencies
├── README.md            # Quick team reference
├── DEVELOPMENT.md       # Technical documentation
├── CONTRIBUTOR-GUIDE.md # This document!
│
├── posts/               # SOURCE: Original Quarto blog posts (.qmd)
├── _stories/            # GENERATED: Rendered blog posts (don't edit)
│
├── _datasets/           # SOURCE: Dataset documentation (.md)
├── _packages/           # SOURCE: R package documentation (.md)
├── _projects/           # SOURCE: Project descriptions (.md)
├── _resources/          # SOURCE: Guides and publications (.md)
│
├── _layouts/            # HTML templates for pages
├── _includes/           # Reusable HTML components
├── assets/              # Static files (CSS, images, fonts)
├── src/                 # Custom SCSS styling
│
├── docs/                # GENERATED: Published website (GitHub Pages)
├── _site/               # GENERATED: Development build (don't commit)
│
└── ideation docs/       # Team notes and planning
```

### Collections: Where Content Lives

Jekyll organizes content into **collections**. Each collection is a folder starting with `_`:

| Collection | Folder | Purpose | File Type |
|-----------|--------|---------|-----------|
| **Datasets** | `_datasets/` | Document datasets | `.md` |
| **Packages** | `_packages/` | Document R packages | `.md` |
| **Projects** | `_projects/` | Describe projects | `.md` |
| **Stories** | `_stories/` | Blog posts (generated) | `.md` |
| **Resources** | `_resources/` | Guides, tools, publications | `.md` |

**Important:** Blog post sources live in `posts/` (not `_stories/`). Quarto generates the files in `_stories/`.

### Configuration Files

| File | What It Controls |
|------|-----------------|
| `_config.yml` | Jekyll settings: site title, navigation, plugins, collections |
| `_quarto.yml` | Quarto settings: blog rendering, R execution, output format |
| `Gemfile` | Ruby gem dependencies for Jekyll |

**When to edit config files:**
- Adding a new navigation menu item → `_config.yml`
- Changing site title or description → `_config.yml`
- Adjusting how R code is executed → `_quarto.yml`
- Changing blog appearance → `_quarto.yml`

**Warning:** After editing config files, you must restart the development server to see changes.

### Generated Folders (Don't Edit)

| Folder | What It Is | Why Not to Edit |
|--------|-----------|----------------|
| `_site/` | Development build | Regenerated every time you build |
| `docs/` | Production build (GitHub Pages) | Regenerated on deployment |
| `_stories/*.md` | Rendered blog posts | Regenerated from `posts/*.qmd` |

**If you edit these:** Your changes will be lost the next time the site builds.

---

## Git and GitHub Basics

Git is a version control system that tracks changes to files. GitHub is a platform for hosting Git repositories and collaborating with others.

### Core Git Concepts

**Think of Git as:** A time machine for your code that also enables collaboration.

**Key concepts:**
- **Repository (repo):** A project folder tracked by Git
- **Commit:** A snapshot of your work at a point in time (like a save point)
- **Branch:** A parallel version of the code where you can make changes
- **Remote:** The version of the repository on GitHub (in the cloud)
- **Local:** The version of the repository on your computer

### The Standard Workflow

Here's the typical workflow for contributing:

```
1. Pull latest changes from main
2. Create a new branch for your work
3. Make changes to files
4. Stage and commit your changes
5. Push your branch to GitHub
6. Open a Pull Request for review
7. Address feedback
8. Merge to main
```

Let's walk through each step.

### Step 1: Pull Latest Changes

Before starting new work, make sure you have the latest version:

**Command Line:**
```bash
git checkout main
git pull origin main
```

**GitHub Desktop:**
1. Make sure you're on the "main" branch (dropdown at top)
2. Click "Fetch origin"
3. Click "Pull origin" if there are updates

### Step 2: Create a New Branch

Always work on a branch, never directly on `main`. This keeps the main branch stable and makes code review easier.

**Naming convention:** Use descriptive names like:
- `add-ruraldefinitions-package`
- `blog-post-broadband-analysis`
- `update-fcc-dataset`
- `fix-navigation-bug`

**Command Line:**
```bash
git checkout -b add-ruraldefinitions-package
```

**GitHub Desktop:**
1. Click "Current Branch" dropdown
2. Click "New Branch"
3. Enter branch name
4. Click "Create Branch"

### Step 3: Make Changes

Edit files in your text editor or RStudio. Add new files. Delete old files. Do your work!

### Step 4: Stage and Commit Changes

Once you've made changes, you need to save them in Git.

**Command Line:**
```bash
# See what changed
git status

# Stage specific files
git add _packages/ruraldefinitions.md

# Or stage all changes
git add .

# Commit with a message
git commit -m "Add ruraldefinitions package documentation"
```

**GitHub Desktop:**
1. You'll see changed files in the left sidebar
2. Check the boxes next to files you want to commit
3. Write a commit message in the bottom-left
4. Click "Commit to [your-branch-name]"

**Writing good commit messages:**
- Start with a verb: "Add", "Update", "Fix", "Remove"
- Be specific: "Add ruraldefinitions package" not "Update files"
- Keep it under 50 characters if possible

### Step 5: Push Your Branch

Send your local changes to GitHub:

**Command Line:**
```bash
git push origin add-ruraldefinitions-package

# First time pushing a new branch, use:
git push -u origin add-ruraldefinitions-package
```

**GitHub Desktop:**
1. Click "Push origin" button at the top
2. If it's a new branch, it will say "Publish branch"

### Step 6: Open a Pull Request

A Pull Request (PR) is how you ask to merge your changes into the main branch.

**On GitHub.com:**
1. Go to https://github.com/ruralinnovation/blog
2. You'll see a banner: "Your branch is ahead of main" with a "Compare & pull request" button
3. Click that button
4. Fill in the PR template:
   - **Title:** Brief description (e.g., "Add ruraldefinitions package documentation")
   - **Description:** What you changed and why
   - **Reviewers:** Tag team members who should review
5. Click "Create pull request"

**GitHub Desktop:**
1. After pushing, you'll see "Create Pull Request" button
2. Click it to open GitHub in your browser
3. Follow the steps above

### Step 7: Address Feedback

Reviewers may request changes. To update your PR:

1. Make the requested changes in your local files
2. Commit the changes (same as Step 4)
3. Push again (same as Step 5)
4. The PR automatically updates!

### Step 8: Merge

Once approved, a maintainer will merge your PR. Your changes are now live!

After merging:
```bash
# Switch back to main
git checkout main

# Pull the latest (includes your changes)
git pull origin main

# Delete your old branch locally
git branch -d add-ruraldefinitions-package
```

### Common Git Commands Reference

| Command | What It Does |
|---------|-------------|
| `git status` | Show what's changed |
| `git log` | Show commit history |
| `git diff` | Show detailed changes |
| `git branch` | List all branches |
| `git checkout main` | Switch to main branch |
| `git checkout -b new-branch` | Create and switch to new branch |
| `git add .` | Stage all changes |
| `git commit -m "message"` | Commit with message |
| `git push` | Push to remote |
| `git pull` | Pull from remote |

---

## Markdown and YAML Syntax

Most content in the blog is written in Markdown, with YAML metadata at the top.

### Markdown Basics

Markdown is a simple way to format text that converts to HTML.

#### Headers
```markdown
# Heading 1
## Heading 2
### Heading 3
#### Heading 4
```

#### Text Formatting
```markdown
**bold text**
*italic text*
***bold and italic***
~~strikethrough~~
```

#### Lists

Unordered:
```markdown
- Item 1
- Item 2
  - Nested item
  - Another nested item
- Item 3
```

Ordered:
```markdown
1. First item
2. Second item
3. Third item
```

#### Links
```markdown
[Link text](https://example.com)
[Link to another page](/datasets/fcc-broadband/)
```

#### Images
```markdown
![Alt text](path/to/image.png)
![Rural map](../assets/images/rural-map.png)
```

#### Code

Inline code:
```markdown
Use the `get_definition()` function
```

Code blocks:
````markdown
```r
library(ruraldefinitions)
data <- get_definition("cori", 2020)
```
````

#### Tables
```markdown
| Column 1 | Column 2 | Column 3 |
|----------|----------|----------|
| Data 1   | Data 2   | Data 3   |
| Data 4   | Data 5   | Data 6   |
```

#### Blockquotes
```markdown
> This is a quoted text.
> It can span multiple lines.
```

### YAML Front Matter

YAML is the metadata at the top of every markdown file, enclosed by `---`:

```yaml
---
title: "My Document Title"
description: "A brief description"
date: 2026-02-02
categories: ["Category1", "Category2"]
tags: ["tag1", "tag2", "tag3"]
---
```

**YAML syntax rules:**
- Must start and end with `---`
- Key-value pairs: `key: value`
- Strings with special characters need quotes: `"Title: With Colon"`
- Lists use brackets: `["item1", "item2"]`
- Dates format: `YYYY-MM-DD`
- Indentation matters (use 2 spaces)

**Common fields:**

| Field | Purpose | Example |
|-------|---------|---------|
| `title` | Page title | `"Rural Broadband Analysis"` |
| `description` | Short summary | `"Analyzing FCC broadband data"` |
| `date` | Publication date | `2026-02-02` |
| `categories` | Broad categories | `["Broadband", "Analysis"]` |
| `tags` | Specific keywords | `["FCC", "RUCA", "Maps"]` |
| `featured` | Show on homepage | `true` or `false` |

---

## Contributing Content: Step-by-Step Workflows

Now let's walk through how to add different types of content.

### Workflow 1: Adding an R Package

This is a complete walkthrough using the **ruraldefinitions** package as a real example.

#### Step 1: Gather Package Information

Before creating the documentation, collect:
- Package name
- Description (one sentence)
- GitHub URL
- Installation command
- Current version
- Maintainer name
- Key functions
- Main use cases

For ruraldefinitions, we gathered this from:
- `DESCRIPTION` file in the package repository
- `README.md` file
- Function documentation

#### Step 2: Create a New Branch

```bash
git checkout main
git pull origin main
git checkout -b add-ruraldefinitions-package
```

#### Step 3: Create the Package File

Create a new markdown file in `_packages/` named after the package:

```bash
touch _packages/ruraldefinitions.md
```

Or create it in your text editor: `_packages/ruraldefinitions.md`

#### Step 4: Add YAML Front Matter

Start with the metadata:

```yaml
---
title: "ruraldefinitions"
description: "R package for accessing federal rural definitions at multiple geographic levels"
date: 2026-02-02
categories: ["R Package", "Rural Classification"]
tags: ["USDA", "Census", "RUCA", "Rural Definitions", "Geography"]
featured: true
packageName: "ruraldefinitions"
githubUrl: "https://github.com/ruralinnovation/ruraldefinitions"
installCommand: 'devtools::install_github("ruralinnovation/ruraldefinitions")'
status: "stable"
version: "0.1.0"
maintainer: "Camden Blatchly"
---
```

**Package-specific fields:**
- `packageName`: Exact package name (for linking)
- `githubUrl`: Link to GitHub repository
- `installCommand`: R command to install
- `status`: "stable", "development", or "archived"
- `version`: Current version number
- `maintainer`: Package maintainer name

#### Step 5: Write the Content

After the front matter, write the package documentation:

```markdown
## Overview

Brief description of what the package does and why it's useful.

## Installation

Installation instructions with code block.

## Quick Start

A simple example showing basic usage.

## Key Functions

List of main functions with brief descriptions.

## Use Cases

Real-world applications or projects using this package.

## Documentation

Links to detailed documentation, vignettes, etc.
```

**See the complete example:** `_packages/ruraldefinitions.md` in the repository.

#### Step 6: Test Locally

Build the site to see your changes:

```bash
bundle exec jekyll serve
```

Open http://localhost:4000 and navigate to the Packages section. Verify:
- Package appears in the list
- All information displays correctly
- Links work
- Code formatting looks good

#### Step 7: Commit and Push

```bash
git add _packages/ruraldefinitions.md
git commit -m "Add ruraldefinitions package documentation"
git push -u origin add-ruraldefinitions-package
```

#### Step 8: Open Pull Request

1. Go to GitHub.com
2. Open a Pull Request from your branch to `main`
3. Fill in the PR description
4. Request reviews from team members
5. Wait for approval and merge

**Congratulations!** You've added an R package to the blog.

---

### Workflow 2: Adding a Dataset

Datasets are documented in `_datasets/` and follow a similar process.

#### Step 1: Create Dataset File

```bash
git checkout -b add-dataset-name
touch _datasets/dataset-name.md
```

#### Step 2: Add Front Matter

```yaml
---
title: "FCC National Broadband Map"
description: "Comprehensive broadband availability data from the FCC"
date: 2026-02-02
categories: ["Broadband", "Federal Data"]
tags: ["FCC", "Infrastructure", "Coverage"]
featured: true
datasetName: "FCC National Broadband Map"
source: "Federal Communications Commission"
sourceUrl: "https://broadbandmap.fcc.gov/"
accessMethod: "API and Bulk Download"
updateFrequency: "Biannual"
geographicLevel: "Location-level (addresses)"
dataFormat: ["CSV", "Parquet", "API"]
---
```

**Dataset-specific fields:**
- `datasetName`: Official dataset name
- `source`: Organization that produces it
- `sourceUrl`: Where to access it
- `accessMethod`: How to get the data
- `updateFrequency`: How often it updates
- `geographicLevel`: Geographic granularity
- `dataFormat`: Available formats

#### Step 3: Write Content

```markdown
## Overview

What the dataset contains and why it's important.

## Access

How to download or access the data.

## Structure

Description of the data structure, fields, and formats.

## Use Cases

Projects or analyses that use this dataset.

## Related Resources

Links to R packages, blog posts, or documentation.
```

#### Step 4: Follow Same Process

Test locally, commit, push, and open PR (same as package workflow).

---

### Workflow 3: Writing a Blog Post with Quarto

Blog posts are special because they can include R code that executes.

#### Step 1: Understand the Posts Folder Structure

Blog posts live in numbered folders inside `posts/`:

```
posts/
├── 01_awesomejq/
│   ├── index.qmd
│   └── (any images or data files)
├── 02_mapping_rural_tips/
│   ├── index.qmd
│   └── figures/
└── 17_your_new_post/
    └── index.qmd
```

**Naming convention:** `[number]_[short-slug]/index.qmd`

#### Step 2: Create New Post Folder

```bash
git checkout -b blog-post-broadband-equity

# Find the next number (if last post is 16_, use 17_)
mkdir posts/17_broadband_equity_analysis
cd posts/17_broadband_equity_analysis
touch index.qmd
```

#### Step 3: Create QMD File

Open `posts/17_broadband_equity_analysis/index.qmd` in RStudio.

#### Step 4: Add YAML Header

Quarto QMD files use YAML at the top:

```yaml
---
title: "Analyzing Broadband Equity Across Rural America"
description: "Using FCC data to identify broadband gaps in rural counties"
author: "Your Name"
date: "2026-02-02"
categories: ["Broadband", "Equity", "Analysis"]
tags: ["FCC", "Rural", "Data Visualization"]
image: "featured-image.png"
execute:
  echo: true
  warning: false
  message: false
---
```

**Blog post-specific fields:**
- `author`: Your name
- `image`: Featured image filename (optional, in same folder)
- `execute`: Controls how R code runs
  - `echo: true` - Show code in output
  - `warning: false` - Hide R warnings
  - `message: false` - Hide R messages

#### Step 5: Write Your Content

Mix markdown text with R code chunks:

````markdown
## Introduction

Brief overview of what you're analyzing.

## Load Libraries

```{r}
library(tidyverse)
library(cori.data.fcc)
library(ruraldefinitions)
```

## Load Data

```{r}
# Get broadband data for Vermont
vt_broadband <- get_broadband_data(state = "VT")

# Get rural definitions
rural_def <- get_definition("cori", 2020)
```

## Analysis

Describe what you're doing:

```{r}
# Calculate coverage by rural classification
coverage_by_rural <- vt_broadband %>%
  left_join(rural_def, by = "geoid") %>%
  group_by(is_rural) %>%
  summarize(
    avg_download_speed = mean(download_speed, na.rm = TRUE),
    coverage_pct = mean(is_covered) * 100
  )

print(coverage_by_rural)
```

## Visualization

```{r}
#| fig-width: 8
#| fig-height: 6
#| fig-cap: "Broadband coverage by rural classification"

ggplot(coverage_by_rural, aes(x = is_rural, y = coverage_pct)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Broadband Coverage: Rural vs. Urban",
    x = "Rural Classification",
    y = "Coverage (%)"
  ) +
  theme_minimal()
```

## Conclusions

Summarize your findings.
````

**Code chunk options:**

Add options after `{r}` using `#|` syntax:
```r
{r}
#| echo: false       # Don't show code
#| eval: false       # Don't run code
#| fig-width: 8      # Figure width in inches
#| fig-height: 6     # Figure height in inches
#| fig-cap: "..."    # Figure caption
#| warning: false    # Hide warnings
```

#### Step 6: Render Locally

**In RStudio:**
- Click the "Render" button at the top of the editor
- Or press Cmd+Shift+K (Mac) / Ctrl+Shift+K (Windows)

**In Terminal:**
```bash
quarto render posts/17_broadband_equity_analysis/index.qmd
```

This runs your R code and generates markdown output in `_stories/`.

#### Step 7: Build Full Site

After rendering your post, build the Jekyll site:

```bash
bundle exec jekyll serve
```

Visit http://localhost:4000 to see your blog post.

#### Step 8: Commit Everything

```bash
# Add your source QMD file
git add posts/17_broadband_equity_analysis/

# Add generated markdown (important!)
git add _stories/broadband_equity_analysis.md

# Commit
git commit -m "Add blog post: Analyzing Broadband Equity"

# Push
git push -u origin blog-post-broadband-equity
```

**Important:** Commit both the source `.qmd` file AND the generated `_stories/*.md` file.

#### Step 9: Open PR

Follow the same PR process as before.

---

### Workflow 4: Modifying Site Configuration

Sometimes you need to change site-wide settings like navigation menus or the site title.

#### Step 1: Identify What to Change

| What You Want to Change | File to Edit |
|------------------------|-------------|
| Site title, description | `_config.yml` |
| Navigation menu items | `_config.yml` |
| Blog rendering settings | `_quarto.yml` |
| CSS styling | `src/custom.scss` |

#### Step 2: Edit Configuration File

**Example: Adding a navigation menu item**

Edit `_config.yml`:

```yaml
header_pages:
  - index.md
  - packages.md
  - datasets.md
  - projects.md
  - blog.md
  - resources.md
  - about.md           # New page
```

**Example: Changing site title**

```yaml
title: "CORI Rural Dataverse"
description: "Data, tools, and analysis for rural innovation"
```

#### Step 3: Restart Server

Configuration changes require restarting the development server:

```bash
# Stop server: Ctrl+C

# Start again:
bundle exec jekyll serve
```

#### Step 4: Commit and Push

```bash
git checkout -b update-site-navigation
git add _config.yml
git commit -m "Add About page to navigation"
git push -u origin update-site-navigation
```

Open PR as usual.

---

## R Code Blocks and Quarto Features

Quarto offers powerful features for including R code and visualizations in blog posts.

### Basic Code Chunks

The simplest code chunk:

````markdown
```{r}
library(tidyverse)
data <- read_csv("data.csv")
summary(data)
```
````

### Chunk Options

Control how code executes and displays:

````markdown
```{r}
#| label: load-data
#| echo: false
#| message: false
#| warning: false
#| cache: true

library(tidyverse)
data <- read_csv("large_dataset.csv")
```
````

**Common options:**

| Option | Values | Effect |
|--------|--------|--------|
| `echo` | `true`/`false` | Show/hide code |
| `eval` | `true`/`false` | Run/don't run code |
| `message` | `true`/`false` | Show/hide messages |
| `warning` | `true`/`false` | Show/hide warnings |
| `error` | `true`/`false` | Show/hide errors |
| `include` | `true`/`false` | Include chunk output at all |
| `cache` | `true`/`false` | Cache results for speed |
| `label` | string | Name for the chunk |

### Figure Options

Control plot appearance:

````markdown
```{r}
#| label: fig-coverage-map
#| fig-width: 10
#| fig-height: 8
#| fig-cap: "Broadband coverage across Vermont"
#| fig-alt: "Map showing broadband coverage percentages by county in Vermont"

plot_coverage_map(vt_data)
```
````

**Figure options:**

| Option | Purpose |
|--------|---------|
| `fig-width` | Width in inches |
| `fig-height` | Height in inches |
| `fig-cap` | Figure caption |
| `fig-alt` | Alt text for accessibility |
| `fig-align` | `left`, `center`, `right` |
| `out-width` | Output width (e.g., "100%") |

### Tables

Display data frames as formatted tables:

````markdown
```{r}
#| label: tbl-summary
#| tbl-cap: "Summary statistics by rural classification"

summary_table <- data %>%
  group_by(rural_classification) %>%
  summarize(
    n = n(),
    mean_speed = mean(download_speed),
    median_speed = median(download_speed)
  )

knitr::kable(summary_table)
```
````

Or use the `gt` package for fancy tables:

````markdown
```{r}
library(gt)

summary_table %>%
  gt() %>%
  tab_header(
    title = "Broadband Speeds by Rural Classification",
    subtitle = "Analysis of FCC data"
  ) %>%
  fmt_number(
    columns = c(mean_speed, median_speed),
    decimals = 1
  )
```
````

### Inline Code

Include R results in text:

```markdown
The average download speed in rural areas is `r mean(rural_data$speed)` Mbps.
```

This executes R code and inserts the result directly in the text.

### Code Folding

Allow readers to show/hide code:

````markdown
```{r}
#| code-fold: true
#| code-summary: "Show the code"

# Complex data processing
result <- data %>%
  filter(year == 2023) %>%
  group_by(county) %>%
  summarize(avg = mean(value))
```
````

Readers see a "Show the code" button they can click to view the code.

### Multiple Outputs

One code chunk can produce multiple outputs:

````markdown
```{r}
# Print summary
print(summary(data))

# Create plot
ggplot(data, aes(x = variable)) +
  geom_histogram()

# Print table
kable(head(data))
```
````

### Panel Layout

Display multiple figures side-by-side:

````markdown
::: {layout-ncol=2}

```{r}
#| echo: false
plot1
```

```{r}
#| echo: false
plot2
```

:::
````

### Citations

Add references to your posts:

1. Create a `references.bib` file in your post folder
2. Add to YAML header:
```yaml
bibliography: references.bib
```
3. Cite in text: `[@citation-key]`
4. References section auto-generates at the end

---

## Testing Before Publishing

Always test your changes locally before opening a PR.

### Building the Site Locally

**Full build:**
```bash
bundle exec jekyll serve
```

**Quick build (skip drafts):**
```bash
bundle exec jekyll serve --skip-initial-build
```

**Build with incremental regeneration:**
```bash
bundle exec jekyll serve --incremental
```

### What to Check

Before committing, verify:

- [ ] Content displays correctly
- [ ] No broken links
- [ ] Images load properly
- [ ] Code blocks are formatted correctly
- [ ] R code executes without errors
- [ ] Plots and figures render
- [ ] Navigation works
- [ ] Mobile view looks good (resize browser)
- [ ] No Jekyll build errors in terminal

### Common Preview Issues

**Issue: "Changes don't appear"**

Solutions:
1. Refresh browser (Cmd+R / Ctrl+R)
2. Hard refresh (Cmd+Shift+R / Ctrl+Shift+F5)
3. Stop and restart Jekyll server (Ctrl+C, then re-run)
4. Clear browser cache
5. Check if you edited a generated file by mistake

**Issue: "R code doesn't execute"**

Solutions:
1. Render the Quarto document first: `quarto render posts/XX_post/index.qmd`
2. Check for R errors in the terminal
3. Make sure required packages are installed
4. Verify working directory is correct

**Issue: "Jekyll build fails"**

Solutions:
1. Read the error message carefully
2. Common causes:
   - Invalid YAML syntax
   - Missing closing quotes
   - Incorrect indentation
   - Liquid template errors
3. Run `bundle exec jekyll build --verbose` for detailed errors

**Issue: "Page returns 404"**

Solutions:
1. Check the file is in the correct collection folder
2. Verify YAML front matter is present and valid
3. Make sure file has `.md` extension
4. Restart Jekyll server

### Testing on Different Browsers

Test in multiple browsers to ensure compatibility:
- Chrome/Edge
- Firefox
- Safari (Mac)

### Accessibility Check

Basic accessibility checks:
- All images have alt text
- Headings are in logical order (h1, h2, h3)
- Links have descriptive text (not "click here")
- Color contrast is sufficient
- Tables have headers

---

## Collaboration and Code Review

The team uses Pull Requests for code review before merging changes.

### The PR Process

```
Your work → Push to branch → Open PR → Code review → Address feedback → Merge
```

### Opening a Good PR

**PR Title:** Clear and concise
- ✅ "Add ruraldefinitions package documentation"
- ✅ "Fix broken links in FCC dataset page"
- ❌ "Updates"
- ❌ "Changes to files"

**PR Description:** Include:
1. **What:** What you changed
2. **Why:** Why you made the change
3. **How to test:** How reviewers can verify
4. **Screenshots:** (if visual changes)
5. **Checklist:**
   - [ ] Tested locally
   - [ ] No console errors
   - [ ] Links work
   - [ ] Mobile-friendly

**Example:**

```markdown
## What

Added documentation for the ruraldefinitions R package to the packages collection.

## Why

The ruraldefinitions package is now stable (v0.1.0) and being used in multiple projects. We need it documented on the blog for discoverability.

## Changes

- Created `_packages/ruraldefinitions.md` with complete package documentation
- Includes installation instructions, quick start guide, and table of available definitions
- Links to GitHub repository and related resources

## How to Test

1. Pull this branch
2. Run `bundle exec jekyll serve`
3. Navigate to Packages section
4. Verify ruraldefinitions appears and all content displays correctly

## Screenshots

(Include screenshot of the package page)

## Checklist

- [x] Tested locally
- [x] No build errors
- [x] All links verified
- [x] Follows existing package page format
```

### Requesting Reviews

Tag team members who should review:
- **Content expert:** Someone familiar with the subject matter
- **Technical reviewer:** Someone who can check code/configuration
- **Documentation lead:** For large content additions

In GitHub, use the "Reviewers" section on the right side of the PR.

### Responding to Review Feedback

When reviewers request changes:

1. **Read carefully:** Understand what's being asked
2. **Ask questions:** If unclear, ask for clarification
3. **Make changes:** Update your local files
4. **Commit and push:** PR updates automatically
5. **Respond:** Comment on each review item
   - "Fixed in [commit-hash]"
   - "Good catch, updated"
   - "I kept it as-is because [reason], let me know if you disagree"

### Common Review Comments

**"Can you add a description?"**
→ Add or expand the description in front matter or text

**"This link is broken"**
→ Fix the URL or path

**"Code formatting is off"**
→ Check syntax highlighting, indentation, or use proper code blocks

**"Needs more context"**
→ Add explanation of what the code does or why it matters

**"LGTM" (Looks Good To Me)**
→ Approval! 🎉

### Merge Conflicts

If someone else merged to `main` while you were working, you may get conflicts.

**Resolve conflicts:**

```bash
# Update your local main
git checkout main
git pull origin main

# Switch to your branch
git checkout your-branch-name

# Merge main into your branch
git merge main

# If conflicts occur, Git will tell you which files
# Open those files and look for:
<<<<<<< HEAD
Your changes
=======
Their changes
>>>>>>> main

# Edit to keep the correct version
# Remove the conflict markers
# Save the file

# Stage resolved files
git add conflicted-file.md

# Complete the merge
git commit -m "Resolve merge conflicts"

# Push
git push
```

**In GitHub Desktop:**
1. GitHub Desktop will show conflicts
2. Click "Open in [your editor]"
3. Resolve conflicts in your editor
4. Save files
5. Return to GitHub Desktop
6. Click "Commit merge"

### After Merge

Once your PR is merged:

1. **Celebrate!** 🎉 Your contribution is live
2. **Pull latest main:**
   ```bash
   git checkout main
   git pull origin main
   ```
3. **Delete your branch:**
   ```bash
   git branch -d your-branch-name
   ```
4. **Check the live site** (if auto-deployed)

---

## Common Gotchas and Troubleshooting

Here are issues contributors commonly encounter and how to fix them.

### Build Errors and Dependency Issues

**Error: "Could not find gem"**

```bash
# Reinstall dependencies
bundle install

# If that doesn't work, update bundler
gem install bundler
bundle update
```

**Error: "Liquid Exception"**

Usually a template syntax error. Check for:
- Unescaped special characters in YAML
- Missing quotes around strings with colons
- Unclosed Liquid tags

**Error: "Quarto command not found"**

Install Quarto:
- Download from https://quarto.org/docs/get-started/
- Or via Homebrew: `brew install quarto`

**Error: "Cannot load such file -- webrick"**

```bash
bundle add webrick
```

### File Path and Naming Confusion

**Problem: "My package doesn't appear"**

Check:
- ✅ File is in `_packages/` folder
- ✅ File has `.md` extension
- ✅ YAML front matter is present and valid
- ✅ You restarted Jekyll server

**Problem: "Blog post not rendering"**

Check:
- ✅ Source file is in `posts/XX_slug/index.qmd`
- ✅ You ran `quarto render` on the QMD file
- ✅ Generated markdown appeared in `_stories/`
- ✅ Both source and generated files committed

**Problem: "Image not showing"**

Check:
- ✅ Image is in the correct folder (same as post, or `/assets/images/`)
- ✅ Path in markdown is correct (use relative paths)
- ✅ Image file name matches exactly (case-sensitive)
- ✅ Image file committed to Git

### Preview Not Showing Changes

**Changes don't appear in browser:**

1. Hard refresh: Cmd+Shift+R (Mac) / Ctrl+Shift+F5 (Windows)
2. Restart Jekyll: Ctrl+C, then `bundle exec jekyll serve`
3. Clear browser cache
4. Try incognito/private window

**Config changes don't appear:**

You MUST restart Jekyll after editing `_config.yml` or `_quarto.yml`.

**Quarto content doesn't update:**

1. Re-render the QMD: `quarto render posts/XX_post/index.qmd`
2. Restart Jekyll server
3. Hard refresh browser

### Git Merge Conflicts and Sync Issues

**Error: "Your branch is behind 'origin/main'"**

```bash
git pull origin main
```

**Error: "Your local changes would be overwritten by merge"**

```bash
# Stash your changes temporarily
git stash

# Pull latest
git pull origin main

# Reapply your changes
git stash pop

# Resolve any conflicts, then commit
```

**Error: "Failed to push"**

```bash
# Pull first
git pull origin your-branch-name

# Then push
git push
```

**Merge conflict in generated file:**

If `_stories/post.md` or `docs/` has conflicts:
1. Accept either version (doesn't matter)
2. Re-generate: `quarto render` or `bundle exec jekyll build`
3. Commit the new generated version

### R Code Errors

**Error: "Package 'xyz' not found"**

```r
install.packages("xyz")
# Or for GitHub packages:
devtools::install_github("user/package")
```

**Error: "Object not found"**

Check:
- Is the variable defined in an earlier chunk?
- Are chunks in the right order?
- Did you load required libraries?

**Error: "Cannot open file 'data.csv'"**

Check:
- File path is relative to the QMD file
- File exists and is committed
- File name matches exactly (case-sensitive)

**Plots don't render:**

```{r}
#| eval: true
#| echo: true
#| warning: false
#| fig-width: 8
#| fig-height: 6

print(your_plot)  # Sometimes explicitly printing helps
```

### Jekyll-Specific Issues

**Error: "Conversion error"**

Usually a markdown syntax issue. Check:
- Code blocks are properly fenced (```)
- No unescaped special characters
- Tables have correct syntax

**Error: "Liquid syntax error"**

You may have text that looks like Liquid template code. Escape it:

```markdown
{% raw %}
{{ this will be displayed literally }}
{% endraw %}
```

**Error: "Permlink issue"**

Check front matter for `permalink` field. Usually not needed for collections.

### Performance Issues

**Build is very slow:**

- Use `--incremental` flag: `bundle exec jekyll serve --incremental`
- Limit posts: `bundle exec jekyll serve --limit_posts 10`
- Disable rendering for all posts: temporarily move `posts/` folder

**Quarto rendering is slow:**

- Cache results: Add `cache: true` to chunk options
- Reduce data size: Use samples for testing
- Skip evaluation: `eval: false` for chunks you're not testing

### Getting Help

If you're stuck:

1. **Check the error message** carefully - it usually tells you what's wrong
2. **Search the docs:**
   - Jekyll: https://jekyllrb.com/docs/
   - Quarto: https://quarto.org/docs/guide/
3. **Ask the team:** Post in the team Slack/chat
4. **Check existing issues:** Look at closed PRs for similar problems
5. **Create an issue:** If it's a bug, open a GitHub issue

---

## Quick Reference

### Essential Commands

```bash
# Git workflow
git checkout main
git pull origin main
git checkout -b new-branch
git add .
git commit -m "message"
git push -u origin new-branch

# Build site
bundle exec jekyll serve

# Render Quarto post
quarto render posts/XX_post/index.qmd

# Install Ruby dependencies
bundle install

# Update dependencies
bundle update
```

### File Locations

| Content Type | Source Folder | Generated Folder |
|-------------|---------------|------------------|
| R Packages | `_packages/` | - |
| Datasets | `_datasets/` | - |
| Projects | `_projects/` | - |
| Resources | `_resources/` | - |
| Blog Posts | `posts/` | `_stories/` |
| Website | - | `docs/` (production) |
| Website | - | `_site/` (development) |

### Front Matter Templates

**Package:**
```yaml
---
title: "package-name"
description: "Brief description"
date: YYYY-MM-DD
categories: ["R Package", "Category"]
tags: ["tag1", "tag2"]
featured: true
packageName: "package-name"
githubUrl: "https://github.com/org/repo"
installCommand: 'install command here'
status: "stable"
version: "0.1.0"
maintainer: "Name"
---
```

**Dataset:**
```yaml
---
title: "Dataset Name"
description: "Brief description"
date: YYYY-MM-DD
categories: ["Category1", "Category2"]
tags: ["tag1", "tag2"]
featured: true
datasetName: "Official Name"
source: "Organization"
sourceUrl: "https://example.com"
accessMethod: "API/Download"
updateFrequency: "Annual/Monthly/etc"
geographicLevel: "County/Tract/etc"
dataFormat: ["CSV", "API"]
---
```

**Blog Post:**
```yaml
---
title: "Post Title"
description: "Brief description"
author: "Your Name"
date: "YYYY-MM-DD"
categories: ["Category1", "Category2"]
tags: ["tag1", "tag2"]
image: "featured.png"
execute:
  echo: true
  warning: false
  message: false
---
```

### Markdown Syntax

```markdown
# Heading 1
## Heading 2
### Heading 3

**bold**
*italic*
[link](url)
![image](path)

- List item
- List item

1. Numbered item
2. Numbered item

`inline code`

```language
code block
```

| Table | Header |
|-------|--------|
| Cell  | Cell   |
```

### Quarto Code Chunk Options

```r
{r}
#| label: chunk-name
#| echo: true/false
#| eval: true/false
#| message: true/false
#| warning: true/false
#| fig-width: 8
#| fig-height: 6
#| fig-cap: "Caption"
#| cache: true/false
```

### Common URLs

- **Blog repository:** https://github.com/ruralinnovation/blog
- **Live site:** https://ruralinnovation.github.io/blog/
- **Jekyll docs:** https://jekyllrb.com/docs/
- **Quarto docs:** https://quarto.org/docs/guide/
- **Markdown guide:** https://www.markdownguide.org/

---

## Conclusion

Congratulations! You now have a comprehensive understanding of how to contribute to the Rural Dataverse blog.

**Remember:**
1. **Concepts first:** Understand Jekyll + Quarto before diving in
2. **Test locally:** Always preview changes before opening a PR
3. **Git workflow:** Branch → Commit → Push → PR → Review → Merge
4. **Ask for help:** The team is here to support you

**Next steps:**
1. Set up your development environment following [Prerequisites and Setup](#prerequisites-and-setup)
2. Clone the repository
3. Try adding a simple package or dataset using the workflows
4. Read existing content for examples
5. Open your first PR!

Happy contributing! 🎉

---

**Document Feedback:**

If you find errors, have suggestions, or encounter situations not covered in this guide, please:
- Open an issue: https://github.com/ruralinnovation/blog/issues
- Or submit a PR to improve this document

This is a living document that grows with the team's needs.
