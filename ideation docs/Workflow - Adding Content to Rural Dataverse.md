# Workflow: Adding Content to Rural Dataverse

**Last Updated:** January 31, 2026

---

## 🎯 Quick Overview

```
Create File → Edit Content → Preview Locally → Deploy to GitHub Pages
   (2 min)      (10-30 min)      (instant)          (1-2 min)
```

Total time: **~15-35 minutes** from start to live

---

## 📊 Visual Workflow

```
┌─────────────────────────────────────────────────────────────────────┐
│                     ADDING CONTENT WORKFLOW                          │
└─────────────────────────────────────────────────────────────────────┘

STEP 1: Choose Content Type
┌──────────────────────────────────────────────────────────┐
│  Dataset  │  R Package  │  Project  │  Blog  │  Resource │
│ _datasets │ _packages   │ _projects │_stories│_resources │
└──────────────────────────────────────────────────────────┘
                            ↓

STEP 2: Create/Copy Template File
┌──────────────────────────────────────────────────────────┐
│  Option A: Copy existing example                         │
│    cp _packages/cori-fcc.md _packages/new-package.md    │
│                                                          │
│  Option B: Create new file from scratch                 │
│    touch _packages/new-package.md                       │
└──────────────────────────────────────────────────────────┘
                            ↓

STEP 3: Edit Content
┌──────────────────────────────────────────────────────────┐
│  Open in your editor:                                    │
│    - VS Code: code _packages/new-package.md             │
│    - Terminal: nano _packages/new-package.md            │
│                                                          │
│  Fill in frontmatter (YAML) and markdown content        │
└──────────────────────────────────────────────────────────┘
                            ↓

STEP 4: Preview Locally (Optional but Recommended)
┌──────────────────────────────────────────────────────────┐
│  Start dev server:                                       │
│    bundle exec jekyll serve                              │
│                                                          │
│  View in browser:                                        │
│    http://localhost:4000/blog/packages/new-package/      │
│                                                          │
│  Make changes → Save → Refresh browser (auto-reload!)   │
└──────────────────────────────────────────────────────────┘
                            ↓

STEP 5: Build for Production
┌──────────────────────────────────────────────────────────┐
│  Generate static site:                                   │
│    bundle exec jekyll build                              │
│                                                          │
│  Output: docs/ folder (ready for GitHub Pages)          │
└──────────────────────────────────────────────────────────┘
                            ↓

STEP 6: Commit & Deploy
┌──────────────────────────────────────────────────────────┐
│  git add _packages/new-package.md docs/                 │
│  git commit -m "Add new-package documentation"          │
│  git push origin dev/dataverse                          │
│                                                          │
│  GitHub Pages auto-deploys in 1-2 minutes              │
└──────────────────────────────────────────────────────────┘
                            ↓

✅ LIVE! https://ruralinnovation.github.io/blog/packages/new-package/
```

---

## 📝 Detailed Step-by-Step Guide

### STEP 1: Choose Your Content Type

Decide what you're adding:

| Type | Folder | Example |
|------|--------|---------|
| **Dataset** | `_datasets/` | FCC Broadband Map, Census data |
| **R Package** | `_packages/` | cori.fcc, cori.data.bds |
| **Project** | `_projects/` | Broadband Equity Analysis |
| **Blog Post** | `_stories/` | Technical analysis, methods |
| **Resource** | `_resources/` | Geographic crosswalks, guides |

---

### STEP 2: Create Your File

#### Option A: Copy an Existing Example (Fastest)

```bash
# Navigate to project
cd ~/Documents/GitHub/blog

# Copy example and rename
cp _packages/cori-data-bds.md _packages/my-new-package.md
```

#### Option B: Create from Scratch

```bash
# Create new file
touch _packages/my-new-package.md

# Open in editor
code _packages/my-new-package.md  # VS Code
# or
nano _packages/my-new-package.md  # Terminal editor
```

---

### STEP 3: Edit Content

Every content file has two parts:

#### Part 1: Frontmatter (YAML) - The Metadata

```yaml
---
title: "Package Name"
description: "Brief description"
date: 2026-01-31
categories: ["Category1", "Category2"]
tags: ["tag1", "tag2"]
featured: true  # Shows on homepage
# ... other fields specific to content type
---
```

#### Part 2: Markdown Content - The Body

```markdown
## Overview

Your content here in standard markdown...

## Installation

```r
# Code examples
```

## More sections...
```

**💡 Tip**: Look at existing examples in each folder for the exact fields to use!

---

### STEP 4: Preview Locally (Recommended)

#### Start Development Server

```bash
# Navigate to project
cd ~/Documents/GitHub/blog

# Ensure Ruby 4.0 is in PATH
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"

# Start server
bundle exec jekyll serve
```

**Output:**
```
Server address: http://127.0.0.1:4000/blog/
Server running... press ctrl-c to stop.
```

#### View Your Content

**Listing Page:**
- Datasets: http://localhost:4000/blog/datasets/
- Packages: http://localhost:4000/blog/packages/
- Projects: http://localhost:4000/blog/projects/
- Blog: http://localhost:4000/blog/stories/
- Resources: http://localhost:4000/blog/resources/

**Detail Page:**
- http://localhost:4000/blog/packages/my-new-package/

#### Make Changes

1. Edit the file and save
2. Jekyll auto-rebuilds (watch terminal)
3. Refresh browser to see changes
4. Repeat until satisfied

**Press `Ctrl+C` to stop server when done**

---

### STEP 5: Build for Production

When you're happy with your content:

```bash
# Navigate to project
cd ~/Documents/GitHub/blog

# Ensure Ruby 4.0 is in PATH
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"

# Build the site
bundle exec jekyll build
```

**What This Does:**
- Generates static HTML files in `docs/` folder
- Optimizes for production
- Ready for GitHub Pages deployment

**Output:**
```
Configuration file: /Users/.../blog/_config.yml
            Source: /Users/.../blog
       Destination: /Users/.../blog/docs
 Incremental build: disabled. Enable with --incremental
      Generating...
                    done in 2.5 seconds.
```

---

### STEP 6: Commit & Deploy

#### Git Workflow

```bash
# Check what changed
git status

# Add your new content file
git add _packages/my-new-package.md

# Add generated docs
git add docs/

# Commit with descriptive message
git commit -m "Add my-new-package R package documentation"

# Push to GitHub
git push origin dev/dataverse
```

#### GitHub Pages Auto-Deploy

- GitHub automatically detects the push
- Rebuilds site from `docs/` folder
- **Live in 1-2 minutes!**

#### Verify Deployment

Visit: https://ruralinnovation.github.io/blog/packages/my-new-package/

---

## 🚀 Quick Reference Commands

### One-Time Setup (Per Session)

```bash
cd ~/Documents/GitHub/blog
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
```

### Development

```bash
# Start dev server
bundle exec jekyll serve

# Stop dev server
# Press Ctrl+C or: pkill -f jekyll
```

### Deployment

```bash
# Build
bundle exec jekyll build

# Deploy
git add .
git commit -m "Descriptive message"
git push origin dev/dataverse
```

---

## 📋 Content Type Templates

### R Package Template

```yaml
---
title: "package.name"
description: "What the package does"
date: 2026-01-31
categories: ["R Package", "Topic"]
tags: ["tag1", "tag2"]
featured: true
packageName: "package.name"
githubUrl: "https://github.com/ruralinnovation/package.name"
installCommand: 'remotes::install_github("ruralinnovation/package.name")'
status: "stable"  # or "beta", "development"
version: "1.0.0"
maintainer: "Name"
---

## Overview
What the package does...

## Installation
```r
remotes::install_github("ruralinnovation/package.name")
```

## Quick Start
```r
library(package.name)
# Examples...
```

## Key Functions
- `function1()` - Description
- `function2()` - Description

## Documentation
- [GitHub](https://github.com/...)
- [Vignettes](https://github.com/.../vignettes)
```

### Dataset Template

```yaml
---
title: "Dataset Name"
description: "Brief description"
date: 2026-01-31
categories: ["Category1", "Category2"]
tags: ["tag1", "tag2"]
featured: true
downloadUrl: "https://link-to-data"
githubUrl: "https://github.com/..."
dataFormat: "CSV, Parquet"
updateFrequency: "Monthly"
coverage: "United States"
---

## Overview
Description of dataset...

## Key Features
- Feature 1
- Feature 2

## Usage
How to use this data...

## Download
- [Direct Download](URL)
- [S3 Bucket](URL)

## Related Resources
- [Related Package](/packages/package-name/)
```

### Project Template

```yaml
---
title: "Project Name"
description: "Project description"
date: 2026-01-31
categories: ["Category"]
tags: ["tag1", "tag2"]
featured: true
projectUrl: "https://project-site.com"
status: "active"  # or "completed"
team: ["person1", "person2"]
usesDatasets: ["dataset-slug"]
usesPackages: ["package-slug"]
---

## Overview
What this project does...

## Methodology
How we did it...

## Key Findings
Results...

## Data & Tools Used
**Datasets:**
- [Dataset Name](/datasets/dataset-slug/)

**R Packages:**
- [package.name](/packages/package-slug/)

## Outputs
- [Dashboard](URL)
- [Report](URL)
```

### Blog Post Template (Markdown)

```yaml
---
title: "Post Title"
description: "Brief description"
date: 2026-01-31
author: "Your Name"
categories: ["Category"]
tags: ["tag1", "tag2"]
featured: true
usesDatasets: ["dataset-slug"]  # Optional
usesPackages: ["package-slug"]  # Optional
---

Your content here...
```

### Blog Post Template (Quarto)

```yaml
---
title: "Post Title"
description: "Brief description"
date: 2026-01-31
author: "Your Name"
categories: ["Category"]
tags: ["tag1", "tag2"]
featured: true
freeze: true  # Cache code execution
---

Your content with R code...

```{r}
library(tidyverse)
# Your analysis...
```
```

### Resource Template

```yaml
---
title: "Resource Name"
description: "Brief description"
date: 2026-01-31
type: "guide"  # or "tool", "publication"
categories: ["Category"]
tags: ["tag1", "tag2"]
featured: true
---

## Overview
What this resource provides...

## How to Use
Instructions...
```

---

## 💡 Pro Tips

### Speed Up Your Workflow

1. **Keep dev server running** while editing
   - Start once, leave it running
   - Auto-rebuilds on save
   - Just refresh browser

2. **Copy similar content** as template
   - Faster than starting from scratch
   - Maintains consistent formatting

3. **Test locally first** before deploying
   - Catch formatting issues early
   - Preview exactly what users will see

4. **Use featured: true** for homepage
   - Makes content visible on landing page
   - Highlights important additions

5. **Link between content**
   - Use `/packages/slug/` format
   - Creates rich interconnections
   - Helps users discover related work

### Common Gotchas

❌ **Don't forget YAML closing dashes**
```yaml
---
title: "My Title"
---
```
(Need both opening `---` and closing `---`)

❌ **Don't use spaces in filenames**
```bash
# Bad: my new package.md
# Good: my-new-package.md
```

❌ **Don't forget to build before deploying**
```bash
# Always run this before git push:
bundle exec jekyll build
```

✅ **Do use descriptive commit messages**
```bash
# Good: "Add cori.data.bds package documentation"
# Bad: "update"
```

✅ **Do test links locally**
- Click through all internal links
- Verify external links work
- Check images load

---

## 📊 Workflow Comparison

### Simple Content (e.g., Resource Guide)

```
Create file (2 min) → Edit (10 min) → Deploy (2 min)
Total: ~15 minutes
```

### Complex Content (e.g., R Package Documentation)

```
Create file (2 min) → Research & Edit (30 min) → Preview (5 min)
→ Revisions (10 min) → Deploy (2 min)
Total: ~50 minutes
```

### Blog Post with Code

```
Create .qmd (2 min) → Write & Code (1-2 hours) → Render in RStudio (1 min)
→ Preview (5 min) → Deploy (2 min)
Total: ~1-2 hours
```

---

## 🎯 Quick Decision Tree

```
What are you adding?

├─ Data source? → _datasets/
│
├─ R package? → _packages/
│
├─ Research project? → _projects/
│
├─ Technical blog post?
│   ├─ With R/Python code? → _stories/ (.qmd file)
│   └─ Just markdown? → _stories/ (.md file)
│
└─ Guide, tool, or reference? → _resources/
```

---

## 🔄 Iterative Development Workflow

Many people find this iterative approach works well:

```
Session 1 (30 min):
├─ Start dev server
├─ Create file with basic structure
├─ Fill in frontmatter
└─ Write overview section

Session 2 (30 min):
├─ Add installation/usage examples
├─ Add key features
└─ Preview and refine

Session 3 (15 min):
├─ Add documentation links
├─ Final review
├─ Build and deploy
└─ Verify live site
```

**Total**: 75 minutes spread across multiple sessions

---

## 📞 Getting Help

### Check Existing Examples

Look at similar content in the same folder:
```bash
ls _packages/        # See all package examples
cat _packages/cori-data-bds.md  # View example
```

### Documentation References

- **Development Guide**: `DEVELOPMENT.md`
- **Quick Start**: `README-JEKYLL.md`
- **Architecture Plan**: `ideation docs/Rural Dataverse - Architecture & Implementation Plan.md`
- **Session Log**: `ideation docs/Session Log - 2026-01-31 - Jekyll Setup & POC.md`

### Common Issues

**Server won't start?**
```bash
# Ensure Ruby 4.0 is in PATH
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
bundle exec jekyll serve
```

**Changes not showing?**
```bash
# Clear cache
rm -rf _site .jekyll-cache
bundle exec jekyll serve
```

**Build fails?**
```bash
# Check for YAML errors
bundle exec jekyll build --verbose
```

---

## ✅ Success Checklist

Before deploying, verify:

- [ ] Frontmatter has all required fields
- [ ] Opening and closing `---` present
- [ ] No typos in title/description
- [ ] Links work (internal and external)
- [ ] Code examples formatted correctly
- [ ] Images load (if any)
- [ ] Preview looks good locally
- [ ] Categories and tags appropriate
- [ ] Date is correct
- [ ] Built successfully: `bundle exec jekyll build`
- [ ] Committed with descriptive message
- [ ] Pushed to GitHub

---

## 🎉 You Did It!

Congratulations! You've added content to the Rural Dataverse.

**What's next?**
- Add more content (datasets, packages, projects)
- Share the link with your team
- Update content as needed (same workflow)
- Iterate and improve

**Remember**: The more content you add, the faster this workflow becomes!

---

**Last Updated:** January 31, 2026
**Contributors:** CORI MDA Team
**Questions?** Check `DEVELOPMENT.md` or ask on GitHub issues
