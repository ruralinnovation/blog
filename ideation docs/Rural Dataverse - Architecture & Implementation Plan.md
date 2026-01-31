# Rural Dataverse - Architecture & Implementation Plan

**Date:** January 31, 2026
**Prepared by:** Claude Code Analysis
**Status:** Planning Phase

---

## 🎯 Vision Summary: Rural Dataverse

Based on the team's ideation document and the NCCS reference site, the goal is to transform the current MDA blog into a comprehensive **Rural Dataverse** - a central hub that integrates data, tools, research, and storytelling for rural innovation work.

### Core Components from DevOps Ideation:

**Source/Base Elements:**
- Data sources & downloads (S3 buckets)
- R packages (BEA, FCC, Rural Definitions, Cori.charts, dform, BDS, COG, QCEW, USPTO patents)
- Geographic crosswalks with citations
- Rural definitions ontology
- Project citations & works cited
- Journals/articles/citations used in our works

**Product-Facing Content:**
- Blog posts (current MDA blog - to be integrated as "Stories")
- Applications & dashboards
- Project pages with tagged resources
- Research articles & publications
- Data visualizations repository (CORI-branded)
- Social media posts

**Key Features Desired:**
1. Data download capabilities
2. R package documentation with example code (beyond vignettes)
3. Geographic transformation processes & crosswalks
4. Visual map of dataset relationships
5. Projects tagged by resources used
6. Agent-readable config file (YML-based)
7. User-friendly, externally-facing interface
8. Change log tracking
9. "Stats Lab for Rural" methodology showcase
10. Procedurally generated using config file
11. Blog posts linked to datasets they use
12. GitHub links to dataset prep code
13. Data viz as teaching tools and downloadable branded content

---

## 📐 Technical Architecture Comparison

### Current Blog (Quarto):
- **Framework:** Quarto static site generator
- **Content:** 16 blog posts covering broadband data, spatial analysis, data tools
- **Navigation:** Simple (Home/About)
- **Theme:** Minty Bootstrap with custom SCSS
- **Deployment:** Manual build to `/docs` folder for GitHub Pages
- **Strengths:**
  - Excellent for R/Python code execution
  - Team is familiar with workflow
  - Good for technical content with visualizations

### NCCS Reference Site (Jekyll):
- **Framework:** Jekyll with hybrid Quarto integration
- **Architecture:** Collections-based with 3 content types:
  - `_datasets/` - Data catalog entries
  - `_resources/` - Projects, publications, tools
  - `_stories/` - Blog posts/articles
- **Features:**
  - Filterable listings by category/type
  - Sidebar layouts for detail pages
  - Component-based design system
  - Search functionality
  - Featured content sections
- **Deployment:** GitHub Pages with automatic builds
- **Hybrid Approach:** Quarto renders .qmd → .md, Jekyll builds final site
- **Strengths:**
  - Native collections for multiple content types
  - Powerful filtering/categorization
  - Reusable component system
  - Better suited for complex sites

### Example Sites Referenced:
1. [Harvard Dataverse](https://dataverse.harvard.edu/) - Data repository focus
2. [Urban Institute NCCS](https://nccsgit.urban.org/nccs/) - Layout inspiration
3. [Geocorr](https://mcdc.missouri.edu/applications/) - Geographic tools
4. [NFI Datamart](https://apps.fs.usda.gov/fia/datamart/datamart.html) - Data access
5. [Tidycensus](https://walker-data.com/tidycensus/articles/basic-usage.html) - R package docs
6. [Current CORI ERC Site](https://erc.ruralinnovation.us/) - Existing example

---

## 🏗️ Recommended Architecture for Rural Dataverse

### Hybrid Approach (Best of Both Worlds):

**Jekyll as the Primary Framework** (like NCCS) because:
- Native collections support for multiple content types
- Powerful filtering/search capabilities
- Liquid templating for dynamic content generation
- Component reusability across pages
- Better suited for complex, multi-section sites
- Config-driven (agent-readable YML files)

**Quarto Integration** (preserve for blog):
- Continue using `.qmd` for blog posts with R/Python code
- Quarto renders to markdown that Jekyll consumes
- Preserve team's existing workflow
- Keep frozen execution for reproducibility
- Allows complex data visualizations and analysis

### Proposed Site Structure:

```
/rural-dataverse/
├── _config.yml              # Jekyll config with collections, nav, metadata
├── _quarto.yml              # Quarto settings for blog posts
├── package.json             # Build scripts and dependencies
├── Gemfile                  # Ruby dependencies
│
├── Collections (Content Types):
│   ├── _datasets/           # Data catalog (S3 datasets, APIs, CSVs)
│   │   ├── fcc-nbm.md
│   │   ├── bea-regional.md
│   │   ├── form-d-filings.md
│   │   └── [dataset-name].md
│   │
│   ├── _packages/           # R package documentation
│   │   ├── cori-bea.md
│   │   ├── cori-fcc.md
│   │   ├── rural-definitions.md
│   │   ├── cori-charts.md
│   │   └── dform.md
│   │
│   ├── _projects/           # Project pages with tagged resources
│   │   ├── broadband-equity-analysis.md
│   │   ├── startup-ecosystems.md
│   │   └── [project-name].md
│   │
│   ├── _stories/            # Blog posts (existing MDA blog migrated here)
│   │   ├── awesome-jq.qmd
│   │   ├── mapping-rural-tips.qmd
│   │   ├── estimating-adoption-impact.qmd
│   │   └── [post-name].qmd
│   │
│   ├── _resources/          # Tools, crosswalks, publications, methodologies
│   │   ├── geographic-crosswalks.md
│   │   ├── rural-definitions-guide.md
│   │   ├── data-visualization-guide.md
│   │   └── [resource-name].md
│   │
│   └── _definitions/        # Rural definitions ontology
│       ├── metro-micro.md
│       ├── ruca-codes.md
│       ├── frontier-remote.md
│       └── [definition-name].md
│
├── _layouts/                # Page templates (HTML with Liquid)
│   ├── default.html         # Base template with header/footer
│   ├── dataset.html         # Dataset detail page
│   ├── package.html         # R package documentation page
│   ├── project.html         # Project detail page
│   ├── story.html           # Blog post page
│   ├── resource.html        # Resource detail page
│   └── definition.html      # Definition detail page
│
├── _includes/               # Reusable components
│   ├── components/
│   │   ├── card-horizontal.html    # Horizontal card layout
│   │   ├── card-vertical.html      # Vertical card layout
│   │   ├── filter-controls.html    # Category/type filtering
│   │   ├── hero.html               # Homepage hero section
│   │   ├── sidebar.html            # Sidebar with metadata/CTAs
│   │   ├── divider.html            # Section dividers
│   │   ├── button.html             # CTA buttons
│   │   ├── content-cta.html        # Content call-to-action
│   │   └── page-header.html        # Page title headers
│   ├── nav.html             # Main navigation
│   ├── footer.html          # Site footer
│   └── head.html            # HTML head with metadata
│
├── _data/                   # Structured data (YAML files)
│   ├── people.yml           # Team members (authors)
│   ├── ontology.yml         # Rural socio-economic metrics ontology
│   ├── crosswalks.yml       # Geographic crosswalks with citations
│   ├── navigation.yml       # Site navigation structure
│   └── changelog.yml        # Change log tracking
│
├── public/                  # Static assets
│   ├── scss/                # Design system (modular SCSS)
│   │   ├── base/            # Base styles, typography, reset
│   │   ├── components/      # Component styles
│   │   ├── utilities/       # Utility classes
│   │   └── main.scss        # Main stylesheet import
│   ├── js/                  # JavaScript modules
│   │   ├── filter.js        # Filtering functionality
│   │   ├── search.js        # Search functionality
│   │   └── main.js          # Main JS entry point
│   └── img/                 # Images, logos, placeholders
│       ├── logos/
│       ├── datasets/
│       └── viz-catalog/
│
├── Pages (Top-level site pages):
│   ├── index.html           # Homepage with featured content
│   ├── datasets.html        # Data catalog listing page
│   ├── packages.html        # R packages listing page
│   ├── projects.html        # Projects listing page
│   ├── stories.html         # Blog listing page (MDA blog)
│   ├── resources.html       # Resources listing page
│   ├── definitions.html     # Rural definitions listing
│   ├── catalog.html         # Data viz catalog
│   ├── about.html           # About the Dataverse & team
│   ├── contact.html         # Contact information
│   └── faq.html             # Frequently asked questions
│
├── assets/                  # Fonts and other assets (from current blog)
│   ├── fonts/
│   └── fonts.css
│
└── README.md                # Documentation for developers
```

---

## 🎨 Design & UX (Inspired by NCCS):

### Homepage Sections:
1. **Hero Section:**
   - Welcome message: "Rural Dataverse - Your hub for rural innovation data, tools, and research"
   - Brief description of what the Dataverse offers
   - Primary CTA buttons (Explore Datasets, View R Packages, Read Blog)
   - Hero image or data visualization

2. **Featured Datasets:**
   - 3-4 highlighted datasets with horizontal cards
   - Thumbnail images
   - Brief descriptions
   - "View All Datasets" CTA

3. **R Packages Showcase:**
   - Grid of available CORI R packages
   - Package names, descriptions, GitHub links
   - Installation instructions preview

4. **Latest Blog Posts (Stories):**
   - 3-4 recent posts from MDA blog
   - Author, date, categories
   - Featured images
   - "View All Stories" CTA

5. **Data Viz Gallery:**
   - Showcase of key CORI-branded visualizations
   - Downloadable charts (line charts, establishment entry rates, self-employment, patents, etc.)
   - Educational context for each viz

6. **Projects Highlight:**
   - Featured projects with resource tags
   - Links to full project pages

### Navigation Structure:
**Primary Navigation:**
- Home
- Datasets (Data catalog)
- R Packages (Package documentation)
- Projects (Work showcase)
- Stories (Blog/MDA content)
- Resources (Tools, crosswalks, guides)
- About

**Footer Navigation:**
- Privacy Policy
- Terms of Service
- Contact
- FAQ
- GitHub Organization
- Change Log

### Key Features:

1. **Filterable Collection Listings:**
   - Category filters (e.g., "Broadband", "Economic Development", "Geographic")
   - Type filters (e.g., "Dataset", "Tool", "Publication")
   - Geography filters (State, Region, Metro/Non-metro)
   - Date range filters

2. **Search Functionality:**
   - Full-text search across all collections
   - Search results grouped by content type
   - Autocomplete suggestions

3. **Sidebar Layouts for Detail Pages:**
   - Metadata (author, date, categories, tags)
   - Download CTAs (data files, code, documentation)
   - Related content links
   - Citation information
   - GitHub repository links
   - Used in projects (tagged connections)

4. **Tagged Resource Connections:**
   - Projects tagged with datasets/packages used
   - Blog posts linked to datasets they reference
   - Visual relationship map option

5. **Citation Generation:**
   - Auto-generated citations in multiple formats
   - APA, Chicago, BibTeX
   - Copy-to-clipboard functionality

6. **Responsive Design:**
   - Mobile-first approach
   - Touch-friendly filtering
   - Optimized images

---

## 🔧 Technical Implementation Details

### Jekyll Collections Configuration (_config.yml):

```yaml
collections:
  datasets:
    output: true
    permalink: /datasets/:name/
  packages:
    output: true
    permalink: /packages/:name/
  projects:
    output: true
    permalink: /projects/:name/
  stories:
    output: true
    permalink: /stories/:name/
  resources:
    output: true
    permalink: /resources/:name/
  definitions:
    output: true
    permalink: /definitions/:name/

defaults:
  - scope:
      type: "datasets"
    values:
      layout: "dataset"
      grid: "sidebar"
  - scope:
      type: "stories"
    values:
      layout: "story"
```

### Content Frontmatter Examples:

**Dataset (_datasets/fcc-nbm.md):**
```yaml
---
title: "FCC National Broadband Map Data"
description: "Comprehensive broadband availability and adoption data from the FCC"
date: 2025-12-15
categories: ["Broadband", "Infrastructure"]
tags: ["FCC", "Connectivity", "Geographic"]
featured: true
featuredOrder: 1
downloadUrl: "s3://cori-data/fcc/nbm-latest.csv"
githubUrl: "https://github.com/ruralinnovation/fcc-package"
usedInProjects: ["broadband-equity-analysis", "rural-connectivity-index"]
usedInStories: ["fcc-data-overview", "cleaning-formd"]
dataFormat: "CSV, Parquet"
updateFrequency: "Quarterly"
coverage: "United States"
---
```

**R Package (_packages/cori-fcc.md):**
```yaml
---
title: "cori.fcc"
description: "R package for working with FCC broadband data"
date: 2025-11-20
categories: ["R Package", "Broadband"]
tags: ["FCC", "Data Processing"]
featured: true
packageName: "cori.fcc"
githubUrl: "https://github.com/ruralinnovation/cori.fcc"
installCommand: 'remotes::install_github("ruralinnovation/cori.fcc")'
vignetteUrl: "https://ruralinnovation.github.io/cori.fcc/"
maintainer: "drew-rosebush"
status: "stable"
---
```

**Project (_projects/broadband-equity.md):**
```yaml
---
title: "Broadband Equity Analysis"
description: "Analyzing broadband access disparities in rural communities"
date: 2025-10-01
categories: ["Broadband", "Equity"]
featured: true
projectUrl: "https://broadband-equity.ruralinnovation.us"
usesDatasets: ["fcc-nbm", "census-acs"]
usesPackages: ["cori-fcc", "tidycensus"]
usesResources: ["geographic-crosswalks", "metro-definitions"]
team: ["drew-rosebush", "camden-blatchly", "olivier-leroy"]
status: "active"
---
```

**Story/Blog Post (_stories/estimating-adoption-impact.qmd):**
```yaml
---
title: "Estimating the Impact of Broadband Adoption"
author:
  - id: "drew-rosebush"
  - id: "camden-blatchly"
date: 2026-01-15
categories: ["Econometrics", "Broadband"]
tags: ["Regression", "Impact Evaluation"]
image: "thumbnail.png"
featured: true
usesDatasets: ["fcc-nbm", "bea-regional"]
usesPackages: ["cori-fcc", "fixest"]
usesResources: ["econometric-methods"]
freeze: true
---
```

### Quarto + Jekyll Integration:

**_quarto.yml:**
```yaml
project:
  type: website
  output-dir: ./_stories  # Render to Jekyll collection

format:
  gfm:
    variant: +yaml_metadata_block
    preserve-yaml: true

execute:
  freeze: true  # Cache code execution

extract-media: "_stories/"  # Extract assets to collection folder
```

**Workflow:**
1. Author writes `.qmd` file in `_stories/`
2. RStudio renders Quarto → `.md` in same directory
3. Jekyll processes `.md` with story layout
4. Final HTML generated with site chrome (nav, footer, etc.)

---

## 🚀 Implementation Plan - Phased Approach

### Phase 1 - Foundation (2-3 weeks)
**Goal:** Establish Jekyll architecture and migrate existing blog

**Tasks:**
- [ ] Set up Jekyll project structure
- [ ] Configure collections in `_config.yml`
- [ ] Create base layouts (default, story, dataset, etc.)
- [ ] Migrate existing 16 blog posts to `_stories/` collection
- [ ] Set up Quarto integration for `.qmd` rendering
- [ ] Create basic component library (cards, buttons, headers)
- [ ] Design and implement homepage
- [ ] Set up GitHub Pages deployment
- [ ] Migrate CORI branding (colors, fonts, logos)

**Deliverables:**
- Functional Jekyll site with migrated blog
- Homepage with hero and blog listings
- Basic navigation structure
- Quarto + Jekyll workflow operational

### Phase 2 - Data Integration (3-4 weeks)
**Goal:** Add datasets and R packages collections

**Tasks:**
- [ ] Create `_datasets/` collection with 5-10 key datasets
- [ ] Build dataset layout with sidebar, metadata, download CTAs
- [ ] Create datasets listing page with filtering
- [ ] Create `_packages/` collection for R packages
- [ ] Document existing packages (BEA, FCC, Rural Definitions, cori.charts, dform)
- [ ] Build package layout with installation instructions, vignettes
- [ ] Create packages listing page
- [ ] Implement category/tag filtering system
- [ ] Add GitHub API integration for package stats
- [ ] Create geographic crosswalks section in `_resources/`

**Deliverables:**
- Datasets catalog with filtering
- R packages documentation hub
- Download functionality for data files
- Geographic crosswalks reference

### Phase 3 - Projects & Resources (2-3 weeks)
**Goal:** Add projects and resources collections

**Tasks:**
- [ ] Create `_projects/` collection
- [ ] Build project layout with resource tagging
- [ ] Implement "uses" relationships (datasets, packages, resources)
- [ ] Create projects listing page
- [ ] Create `_resources/` collection for tools, publications, guides
- [ ] Build resources listing page with type filtering (Tools, Publications, Guides)
- [ ] Add rural definitions to `_definitions/` collection
- [ ] Create ontology visualization page

**Deliverables:**
- Projects showcase with tagged resources
- Resources library
- Rural definitions reference
- Interconnected content via tags

### Phase 4 - Advanced Features (3-4 weeks)
**Goal:** Add search, visualization catalog, and enhanced UX

**Tasks:**
- [ ] Implement site-wide search (Algolia or Lunr.js)
- [ ] Create data visualization catalog page
- [ ] Build downloadable viz library (CORI-branded charts)
- [ ] Add relationship visualization (how datasets/projects/packages connect)
- [ ] Implement citation generator
- [ ] Add change log functionality
- [ ] Create agent-readable API/config endpoints
- [ ] Set up CI/CD for automated builds and testing
- [ ] Add RSS feeds for all collections
- [ ] Implement social sharing cards (Open Graph)

**Deliverables:**
- Full-text search functionality
- Data viz catalog with downloads
- Relationship mapping visualization
- Automated deployment pipeline
- Enhanced discoverability (RSS, social sharing)

### Phase 5 - Polish & Launch (1-2 weeks)
**Goal:** Final testing, documentation, and public launch

**Tasks:**
- [ ] Comprehensive testing (links, forms, downloads)
- [ ] Mobile responsiveness testing
- [ ] Accessibility audit (WCAG compliance)
- [ ] Performance optimization (image compression, caching)
- [ ] SEO optimization (meta tags, sitemaps, robots.txt)
- [ ] Write user documentation (README, contributing guide)
- [ ] Create admin/editor guide for team
- [ ] Set up Google Analytics
- [ ] Launch announcement blog post
- [ ] Update CORI website links to new Dataverse
- [ ] Social media promotion

**Deliverables:**
- Production-ready Rural Dataverse
- Comprehensive documentation
- Launch announcement
- Metrics tracking

---

## 📊 Content Migration Strategy

### Existing Blog Posts (16 posts):
**Action:** Migrate to `_stories/` collection
- Keep existing `.qmd` format
- Add new frontmatter fields (featured, usesDatasets, usesPackages)
- Update image paths to work with Jekyll structure
- Re-render with Quarto to generate markdown

### Datasets to Add (Priority):
1. FCC National Broadband Map (NBM)
2. BEA Regional Economic Data
3. SEC Form D Venture Capital Filings
4. Census ACS Rural Indicators
5. USDA Rural-Urban Commuting Area (RUCA) Codes
6. Patents data (USPTO)
7. BDS, COG, QCEW datasets

### R Packages to Document:
1. cori.bea (BEA data)
2. cori.fcc (FCC data)
3. Rural Definitions package
4. cori.charts (visualization)
5. dform (Form D data)
6. Upcoming: BDS, COG, QCEW, USPTO packages

### Resources to Create:
1. Geographic crosswalks guide
2. Rural definitions methodology
3. Data visualization best practices
4. Econometric methods guide
5. Data cleaning workflows
6. API documentation

---

## 🎨 Design System Notes

### Color Palette (Carry from Current Blog):
- **Primary:** #00835D (CORI green for links)
- **Header:** #16343E (Dark teal)
- **Background:** #FFF (White)
- **Text:** #121E22 (Dark gray)
- **Accent:** Light green for breadcrumbs/highlights

### Typography:
- **Headings:** Bitter (from Google Fonts)
- **Body:** TT Hoves (custom font) with fallbacks
- **Code:** Monospace (system fonts)

### Components Needed:
- Hero section with image
- Horizontal cards (datasets listing)
- Vertical cards (grid layouts)
- Filter controls (category, type, date)
- Sidebar (metadata, CTAs, related content)
- Page headers
- Dividers
- Buttons (primary, secondary, external link)
- Content CTAs
- Breadcrumbs
- Citation boxes
- Code blocks with syntax highlighting

---

## 🔍 SEO & Discoverability

### Metadata Strategy:
- Unique meta descriptions for all pages
- Open Graph tags for social sharing
- Twitter Card tags
- Structured data (JSON-LD) for datasets, articles, FAQs
- Canonical URLs
- XML sitemap (auto-generated by Jekyll)
- robots.txt configuration

### Content Strategy:
- Clear, descriptive URLs (/datasets/fcc-broadband-map)
- Alt text for all images
- Heading hierarchy (H1 → H6)
- Internal linking between related content
- External links to authoritative sources
- Regular content updates (signal freshness to search engines)

---

## 🔐 Agent-Readable Configuration

### API Endpoints (JSON):
Create JSON representations of content for agent consumption:

**`/api/datasets.json`** - All datasets with metadata
**`/api/packages.json`** - All R packages with installation info
**`/api/projects.json`** - All projects with resource tags
**`/api/ontology.json`** - Rural metrics ontology structure
**`/api/crosswalks.json`** - Geographic crosswalk definitions

### Implementation:
Use Jekyll's data files and Liquid to generate JSON:

```liquid
---
layout: null
---
{
  "datasets": [
    {% for dataset in site.datasets %}
    {
      "title": "{{ dataset.title }}",
      "description": "{{ dataset.description }}",
      "url": "{{ dataset.url | absolute_url }}",
      "downloadUrl": "{{ dataset.downloadUrl }}",
      "categories": {{ dataset.categories | jsonify }},
      "updatedAt": "{{ dataset.date }}"
    }{% unless forloop.last %},{% endunless %}
    {% endfor %}
  ]
}
```

---

## 📝 Documentation Needs

### For Users:
- Getting started guide
- How to download data
- How to install R packages
- How to cite resources
- FAQ page

### For Contributors:
- Adding new datasets (frontmatter template)
- Writing blog posts (.qmd workflow)
- Adding R package documentation
- Component usage guide
- Style guide
- Git workflow (branches, PRs)

### For Developers:
- Setup instructions (Ruby, Jekyll, Quarto)
- Build process documentation
- Component API reference
- Deployment process
- Troubleshooting guide

---

## 🧪 Testing Strategy

### Functional Testing:
- All internal links work
- All external links valid
- Download links functional
- Forms submit correctly
- Search returns accurate results
- Filters work as expected

### Cross-Browser Testing:
- Chrome, Firefox, Safari, Edge
- Mobile Safari, Chrome Mobile

### Performance Testing:
- Page load times < 3 seconds
- Lighthouse scores (Performance, Accessibility, SEO)
- Image optimization
- Code minification

### Accessibility Testing:
- Screen reader compatibility
- Keyboard navigation
- Color contrast ratios (WCAG AA)
- Focus indicators
- Semantic HTML

---

## 📈 Success Metrics

### Engagement:
- Page views per session
- Time on site
- Blog post read rates
- Dataset downloads
- R package installations (track via GitHub)

### Discoverability:
- Organic search traffic
- Referral sources
- Social shares
- Inbound links

### Utility:
- Search query success rate
- Filter usage
- Citation tool usage
- Most popular datasets/packages
- User feedback (surveys, contact form)

---

## 🤝 Team Coordination

### Roles:
- **Content Authors:** Continue writing blog posts in Quarto
- **Data Stewards:** Maintain dataset metadata and S3 links
- **Package Maintainers:** Update package documentation
- **Web Admin:** Merge PRs, deploy updates, monitor analytics
- **Editorial Review:** Austin (as noted in current README)

### Workflow:
1. Create feature branch
2. Add/update content in appropriate collection
3. Test locally with Jekyll dev server
4. Submit PR for review
5. Editorial review and feedback
6. Merge to main → auto-deploy to GitHub Pages

---

## 🔮 Future Enhancements (Beyond Initial Launch)

### Interactive Features:
- Data explorer tools (filter, visualize, download)
- Embedded Shiny apps (like NCCS Dashboard)
- Interactive maps
- Code sandboxes for package examples

### Community Features:
- User comments (via GitHub Discussions)
- External contributions (guest blog posts)
- Community-submitted datasets
- Use case submissions

### Advanced Analytics:
- Custom dashboard showing dataset popularity
- R package download trends
- Geographic distribution of users
- Content recommendation engine

### Integration:
- Slack notifications for new content
- RSS to email subscription
- Integration with CORI's main website
- Cross-promotion with research reports

---

## 📚 References

### Example Sites:
- Harvard Dataverse: https://dataverse.harvard.edu/
- Urban Institute NCCS: https://nccsgit.urban.org/nccs/
- Geocorr: https://mcdc.missouri.edu/applications/
- NFI Datamart: https://apps.fs.usda.gov/fia/datamart/datamart.html
- Tidycensus: https://walker-data.com/tidycensus/
- Current CORI ERC: https://erc.ruralinnovation.us/

### Technical Documentation:
- Jekyll: https://jekyllrb.com/docs/
- Quarto: https://quarto.org/docs/
- Liquid Templating: https://shopify.github.io/liquid/
- GitHub Pages: https://docs.github.com/en/pages

### Design Resources:
- NCCS GitHub Repo: https://github.com/UrbanInstitute/nccs
- Urban Institute Design System
- CORI Brand Guidelines (internal)

---

## ✅ Next Steps

**Immediate Actions:**
1. Review and discuss this plan with the team
2. Prioritize features based on team capacity and timeline
3. Set up development environment (Jekyll + Quarto)
4. Begin Phase 1 implementation
5. Establish regular check-ins for progress updates

**Questions to Resolve:**
- Domain name for Rural Dataverse (subdomain or new domain?)
- Hosting preferences (GitHub Pages vs. Netlify vs. custom)
- Access control (public vs. gated content)
- Analytics platform preferences
- Budget for any paid tools (Algolia search, etc.)

---

**Document Version:** 1.0
**Last Updated:** January 31, 2026
**Contact:** Drew Rosebush (drew.rosebush@ruralinnovation.us)
