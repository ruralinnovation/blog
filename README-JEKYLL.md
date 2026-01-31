# Rural Dataverse

> Your hub for rural innovation data, tools, research, and analysis from the CORI Mapping & Data Analytics team.

**Live Site:** https://ruralinnovation.github.io/blog/

---

## 🚀 Quick Start

```bash
# Install dependencies
bundle install

# Start development server
bundle exec jekyll serve

# Visit: http://localhost:4000/blog/
```

---

## 📦 What's Included

This site provides:

- **📊 Datasets**: Curated rural data resources
- **📦 R Packages**: Tools for rural data analysis
- **🔬 Projects**: Research and analysis showcases
- **✍️ Blog**: Technical posts and methodologies
- **📚 Resources**: Guides, crosswalks, and references

---

## 🏗️ Architecture

**Jekyll + Quarto Hybrid**

- **Jekyll**: Site framework with collections for multiple content types
- **Quarto**: Blog post authoring with R/Python code execution
- **GitHub Pages**: Automatic deployment from `docs/` folder

### Why This Stack?

- ✅ Multiple content types (datasets, packages, projects, blog)
- ✅ Keep existing Quarto blog workflow
- ✅ Component-based design
- ✅ Easy to add/edit content
- ✅ Fast iteration and deployment

---

## 📝 Adding Content

### Quick Examples:

**Add a dataset:**
```bash
# Create: _datasets/my-data.md
```

**Add a blog post:**
```bash
# Simple: _stories/my-post.md
# With R code: _stories/my-analysis.qmd
```

**Add an R package:**
```bash
# Create: _packages/my-package.md
```

See [DEVELOPMENT.md](DEVELOPMENT.md) for detailed instructions.

---

## 🚢 Deployment

### Current Simple Workflow:

1. Make changes locally
2. Build site: `bundle exec jekyll build`
3. Commit and push to GitHub
4. GitHub Pages auto-deploys from `docs/` folder

**That's it!** Changes live in ~1 minute.

See [DEVELOPMENT.md](DEVELOPMENT.md) for automated deployment with GitHub Actions.

---

## 📚 Documentation

- **[DEVELOPMENT.md](DEVELOPMENT.md)** - Full development guide
- **[ideation docs/Rural Dataverse - Architecture & Implementation Plan.md](ideation%20docs/Rural%20Dataverse%20-%20Architecture%20%26%20Implementation%20Plan.md)** - Architecture and roadmap

---

## 🎨 Customization

### Styling

Edit `assets/css/main.css` for custom styles.

### Navigation

Edit `_config.yml` to change nav items:

```yaml
nav:
  - title: Home
    url: /
  - title: Datasets
    url: /datasets/
  # Add more items...
```

### Layouts

Modify templates in `_layouts/` and components in `_includes/components/`

---

## 🔧 Tech Stack

- **Jekyll 4.3** - Static site generator
- **Quarto** - Document authoring with code
- **Liquid** - Templating language
- **GitHub Pages** - Hosting
- **Ruby 2.7+** - Jekyll runtime

---

## 📂 Project Structure

```
/blog/
├── _datasets/       # Data catalog
├── _packages/       # R packages
├── _projects/       # Projects
├── _stories/        # Blog posts
├── _resources/      # Resources
├── _layouts/        # Page templates
├── _includes/       # Components
├── assets/          # CSS, images
├── docs/            # Built site (generated)
└── _config.yml      # Jekyll config
```

---

## 🤝 Contributing

1. Create a feature branch
2. Add/edit content
3. Test locally: `bundle exec jekyll serve`
4. Submit a pull request
5. Editorial review (Austin)
6. Merge to main → auto-deploy

---

## 📊 Migration Status

- [x] Jekyll structure set up
- [x] Collections configured
- [x] Example content created
- [x] Layouts and components built
- [x] Homepage and listing pages
- [ ] Migrate all 16 blog posts from `posts/` to `_stories/`
- [ ] Add all datasets to `_datasets/`
- [ ] Document R packages in `_packages/`
- [ ] Add projects to `_projects/`
- [ ] Enhance styling
- [ ] Add filtering/search
- [ ] Set up GitHub Actions

---

## 🐛 Troubleshooting

**Site not building?**
```bash
bundle exec jekyll build --verbose
```

**Ruby issues?**
```bash
ruby --version  # Should be 2.7+
gem install bundler
bundle install
```

**Quarto not working?**
```bash
quarto check
```

See [DEVELOPMENT.md](DEVELOPMENT.md) for more help.

---

## 📞 Contact

**CORI Mapping & Data Analytics Team**

- GitHub: https://github.com/ruralinnovation
- Website: https://ruralinnovation.us
- Email: drew.rosebush@ruralinnovation.us

---

## 📄 License

ISC License - Rural Innovation Strategies, Inc.
