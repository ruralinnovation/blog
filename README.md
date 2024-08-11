# Data blog for the Mapping and Data Analytics team at the Center on Rural Innovation (CORI)

## Getting started

1. [Download Quarto](https://quarto.org/docs/download/)
2. Clone this repository

## Blog post workflow

1. Draft blog post
   1. Create a short name representing the title for your article. This should be lowercase with underscores instead of spaces. Use this name to create a new branch (`git branch -d your_branch_name`) and add a new folder in the `posts` directory (`mkdir posts/your_branch_name`).
   2. Within the new folder, add an `index.qmd` file (where your blog post will go) and a `thumbnail.png` file (an image that will appear in the listing of recent articles; screenshots are OK, but charts/diagrams/infographics are better).
   3. Write your blog post. Using the command line, navigate to the root folder of the blog project and run `quarto preview`  (or use the “Render” button in RStudio) to view your changes in the browser.
2. Commit changes and publish to Github with (Draft) in the title
3. Copy text into a google doc
   1. Make the document title = blog post's full title
   2. In the google doc, add a link to the blog post's folder in Github ([ex.](https://github.com/ruralinnovation/blog/tree/main/posts/awesomejq))
4. Let Austin know that there is a blog post ready for review
5. Austin will edit the blog post (over the next 5 days)
   1. If you don’t see any edits/suggestions, do not assume that it’s perfect. Reach out and confirm with Austin
6. Once Austin’s feedback is the document, action suggestions/edits (only if it makes sense)
7. Migrate edits over to the blog’s quarto file (locally)
8. Publish final changes:
   1. Commit to blog post branch:
      1. In reference to step 1 above for drafting a blog post, check out the same branch that you used to initially create the article.
      2. Edit your blog post. Using the command line, navigate to the root folder of the blog project and run `quarto preview`  (or use the “Render” button in RStudio) to view your changes in the browser.
         1. If you didn’t use the RStudio “Render” button to preview your changes, you now need to run `quarto render` in your terminal to output new files to the `docs` directory.
         2. If you preview and notice that there are style issues, either fix it or flag the issue for your reviewer after you push the changes
      3. Stage your updates (including file changes in `docs`) and commit your changes (`git commit -m “[message]”`). Push a new remote branch (`git push -u origin your_branch_name`) to Github
   2. Create a pull request to merge blog post branch into `main`.
      1. Have another team member review your pull request.
      2. Merge your changes


## Communications with com's

Link to G-drive: https://drive.google.com/drive/folders/1zkaHw8XEH0TPZExkmn-VOB1SITgXXPAd 
