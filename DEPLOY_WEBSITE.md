# 🚀 Deploy Your pkgdown Website

Your package website is ready to deploy! Follow these steps to make it live.

## Step 1: Wait for GitHub Actions to Complete ⏱️

After pushing your code, GitHub Actions automatically builds your website.

1. **Go to**: https://github.com/ianmoran11/openstockflow/actions
2. **Look for**: The "pkgdown" workflow
3. **Wait for**: A green checkmark ✅ (usually takes 2-5 minutes)

If you see a yellow circle 🟡, the workflow is still running. Wait for it to complete.

## Step 2: Verify the gh-pages Branch Exists 🔍

The pkgdown workflow creates a special `gh-pages` branch with your website files.

1. **Go to**: https://github.com/ianmoran11/openstockflow/branches
2. **Look for**: A branch called `gh-pages`
3. If it exists, you're ready for the next step!

## Step 3: Configure GitHub Pages ⚙️

Now tell GitHub to serve your website from the gh-pages branch:

1. **Go to**: https://github.com/ianmoran11/openstockflow/settings/pages
2. Under "Build and deployment":
   - **Source**: Select "Deploy from a branch"
   - **Branch**: Select `gh-pages`
   - **Folder**: Select `/ (root)`
3. Click **"Save"**

You should see a message saying "Your site is ready to be published at..."

## Step 4: Add Website URL to Repository About Section 🔗

Make the website link visible on your repository homepage:

1. **Go to**: https://github.com/ianmoran11/openstockflow
2. Click the **⚙️ gear icon** next to "About" (top right of the page)
3. In the "Website" field, enter: `https://ianmoran11.github.io/openstockflow/`
4. Optionally add a description: `Categorical Stock-Flow Modeling with Compositional Diagrams`
5. Check the box for "Use your GitHub Pages website" (if available)
6. Click **"Save changes"**

Now your website link will appear prominently in the repository's About section!

## Step 5: Wait for Deployment 🌐

GitHub Pages typically takes 2-10 minutes to deploy after configuration.

**Your website will be live at**:
### 🌐 https://ianmoran11.github.io/openstockflow/

## Verify Deployment ✅

Once deployed, your website should show:
- 🏠 **Home page** with package overview and quick start
- 📚 **Reference** with all functions organized by topic
- 📖 **Articles** with all 8 vignettes
- 🔍 **Search functionality**
- 📱 **Mobile-responsive design**

## Troubleshooting 🔧

### Website shows 404 error?
- Wait a few more minutes - initial deployment can take up to 10 minutes
- Check that the pkgdown workflow completed successfully (green checkmark)
- Verify GitHub Pages is configured to use `gh-pages` branch

### Workflow failed?
- Go to: https://github.com/ianmoran11/openstockflow/actions
- Click on the failed workflow to see error logs
- Most common issues are dependencies or R version problems

### Website is blank or shows old content?
- GitHub Pages can cache content. Wait 5-10 minutes or try:
  - Hard refresh your browser (Cmd+Shift+R on Mac, Ctrl+Shift+R on Windows)
  - Clear your browser cache
  - Try in a private/incognito window

## Future Updates 🔄

Good news! Your website will **automatically rebuild** whenever you:
- Push changes to the `main` branch
- Update any documentation
- Add new vignettes
- Modify the README

No manual steps needed after initial setup! 🎉

## What Your Website Includes 📦

✅ Beautiful tidyverse-style design
✅ All function documentation organized by topic
✅ 8 comprehensive vignettes:
   - Quick Start guides (Algebraic + Pipe-based)
   - Introduction tutorials (Algebraic + Pipe-based)
   - Composition guides (Algebraic + Pipe-based)
   - Mathematical Foundations (Algebraic + Pipe-based)
✅ Search functionality across all documentation
✅ Responsive mobile layout
✅ Automatic updates on every push

---

## Questions?

If you run into issues:
1. Check the Actions tab for workflow logs
2. Review GitHub Pages documentation: https://docs.github.com/en/pages
3. Verify the pkgdown badge on your README shows a green checkmark
