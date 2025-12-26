# GitHub Setup Instructions

Your openstockflow package is ready to be published! Follow these steps to create the GitHub repository and deploy the website.

## Step 1: Create the GitHub Repository

1. Go to https://github.com/new
2. Fill in the repository details:
   - **Repository name**: `openstockflow`
   - **Description**: "Categorical Stock-Flow Modeling with Compositional Diagrams"
   - **Visibility**: Public (recommended) or Private
   - **DO NOT** initialize with README, .gitignore, or license (we already have these)
3. Click "Create repository"

## Step 2: Push Your Code

Once the repository is created, run these commands from your terminal:

```bash
cd /Users/ianmoran/Library/CloudStorage/Dropbox/projects/openstockflow

# Push all your commits to GitHub
git push -u origin main
```

If you encounter authentication issues, you may need to set up a personal access token:
- Go to https://github.com/settings/tokens
- Generate a new token (classic) with `repo` scope
- Use the token as your password when prompted

## Step 3: Configure GitHub Pages

After pushing, configure GitHub Pages to display your website:

1. Go to your repository on GitHub: https://github.com/ianmoran11/openstockflow
2. Click on **Settings** (top right)
3. Click on **Pages** (left sidebar)
4. Under "Build and deployment":
   - **Source**: Deploy from a branch
   - **Branch**: Select `gh-pages` and `/ (root)`, then click Save

**Note**: The `gh-pages` branch will be created automatically by the GitHub Actions workflow after your first push. If it doesn't appear immediately, wait a few minutes for the workflow to complete.

## Step 4: Verify Deployment

After a few minutes (typically 2-5 minutes):

1. Check the Actions tab: https://github.com/ianmoran11/openstockflow/actions
   - You should see two workflows running:
     - "pkgdown" - builds your website
     - "R-CMD-check" - tests your package

2. Once the pkgdown workflow completes (green checkmark), your website will be available at:
   - https://ianmoran11.github.io/openstockflow/

## Troubleshooting

### If the website doesn't appear:

1. Check that the pkgdown workflow completed successfully
2. Verify that the `gh-pages` branch exists in your repository
3. Ensure GitHub Pages is configured to use the `gh-pages` branch
4. Wait a few more minutes - GitHub Pages can take up to 10 minutes to deploy

### If authentication fails:

You can use SSH instead of HTTPS:
```bash
git remote set-url origin git@github.com:ianmoran11/openstockflow.git
git push -u origin main
```

This requires SSH keys set up in your GitHub account: https://docs.github.com/en/authentication/connecting-to-github-with-ssh

## What's Included

Your repository includes:

✅ Comprehensive README with examples
✅ pkgdown configuration with tidyverse styling
✅ GitHub Actions for automated website deployment
✅ GitHub Actions for CI/CD (R CMD check)
✅ 8 comprehensive vignettes
✅ 233 passing tests
✅ All source code and documentation

## Next Steps

After deployment, you can:

- Share your package: `remotes::install_github("ianmoran11/openstockflow")`
- Share your website: https://ianmoran11.github.io/openstockflow/
- Continue development: any push to `main` will automatically rebuild the website
- Add badges to your README (they'll work automatically after first deployment)

## Questions?

If you encounter any issues, feel free to:
- Check the GitHub Actions logs for error messages
- Review GitHub's documentation on Pages: https://docs.github.com/en/pages
- Open an issue on the repository once it's created
