# Repository Layout

This document provides an overview of the top-level structure of the repository and the purpose of each folder and key file. Understanding this layout helps developers, contributors, and collaborators navigate and work with the project safely.

---

## 📁 Top-Level Folders

ai_backend/                 <= AI backend
src/                        <= Main application (Frontend)
docs/                       <= Documentation
scripts/                    <= Public scripts if you installed it & for devs & viewers
Assets/                     <= Static assets & resources
references/                 <= References for people

---

## 📄 Key Configuration & Metadata Files

.babelrc → Babel configuration
.browserslistrc → Supported browser configuration
.editorconfig → Editor consistency rules
.eslintrc.json → ESLint rules for code linting
.gitignore → Files and folders ignored by Git
.htaccess → Web server configuration
.nojekyll → Disables Jekyll processing on GitHub Pages
.prettierrc → Prettier formatting rules
LICENSE → Project license
Privacy-Policy.md → Privacy policy documentation
README.md → Project overview and key information
issues.txt → User-reported issues and known bugs
netlify.toml → Netlify deployment configuration
package-lock.json → Node dependency lockfile
package.json → Project dependencies and scripts
server.log → Runtime logs
vercel.json → Vercel deployment configuration
---

## 🔹 Notes

- **ai_backend/**: Contains server-side code or AI-related logic. Required for backend-dependent features.
- **src/**: The main application code. Safe to modify for development and feature changes.
- **docs/**: All documentation lives here. Edit cautiously to avoid breaking links or references.
- **scripts/**: Automation scripts for building, testing, or deployment. Use as intended; do not delete.
- **Assets/images/**: Static assets for the app, documentation, or demos.
- **tmp/**: Temporary files; do not modify or commit changes from this folder.

- Configuration files define environment, formatting, and deployment behavior. Only edit if you understand their impact.
- `issues.txt` is a user-facing file to report or view known issues; do not remove.
- Deployment configs (`netlify.toml`, `vercel.json`, `.htaccess`) should only be edited by maintainers or experienced contributors.

---

## ⚡ Summary

Understanding this layout ensures:
- Safe navigation of the repository
- Avoiding accidental edits to critical files
- Efficient contribution and development workflow

For more details on roles and setup, see:
- `Cloned-Repo-Guide.md`
- `Getting-Started.md`
- `Developer-Guide.md`
