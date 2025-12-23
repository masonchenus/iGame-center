# Welcome! My name is Mason and I am the maintainer of this repo.

This guide is for anyone who cloned the repository and wants to understand what to do next.
**It applies to viewers, developers, contributors, and collaborators.**

Hello! My name is Mason and I am the maintainer of this repo. You can see that i have some collabs already.

Before continuing, Please make sure you have a supported platform or browser.

## Here is the repo layout (top level & configs)

.github/                — GitHub CI/CD workflows  
.vscode/               — Editor settings  
.zencoder/             — (optional / tool configs)  
Assets/images/         — Static images and assets  
ai_backend/           — Backend services or AI logic  
docs/                  — Project documentation  
scripts/               — Build/dev scripts  
src/                   — Main application source  
tmp/                   — Temporary files  

### Configuration & metadata files:
.babelrc               — Babel config  
.browserslistrc        — Target browser config  
.editorconfig          — Editor consistency rules  
.eslintrc.json         — Linting rules  
.gitignore             — Ignored files for Git  
.htaccess              — Web server config  
.nojekyll              — Disable GitHub Pages Jekyll  
.prettierrc           — Prettier formatting rules  
LICENSE                — Project license  
Privacy-Policy.md      — Privacy policy  
README.md              — Overview and essential info  
issues.txt             — Issue notes  
netlify.toml           — Netlify deploy config  
package-lock.json      — Node lockfile  
package.json           — Dependencies & scripts  
server.log             — Log file  
vercel.json            — Vercel deployment config  

## ✅ What You Should (and Should Not) Do After Cloning

This section exists to keep the repository stable and to save you time.

### ✅ You Should
- Read the documentation that matches **your role** (viewer, developer, contributor).
- Run the **basic setup or build** once to confirm your environment works.
- Use a **separate branch** for experiments or changes.
- Keep your local repository up to date with the main branch.
- Follow existing **code style, formatting, and structure**.
- Check the `docs/` directory before asking questions — many answers already exist.

---

### ❌ You Should NOT
- Commit directly to the `main` or `production` branch.
- Rename or restructure core directories (`src/`, `ai_backend/`, `docs/`) without discussion.
- Edit generated or temporary files (`tmp/`, build outputs, logs).
- Commit secrets, tokens, credentials, or `.env` files.
- Modify deployment configuration files (`vercel.json`, `netlify.toml`, `.htaccess`) unless you know exactly why.
- Assume unused folders or files are safe to delete — some are required for tooling or CI.

> ⚠️ If you are unsure whether a change is safe, treat it as **read-only** and ask first.

---

## 🧪 First Successful Run (Recommended)

Before making changes, confirm the project runs correctly on your machine.

**At a minimum, you should be able to:**
1. Install dependencies
2. Run the project in development mode
3. See expected output (application loads, server starts, no fatal errors)

Exact commands and platform-specific steps are documented in:
- `Getting-Started.md`
- `Developer-Guide.md`

If you cannot complete a first run, **do not start modifying code yet** — fix the environment first.

---

## ⚠️ Common Mistakes After Cloning

These issues are common and easy to avoid:

- Skipping dependency installation
- Using unsupported Node or runtime versions
- Running production scripts during development
- Editing files under `tmp/`, logs, or deployment output
- Assuming `ai_backend/` is optional when it is required for features
- Ignoring linting or formatting warnings

If something breaks immediately after cloning, check:
- `Common-Issues.md`
- `issues.txt`
- Recent changelog or commit history
