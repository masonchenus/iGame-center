# Common Issues

This document lists common problems encountered after cloning the repository, along with their causes and recommended fixes.

If you are new to the project, read **Cloned-Repo-Guide.md** first.

---

## 🚫 Installation & Dependency Issues

### Dependencies fail to install
**Symptoms:**
- `npm install` fails
- Missing packages or version conflicts

**Causes:**
- Unsupported Node.js version
- Corrupted lockfile
- Incomplete installation

**Fixes:**
- Ensure you are using a supported Node.js version
- Delete `node_modules/` and reinstall dependencies
- Do not manually edit `package-lock.json`

---

### Package lock conflicts
**Symptoms:**
- Errors referencing `package-lock.json`
- Unexpected dependency behavior

**Causes:**
- Mixing package managers
- Manual edits to lockfile

**Fixes:**
- Use only one package manager (`npm`)
- Regenerate the lockfile if instructed in docs

---

## ⚙️ Build & Runtime Issues

### Project does not start
**Symptoms:**
- App fails to load
- Server exits immediately
- Blank page or fatal error

**Causes:**
- Skipped setup steps
- Missing environment configuration
- Backend not running (if required)

**Fixes:**
- Follow `Getting-Started.md` step by step
- Verify required services are running
- Check console and terminal output for errors

---

### Backend-related errors (`ai_backend/`)
**Symptoms:**
- API errors
- Features not working
- Connection failures

**Causes:**
- Backend not started
- Missing dependencies
- Incorrect configuration

**Fixes:**
- Ensure `ai_backend/` dependencies are installed
- Start backend services before frontend
- Review backend-specific documentation

---

## 🧑‍💻 Development Environment Issues

### Linting or formatting errors
**Symptoms:**
- ESLint or Prettier errors
- CI failing due to formatting

**Causes:**
- Editor not using project settings
- Manual formatting overrides

**Fixes:**
- Use the provided `.editorconfig`
- Enable ESLint and Prettier in your editor
- Do not disable lint rules locally

---

### Editor behaves unexpectedly
**Symptoms:**
- Inconsistent formatting
- Warnings not matching CI

**Causes:**
- Editor extensions overriding repo settings

**Fixes:**
- Use the `.vscode/` configuration if applicable
- Disable conflicting global extensions

---

## 📦 File & Folder Confusion

### Unsure which files are safe to edit
**Rule of thumb:**
- Safe: `src/`, `ai_backend/`, `docs/`
- Avoid: `tmp/`, logs, generated files

If you are unsure, treat the file as **read-only**.

---

### Deleted a file and things broke
**Cause:**
- Some files appear unused but are required by tooling or deployment

**Fix:**
- Restore the file from Git
- Review `Repository-Layout.md` before removing files

---

## 🚀 Deployment & Config Issues

### Deployment fails (Vercel / Netlify)
**Symptoms:**
- Build passes locally but fails in deployment
- Environment mismatch errors

**Causes:**
- Modified deployment config files
- Missing environment variables

**Fixes:**
- Do not edit `vercel.json` or `netlify.toml` unless instructed
- Compare deployment logs with local output

---

### `.htaccess` or routing issues
**Symptoms:**
- Routes not resolving
- 404 errors in production

**Causes:**
- Server misconfiguration
- Unsupported hosting environment

**Fixes:**
- Verify hosting platform compatibility
- Restore original `.htaccess` if modified

---

## 🔐 Permissions & Access Issues

### Cannot push to repository
**Cause:**
- You do not have write access

**Fix:**
- Fork the repository
- Submit changes via Pull Request
- See `Contributing.md`

---

## 🆘 Getting Help

If your issue is not listed here:

1. Check `issues.txt`
2. Search existing GitHub Issues
3. Review recent commits or changelog
4. Open a new issue with:
   - OS and version
   - Runtime version
   - Exact error message
   - Steps to reproduce

---

## ℹ️ Final Notes

- Most issues are environment-related
- Do not rush setup steps
- When in doubt, follow the documentation order

This repository is structured to be safe to explore and extend when the guides are followed.
