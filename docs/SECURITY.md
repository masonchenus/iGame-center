# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 1.0.x   | :white_check_mark: |
| < 1.0   | :x:                |

## Reporting a Vulnerability

**PLEASE DO NOT** report security vulnerabilities publicly. 

If you discover a security vulnerability, please send an email to the project maintainers (masonchenus@masonchenus or @masonchenus@gmail.com) with:

1. **Description** of the vulnerability
2. **Affected versions**
3. **Steps to reproduce** (if applicable)
4. **Potential impact**
5. **Suggested fix** (if you have one)

The maintainers will acknowledge your report within 96 hours and provide an estimated timeline for a fix.

## Security Best Practices

### For Users

- Keep your browser updated
- Enable JavaScript execution only from trusted sources
- Use HTTPS when accessing the application
- Clear browser cache regularly for the latest updates
- Disable browser extensions that may interfere with gaming

### For Developers

- Never commit secrets or API keys to the repository
- Use environment variables for sensitive configuration
- Validate all user inputs
- Sanitize data before rendering
- Keep dependencies updated: `npm audit fix`
- Follow OWASP security guidelines
- Test for XSS, CSRF, and injection vulnerabilities
- Keep .gitignore promptly up-to-date.

## Security Headers

The application includes the following security headers:

- **X-Content-Type-Options**: Prevents MIME-type sniffing
- **X-Frame-Options**: Prevents clickjacking attacks
- **X-XSS-Protection**: Enables browser XSS filtering
- **Referrer-Policy**: Controls referrer information
- **Content-Security-Policy**: (Recommended to add)

## Dependency Security

Run security audits regularly:

```bash
npm audit
npm audit fix
npm outdated
```

## Service Worker Security

The Service Worker implements:

- Cache strategy for offline functionality
- Network-first approach for API calls
- Fallback strategies for failed requests

## Data Privacy

- No personal data is collected
- Game progress is stored locally in the browser
- No tracking or analytics by default
- Users can clear cache at any time

## Secure Deployment

When deploying:

1. Use HTTPS only
2. Enable security headers (.htaccess provided)
3. Keep dependencies updated
4. Monitor for security advisories
5. Use environment variables for configuration
6. Enable GZIP compression
7. Set appropriate cache headers

## Vulnerability Disclosure Timeline

Once a vulnerability is reported:

1. **Day 1**: Initial acknowledgment
2. **Day 7**: Assessment and confirmation
3. **Day 14**: Initial patch development
4. **Day 21**: Security release and CVE assignment (if applicable)
5. **Day 28**: Public disclosure

## Third-party Dependencies

This project uses the following key dependencies:

- **Jest**: Testing framework (actively maintained)
- **Stylus**: CSS preprocessor (stable)
- **Python3**: Optional integration (user-provided)

All dependencies are regularly audited for security vulnerabilities.

## Contact

For security concerns, contact the maintainers directly.

## Changelog

### Version 1.0.1

- Added Service Worker for offline support
- Implemented manifest.json for PWA
- Added security headers via .htaccess
- Enhanced cache control strategies

Thank you for helping keep Game Center secure!
