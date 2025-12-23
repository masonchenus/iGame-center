# Deployment Guide for Devs

This guide covers deploying Game Center to various platforms.

## Pre-Deployment Checklist

- [ ] All tests passing: `npm test`
- [ ] No console errors in development
- [ ] Bundle size optimized
- [ ] Security headers configured
- [ ] Environment variables set
- [ ] Cache strategy verified
- [ ] Accessibility tested
- [ ] Performance metrics acceptable

## Local Testing

Before deployment, test locally:

```bash
# Install dependencies
npm install

# Run tests
npm test

# Start development server
npm start

# Build for production
npm run build
```

## Deployment Platforms

### Netlify

1. **Connect repository**
   ```bash
   # Install Netlify CLI
   npm install -g netlify-cli
   
   # Deploy
   netlify deploy
   ```

2. **Configure in netlify.toml** ✓ (Already configured)

3. **Environment variables** (if needed)
   - Set in Netlify dashboard
   - Settings → Build & deploy → Environment

### Vercel

1. **Deploy**
   ```bash
   npm install -g vercel
   vercel
   ```

2. **Configuration** ✓ (Already configured in vercel.json)

3. **Environment variables**
   - Settings → Environment Variables

### GitHub Pages

1. **Configure repository**
   ```bash
   git add .
   git commit -m "Ready for deployment"
   git push origin main
   ```

2. **Enable Pages**
   - Repository Settings → Pages
   - Select branch: `main`
   - Deploy

3. **Custom domain** (optional)
   - Add CNAME file with domain name
   - Configure DNS CNAME record

### Self-Hosted (Apache)

1. **Prerequisites**
   - Apache 2.4+ with mod_rewrite
   - PHP 7.0+ (optional)
   - SSL certificate (recommended)

2. **Upload files**
   ```bash
   scp -r ./* user@server:/var/www/html/
   ```

3. **Configure config/.htaccess** ✓ (Already configured)

4. **Set permissions**
   ```bash
   chmod 755 /var/www/html/
   chmod 644 /var/www/html/*
   ```

5. **Enable HTTPS**
   ```apache
   # In .htaccess or Apache config
   <IfModule mod_ssl.c>
     SSLEngine on
   </IfModule>
   ```

### Self-Hosted (Nginx)

1. **Configure Nginx**
   ```nginx
   server {
     listen 80;
     server_name example.com;
     
     root /var/www/game-center;
     index index.html;
     
     # Serve static files with long cache
     location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2)$ {
       expires 1y;
       add_header Cache-Control "public, immutable";
     }
     
     # Serve HTML without cache
     location ~ \.html$ {
       expires -1;
       add_header Cache-Control "public, max-age=0, must-revalidate";
     }
     
     # Route all requests to index.html (SPA)
     location / {
       try_files $uri /index.html;
     }
     
     # Security headers
     add_header X-Frame-Options "SAMEORIGIN" always;
     add_header X-Content-Type-Options "nosniff" always;
     add_header X-XSS-Protection "1; mode=block" always;
     add_header Referrer-Policy "strict-origin-when-cross-origin" always;
   }
   ```

2. **Enable HTTPS with Let's Encrypt**
   ```bash
   sudo apt-get install certbot python3-certbot-nginx
   sudo certbot --nginx -d example.com
   ```

### Docker Deployment

1. **Create Dockerfile**
   ```dockerfile
   FROM node:18-alpine
   WORKDIR /app
   COPY package*.json ./
   RUN npm ci --only=production
   COPY . .
   EXPOSE 3000
   CMD ["npm", "start"]
   ```

2. **Build image**
   ```bash
   docker build -t game-center:latest .
   ```

3. **Run container**
   ```bash
   docker run -p 3000:3000 game-center:latest
   ```

### AWS S3 + CloudFront

1. **Create S3 bucket**
   - Enable static website hosting
   - Block all public access (use CloudFront instead)

2. **Upload files**
   ```bash
   aws s3 sync ./ s3://my-bucket/ --acl public-read
   ```

3. **Configure CloudFront**
   - Origin: S3 bucket
   - Behavior: Cache static assets, no-cache HTML
   - SSL certificate: Use default or ACM

## Post-Deployment

### Verification

1. **Check deployment**
   - Visit URL and verify functionality
   - Test all game modes
   - Check network requests

2. **Monitor**
   - Check error logs
   - Monitor performance metrics
   - Test from different devices

3. **Performance**
   ```bash
   # Use Google PageSpeed Insights
   https://pagespeed.web.dev/
   
   # Check Core Web Vitals
   https://web.dev/measure/
   ```

### Updating Cache

After deployment, clear cache:

```bash
# Netlify
netlify cache:clear

# Vercel
vercel env pull

# GitHub Pages (automatic)

# Self-hosted (manual)
# Update CACHE_NAME in sw.js, e.g., 'game-center-v2'
```

## Continuous Deployment

### GitHub Actions

Create `.github/workflows/deploy.yml`:

```yaml
name: Deploy

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - uses: actions/setup-node@v3
        with:
          node-version: '18'
      - run: npm ci
      - run: npm test
      - name: Deploy
        run: npm run deploy
        env:
          DEPLOY_TOKEN: ${{ secrets.DEPLOY_TOKEN }}
```

## Rollback Strategy

If deployment has issues:

1. **Quick rollback**
   - Use platform's rollback feature
   - Redeploy previous version
   - Update Service Worker cache version

2. **Prevention**
   - Test locally before deploying
   - Use staging environment
   - Implement feature flags

## Environment Variables

Example `.env` file:

```env
# API Configuration
REACT_APP_API_BASE=https://api.example.com
REACT_APP_ENV=production

# Feature Flags
REACT_APP_ENABLE_CHAOS_WEAPON=true
REACT_APP_ENABLE_ACHIEVEMENTS=true

# Analytics (optional)
REACT_APP_GOOGLE_ANALYTICS_ID=UA-XXXXXXXX-X
```

## Security Checklist

- [ ] HTTPS enabled
- [ ] Security headers configured
- [ ] Secrets not in code
- [ ] No API keys exposed
- [ ] Dependencies audited
- [ ] Service Worker secure
- [ ] CORS configured
- [ ] Rate limiting enabled

## Monitoring & Logs

### Sentry (Error Tracking)

```javascript
import * as Sentry from '@sentry/browser';

Sentry.init({
  dsn: 'https://key@sentry.io/project',
  environment: 'production'
});
```

### Google Analytics

```html
<!-- Global Site Tag (gtag.js) -->
<script async src="https://www.googletagmanager.com/gtag/js?id=GA_ID"></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());
  gtag('config', 'GA_ID');
</script>
```

## Troubleshooting

### 404 on Refresh

**Problem**: Game loads but refreshing shows 404

**Solution**: Configure server to route all requests to index.html

- Netlify: ✓ Configured in netlify.toml
- Vercel: ✓ Configured in vercel.json
- Apache: ✓ Configured in .htaccess
- Nginx: Configure as shown above

### Service Worker Issues

**Problem**: Updates not showing

**Solution**: Clear cache and update CACHE_NAME version

```javascript
// In sw.js
const CACHE_NAME = 'game-center-v2'; // Increment version
```

### Slow Performance

**Problem**: Game sluggish after deployment

**Solution**:
- Verify minification
- Check network compression
- Test from different locations
- Monitor Core Web Vitals

## Success Metrics

- **Uptime**: 99.9%+
- **Load time**: < 2 seconds
- **LCP**: < 2.5 seconds
- **FID**: < 100ms
- **CLS**: < 0.1

## Support

For deployment issues:
- Check platform documentation
- Review error logs
- Test locally first
- Consult DevTools Network tab

# Trobuleshooting Tips 

1. **Clear browser cache**
2. **Disable ad blockers**
3. **Try incognito mode**
4. **Check network connection**
5. **Verify server status**
6. **Review platform docs**  


*** If all fails, edit the issues.txt file and submit a bug report! ***


# Teamwork

- **Communication**: Regular check-ins, Slack channel.
- **Code Reviews**: Pull Requests for major changes.
- **Documentation**: Keep this README up-to-date.
- **Testing**: Automated tests and manual QA sessions.
- **Version Control**: Git branching model (e.g., feature branches).
- **Security**: Periodic security audits and updates.
- **Accessibility**: Ensure WCAG compliance during development.
- **Continuous Integration**: Automate testing and deployment pipelines.
- **Feedback Loop**: Collect feedback from users and iterate quickly.
- **Collaboration Tools**: Utilize tools like Trello, Asana, or Jira for project management.
- **Knowledge Sharing**: Share best practices and lessons learned within the team.
- **Conflict Resolution**: Address conflicts promptly through open communication and compromise.
- **Team Building Activities**: Organize occasional team-building events to foster camaraderie.
- **Mentorship Program**: Pair experienced developers with newer members for guidance.
- **Open Source Contribution**: Encourage contributions back to open-source projects related to our work.
- **Regular Retrospectives**: Conduct regular retrospectives to reflect on past achievements and identify areas for improvement.
- **Cross-functional Collaboration**: Foster collaboration across different departments (designers, testers, etc.).
- **Remote Work Culture**: Promote a positive remote work culture by setting clear expectations and maintaining transparency.
- **Professional Development**: Provide opportunities for professional growth and skill enhancement.
- ~~ **Professional Development**: Provide opportunities for professional growth and skill enhancement.~~ ~~Oh wait, i forgot we already did that!~~
If you know how to fix a bug, submit a pull request or edit the file: "bugs.txt".
