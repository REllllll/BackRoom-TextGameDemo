# Fix browser caching issues

## Problem statement

After updating `app.js` or `style.css`, a browser refresh still loads the old scripts because the browser caches the previous files.

## Solutions

### Option 1: Use a timestamp (already implemented, recommended for dev)

`index.html` dynamically loads the script with a timestamp:

```html
<script>
    // Dynamically load script and append timestamp to bust cache
    const script = document.createElement('script');
    script.src = `app.js?v=${Date.now()}`;
    document.body.appendChild(script);
</script>
```

This forces the latest script to load on every refresh.

### Option 2: Configure Nginx to disable caching (recommended for dev)

Add the following rules to the Nginx config to disable static asset caching:

```nginx
location ~* \.(js|css|html)$ {
    add_header Cache-Control "no-cache, no-store, must-revalidate";
    add_header Pragma "no-cache";
    add_header Expires "0";
    try_files $uri =404;
}
```

See `nginx.conf.example` for a full configuration.

**After applying the config, reload nginx:**
```bash
sudo nginx -t  # Test configuration
sudo nginx -s reload  # Reload configuration
```

### Option 3: Clear browser cache manually

If the above options are not suitable, clear the browser cache manually:

1. **Chrome/Edge**:
   - Press `Ctrl+Shift+Delete` (Windows/Linux) or `Cmd+Shift+Delete` (Mac)
   - Select "Cached images and files"
   - Click "Clear data"

2. **Firefox**:
   - Press `Ctrl+Shift+Delete` (Windows/Linux) or `Cmd+Shift+Delete` (Mac)
   - Select "Cache"
   - Click "Clear now"

3. **Hard refresh**:
   - Windows/Linux: `Ctrl+F5` or `Ctrl+Shift+R`
   - Mac: `Cmd+Shift+R`

### Option 4: Disable cache via devtools (during development)

In browser devtools:
1. Open devtools (F12)
2. Open the Network tab
3. Check "Disable cache"
4. Keep devtools open

## Recommended setup

- **Development**: Option 1 (timestamp) or Option 2 (nginx config)
- **Production**: Use versioning (e.g., `app.js?v=1.0.0`) and configure long-term caching

## Validate the fix

After modifying `app.js`:
1. Save the file
2. Refresh the browser (or use `Ctrl+F5` for hard refresh)
3. Open browser devtools → Network tab
4. Inspect the `app.js` request and confirm the new timestamp parameter
5. Ensure the latest file is loaded

