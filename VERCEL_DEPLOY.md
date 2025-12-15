# Vercel Deployment Guide

This guide explains how to deploy the **frontend** to Vercel and use a **Serverless Function proxy** to reach the backend HTTP API (avoiding browser mixed-content restrictions).

## Why a proxy is needed

- Vercel serves your frontend over **HTTPS**
- If your backend is only available over **HTTP**, browsers will block HTTPS pages from calling HTTP resources (mixed content)

## Solution

Use a Vercel Serverless Function as an HTTPS-to-HTTP proxy:

```
Frontend (HTTPS) -> Vercel Serverless Function (HTTPS) -> Backend (HTTP)
```

The proxy lives at `playground/api/proxy/[...path].js`.

## Deployment steps

### 1) Ensure these files exist

- `playground/api/proxy/[...path].js` - Serverless Function proxy
- `vercel.json` - Vercel config (repo root)
- `playground/` - static frontend assets

### 2) Create a Vercel project

1. Log in to Vercel
2. Add a new project and import your Git repository
3. Set:
   - **Framework Preset**: Other
   - **Root Directory**: `playground`
   - **Build Command**: empty (static)
   - **Output Directory**: empty

### 3) Configure environment variables

In your Vercel project settings, add:

```
BACKEND_URL=http://your-backend-server.com:8080
```

Notes:

- `BACKEND_URL` must include scheme + host + port
- Do **not** include `/api` (the proxy appends it)

### 4) Deploy

Push to your Git repo (auto-deploy), or click **Deploy** in Vercel.

### 5) Verify

Open the Vercel site and test:

- init game
- run commands
- fetch status
- load map

## Local development

When running locally, the frontend detects `localhost` and uses `/api` (via the nginx proxy, if configured). To test the Vercel proxy locally, use Vercel CLI:

```bash
npm i -g vercel
vercel dev
```

## Troubleshooting

### CORS errors

Check:

- proxy function CORS headers
- backend CORS configuration for your Vercel domain

### "fetch failed" / "Proxy error"

Most commonly:

- `BACKEND_URL` is missing or malformed
- backend service is not reachable (firewall, DNS, network)

Verify backend health:

```bash
curl http://localhost:8080/api/status
```

### 502 Bad Gateway

Likely causes:

- backend not running
- backend not reachable from Vercel
- `BACKEND_URL` invalid

### 404 Not Found

Check:

- `vercel.json` routing
- correct path: `/api/proxy/...` (frontend) -> `/api/...` (backend)

## Security notes

- Restrict `Access-Control-Allow-Origin` in production to your deployed domain
- Do not hardcode backend URLs; use env vars
- Prefer HTTPS on the backend when possible



