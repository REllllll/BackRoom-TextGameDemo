// ============================================================================
// Vercel Serverless Function: API Proxy
// ============================================================================
// Proxy frontend requests to backend HTTP server to avoid mixed content
// ============================================================================

export default async function handler(req, res) {
  // Get path parameters
  const { path } = req.query;
  
  // Read backend address from env; use default if missing
  const backendUrl = process.env.BACKEND_URL || 'http://localhost:8080';
  
  // Build full backend URL
  const pathSegments = Array.isArray(path) ? path.join('/') : (path || '');
  const targetUrl = `${backendUrl}/api/${pathSegments}`;
  
  // Handle query params (exclude path because it is the route param)
  // Use explicit filtering to avoid false matches in other param values containing "path="
  const queryParams = new URLSearchParams();
  Object.keys(req.query).forEach(key => {
    if (key !== 'path') {
      const value = req.query[key];
      if (Array.isArray(value)) {
        value.forEach(v => queryParams.append(key, v));
      } else {
        queryParams.append(key, value);
      }
    }
  });
  
  // Build query string (toString() returns empty string if all filtered)
  const queryString = queryParams.toString();
  // Append ? only when query string is non-empty to avoid trailing ?
  const fullUrl = queryString && queryString.length > 0 
    ? `${targetUrl}?${queryString}` 
    : targetUrl;
  
  try {
    // Prepare request options
    const fetchOptions = {
      method: req.method,
      headers: {
        'Content-Type': 'application/json',
      }
    };
    
    // Attach body when present
    if (req.method !== 'GET' && req.method !== 'HEAD' && req.body) {
      fetchOptions.body = JSON.stringify(req.body);
    }
    
    // Forward request to backend
    const response = await fetch(fullUrl, fetchOptions);
    
    // Read response payload
    let data;
    const contentType = response.headers.get('content-type');
    if (contentType && contentType.includes('application/json')) {
      data = await response.json();
    } else {
      data = await response.text();
    }
    
    // Set CORS headers; narrow in production if needed
    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
    res.setHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');
    res.setHeader('Access-Control-Max-Age', '86400');
    
    // Handle OPTIONS preflight
    if (req.method === 'OPTIONS') {
      return res.status(200).end();
    }
    
    // Return response
    res.status(response.status).json(data);
  } catch (error) {
    console.error('Proxy error:', error);
    res.status(500).json({ 
      error: 'Proxy error',
      message: error.message,
      details: process.env.NODE_ENV === 'development' ? error.stack : undefined
    });
  }
}
