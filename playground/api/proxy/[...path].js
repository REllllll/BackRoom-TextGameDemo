// ============================================================================
// Vercel Serverless Function: API Proxy
// ============================================================================
// Proxy frontend requests to backend HTTP server to avoid mixed content
// ============================================================================

export default async function handler(req, res) {
  // Set CORS headers early so error responses include them
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');
  res.setHeader('Access-Control-Max-Age', '86400');
  
  // Handle OPTIONS preflight
  if (req.method === 'OPTIONS') {
    return res.status(200).end();
  }
  
  // Get path parameters
  const { path } = req.query;
  
  // Get backend URL from environment
  const backendUrl = process.env.BACKEND_URL;
  
  // Ensure environment variable is set
  if (!backendUrl) {
    console.error('BACKEND_URL environment variable is not set');
    return res.status(500).json({ 
      error: 'Configuration error',
      message: 'BACKEND_URL environment variable is not set',
      hint: 'Please set BACKEND_URL in Vercel project settings'
    });
  }
  
  // Build full backend URL
  const pathSegments = Array.isArray(path) ? path.join('/') : (path || '');
  const targetUrl = `${backendUrl}/api/${pathSegments}`;
  
  // Handle query params (exclude path because it is the route param)
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
  
  const queryString = queryParams.toString();
  const fullUrl = queryString ? `${targetUrl}?${queryString}` : targetUrl;
  
  // Log request info for debugging
  console.log(`[Proxy] ${req.method} ${req.url} -> ${fullUrl}`);
  console.log(`[Proxy] Backend URL: ${backendUrl}`);
  
  try {
    // Create timeout controller (20s timeout)
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), 20000);
    
    // Prepare request options
    const fetchOptions = {
      method: req.method,
      headers: {
        'Content-Type': 'application/json',
        'User-Agent': 'Vercel-Proxy/1.0'
      },
      signal: controller.signal
    };
    
    // Attach body when present
    if (req.method !== 'GET' && req.method !== 'HEAD' && req.body) {
      fetchOptions.body = JSON.stringify(req.body);
    }
    
    // Forward request to backend
    const response = await fetch(fullUrl, fetchOptions);
    
    // Clear timeout timer
    clearTimeout(timeoutId);
    
    // Check response status
    if (!response.ok) {
      console.error(`[Proxy] Backend returned error: ${response.status} ${response.statusText}`);
    }
    
    // Read response payload
    let data;
    const contentType = response.headers.get('content-type');
    if (contentType && contentType.includes('application/json')) {
      data = await response.json();
    } else {
      data = await response.text();
    }
    
    // Return response
    return res.status(response.status).json(data);
  } catch (error) {
    // Detailed error handling
    console.error('[Proxy] Error details:', {
      message: error.message,
      name: error.name,
      code: error.code,
      url: fullUrl,
      backendUrl: backendUrl
    });
    
    // Provide error-specific messages
    let errorMessage = error.message;
    let errorHint = '';
    
    if (error.name === 'AbortError' || error.message.includes('timeout')) {
      errorMessage = 'Request timeout';
      errorHint = 'The backend server did not respond within 20 seconds. Please check if the server is running and accessible.';
    } else if (error.message.includes('ECONNREFUSED') || error.message.includes('ENOTFOUND')) {
      errorMessage = 'Connection failed';
      errorHint = `Cannot connect to backend server at ${backendUrl}. Please check if the server is running and the BACKEND_URL is correct.`;
    } else if (error.message.includes('fetch failed')) {
      errorMessage = 'Network error';
      errorHint = `Failed to fetch from ${fullUrl}. Please check network connectivity and firewall settings.`;
    }
    
    return res.status(500).json({ 
      error: 'Proxy error',
      message: errorMessage,
      hint: errorHint,
      backendUrl: backendUrl,
      targetUrl: fullUrl,
      // 在开发环境或特定条件下显示更多调试信息
      details: process.env.VERCEL_ENV === 'development' ? {
        originalError: error.message,
        errorName: error.name,
        errorCode: error.code
      } : undefined
    });
  }
}
