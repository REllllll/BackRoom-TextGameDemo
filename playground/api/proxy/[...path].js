// ============================================================================
// Vercel Serverless Function: API Proxy
// ============================================================================
// 将前端请求代理到后端 HTTP 服务器，解决混合内容问题
// ============================================================================

export default async function handler(req, res) {
  // 设置 CORS 头（先设置，以便错误响应也包含 CORS 头）
  res.setHeader('Access-Control-Allow-Origin', '*');
  res.setHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  res.setHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');
  res.setHeader('Access-Control-Max-Age', '86400');
  
  // 处理 OPTIONS 预检请求
  if (req.method === 'OPTIONS') {
    return res.status(200).end();
  }
  
  // 获取路径参数
  const { path } = req.query;
  
  // 从环境变量获取后端地址
  const backendUrl = process.env.BACKEND_URL;
  
  // 检查环境变量是否设置
  if (!backendUrl) {
    console.error('BACKEND_URL environment variable is not set');
    return res.status(500).json({ 
      error: 'Configuration error',
      message: 'BACKEND_URL environment variable is not set',
      hint: 'Please set BACKEND_URL in Vercel project settings'
    });
  }
  
  // 构建完整的后端 URL
  const pathSegments = Array.isArray(path) ? path.join('/') : (path || '');
  const targetUrl = `${backendUrl}/api/${pathSegments}`;
  
  // 处理查询参数（排除 path 参数，因为它是路由参数）
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
  
  // 记录请求信息（用于调试）
  console.log(`[Proxy] ${req.method} ${req.url} -> ${fullUrl}`);
  console.log(`[Proxy] Backend URL: ${backendUrl}`);
  
  try {
    // 创建超时控制器（20秒超时）
    const controller = new AbortController();
    const timeoutId = setTimeout(() => controller.abort(), 20000);
    
    // 准备请求选项
    const fetchOptions = {
      method: req.method,
      headers: {
        'Content-Type': 'application/json',
        'User-Agent': 'Vercel-Proxy/1.0'
      },
      signal: controller.signal
    };
    
    // 如果有请求体，添加到选项中
    if (req.method !== 'GET' && req.method !== 'HEAD' && req.body) {
      fetchOptions.body = JSON.stringify(req.body);
    }
    
    // 转发请求到后端服务器
    const response = await fetch(fullUrl, fetchOptions);
    
    // 清除超时定时器
    clearTimeout(timeoutId);
    
    // 检查响应状态
    if (!response.ok) {
      console.error(`[Proxy] Backend returned error: ${response.status} ${response.statusText}`);
    }
    
    // 获取响应数据
    let data;
    const contentType = response.headers.get('content-type');
    if (contentType && contentType.includes('application/json')) {
      data = await response.json();
    } else {
      data = await response.text();
    }
    
    // 返回响应
    return res.status(response.status).json(data);
  } catch (error) {
    // 详细的错误处理
    console.error('[Proxy] Error details:', {
      message: error.message,
      name: error.name,
      code: error.code,
      url: fullUrl,
      backendUrl: backendUrl
    });
    
    // 根据错误类型提供更详细的错误信息
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

