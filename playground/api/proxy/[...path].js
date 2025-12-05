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
  // 在 Vercel 的 [...path].js 中，路径参数通过 req.query.path 传递
  let pathParam = req.query.path;
  
  // 如果 path 参数不存在，尝试从 URL 中解析
  // 例如：/api/proxy/command -> command
  if (!pathParam && req.url) {
    const urlMatch = req.url.match(/^\/api\/proxy\/(.+?)(?:\?|$)/);
    if (urlMatch) {
      pathParam = urlMatch[1];
    }
  }
  
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
  
  // 构建路径段
  // pathParam 可能是数组（多个路径段）或字符串（单个路径段）或 undefined
  let pathSegments = '';
  if (pathParam) {
    if (Array.isArray(pathParam)) {
      pathSegments = pathParam.join('/');
    } else {
      pathSegments = String(pathParam);
    }
  }
  
  // 构建完整的后端 URL
  // 如果 pathSegments 为空，则只使用 /api
  const targetUrl = pathSegments 
    ? `${backendUrl}/api/${pathSegments}`
    : `${backendUrl}/api`;
  
  // 处理查询参数（排除 path 参数，因为它是路由参数）
  // 使用显式过滤而不是正则表达式，避免错误匹配其他参数值中包含 "path=" 的情况
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
  
  // 获取查询字符串（如果所有参数都被过滤掉，toString() 会返回空字符串）
  const queryString = queryParams.toString();
  // 只有在查询字符串非空时才添加 ?，避免生成形如 http://example.com/api/test? 的无效 URL
  const fullUrl = queryString && queryString.length > 0 
    ? `${targetUrl}?${queryString}` 
    : targetUrl;
  
  // 记录请求信息（用于调试）
  console.log(`[Proxy] ${req.method} ${req.url}`);
  console.log(`[Proxy] Path param:`, pathParam);
  console.log(`[Proxy] Path segments:`, pathSegments);
  console.log(`[Proxy] Backend URL: ${backendUrl}`);
  console.log(`[Proxy] Target URL: ${targetUrl}`);
  console.log(`[Proxy] Full URL: ${fullUrl}`);
  
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
      // 添加调试信息帮助排查问题
      debug: {
        pathParam: pathParam,
        pathSegments: pathSegments,
        originalUrl: req.url,
        queryParams: Object.keys(req.query)
      },
      // 在开发环境或特定条件下显示更多调试信息
      details: process.env.VERCEL_ENV === 'development' ? {
        originalError: error.message,
        errorName: error.name,
        errorCode: error.code
      } : undefined
    });
  }
}

