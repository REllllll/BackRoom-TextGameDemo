// ============================================================================
// Vercel Serverless Function: API Proxy
// ============================================================================
// 将前端请求代理到后端 HTTP 服务器，解决混合内容问题
// ============================================================================

export default async function handler(req, res) {
  // 获取路径参数
  const { path } = req.query;
  
  // 从环境变量获取后端地址，如果没有则使用默认值
  const backendUrl = process.env.BACKEND_URL || 'http://localhost:8080';
  
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
  
  try {
    // 准备请求选项
    const fetchOptions = {
      method: req.method,
      headers: {
        'Content-Type': 'application/json',
      }
    };
    
    // 如果有请求体，添加到选项中
    if (req.method !== 'GET' && req.method !== 'HEAD' && req.body) {
      fetchOptions.body = JSON.stringify(req.body);
    }
    
    // 转发请求到后端服务器
    const response = await fetch(fullUrl, fetchOptions);
    
    // 获取响应数据
    let data;
    const contentType = response.headers.get('content-type');
    if (contentType && contentType.includes('application/json')) {
      data = await response.json();
    } else {
      data = await response.text();
    }
    
    // 设置 CORS 头，允许所有来源（生产环境可以限制为特定域名）
    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
    res.setHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');
    res.setHeader('Access-Control-Max-Age', '86400');
    
    // 处理 OPTIONS 预检请求
    if (req.method === 'OPTIONS') {
      return res.status(200).end();
    }
    
    // 返回响应
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

