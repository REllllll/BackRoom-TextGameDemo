# Vercel 部署指南

本指南说明如何将前端部署到 Vercel，并通过 Serverless Function 代理访问后端 HTTP 服务器。

## 问题背景

- Vercel 使用 HTTPS 部署前端
- 后端服务器在中国境内，只能使用 HTTP
- 浏览器会阻止 HTTPS 页面请求 HTTP 资源（混合内容问题）

## 解决方案

使用 Vercel Serverless Functions 作为代理，将前端的 HTTPS 请求转发到后端的 HTTP 服务器。

## 部署步骤

### 1. 准备项目

确保项目包含以下文件：
- `playground/api/proxy/[...path].js` - Serverless Function 代理
- `vercel.json` - Vercel 配置文件（在项目根目录）
- `playground/` - 前端文件目录

### 2. 在 Vercel 中创建项目

1. 登录 [Vercel](https://vercel.com)
2. 点击 "Add New Project"
3. 导入你的 Git 仓库
4. 配置项目设置：
   - **Framework Preset**: Other
   - **Root Directory**: `playground`（重要：设置为 playground 目录）
   - **Build Command**: 留空（静态文件无需构建）
   - **Output Directory**: 留空

### 3. 配置环境变量

在 Vercel 项目设置中添加环境变量：

1. 进入项目设置 → Environment Variables
2. 添加以下变量：

```
BACKEND_URL=http://your-backend-server.com:8080
```

**重要提示**：
- `BACKEND_URL` 应该是完整的后端服务器地址（包含协议和端口）
- 不要包含 `/api` 路径，代理函数会自动添加
- 例如：`http://123.456.789.0:8080` 或 `http://backend.example.com:8080`

### 4. 部署

1. 推送代码到 Git 仓库
2. Vercel 会自动检测并部署
3. 或者点击 "Deploy" 手动触发部署

### 5. 验证部署

部署完成后，访问你的 Vercel 域名，测试以下功能：
- 初始化游戏
- 执行命令
- 查看游戏状态
- 查看地图

## 工作原理

```
前端 (HTTPS) → Vercel Serverless Function (HTTPS) → 后端 (HTTP)
```

1. 前端发送请求到 `/api/proxy/status`
2. Vercel Serverless Function 接收请求
3. Function 转发请求到后端服务器（使用环境变量 `BACKEND_URL`）
4. 后端响应返回给 Function
5. Function 将响应返回给前端

## 本地开发

本地开发时，前端会检测到 `localhost`，自动使用 `/api` 路径（通过 nginx 代理）。

如果需要测试 Vercel 代理功能，可以使用 Vercel CLI：

```bash
# 安装 Vercel CLI
npm i -g vercel

# 在项目根目录运行
vercel dev
```

## 环境变量说明

### `BACKEND_URL`（必需）

后端服务器的完整地址，格式：
```
http://hostname:port
```

示例：
```
http://123.456.789.0:8080
http://backend.example.com:8080
```

### 可选：前端直接连接后端

如果后端支持 HTTPS 和 CORS，可以通过 meta 标签或全局变量配置前端直接连接：

在 `index.html` 中添加：
```html
<meta name="api-url" content="https://your-backend-server.com">
```

或者在页面加载前设置：
```javascript
window.__ENV__ = { API_URL: 'https://your-backend-server.com' };
```

## 故障排查

### 1. CORS 错误

如果遇到 CORS 错误，检查：
- Serverless Function 是否正确设置了 CORS 头
- 后端服务器是否允许来自 Vercel 域名的请求

### 2. 502 Bad Gateway

可能原因：
- `BACKEND_URL` 环境变量未设置或格式错误
- 后端服务器无法访问（防火墙、网络问题）
- 后端服务器未运行

### 3. 404 Not Found

检查：
- `vercel.json` 路由配置是否正确
- API 路径是否正确（应该是 `/api/proxy/...`）

### 4. 超时

如果请求超时，可能是：
- 后端服务器响应慢
- 网络延迟
- Vercel Function 超时（默认 10 秒，Pro 计划可延长）

## 安全建议

1. **限制 CORS 来源**（生产环境）：
   修改 `api/proxy/[...path].js` 中的 CORS 设置：
   ```javascript
   res.setHeader('Access-Control-Allow-Origin', 'https://your-app.vercel.app');
   ```

2. **使用环境变量**：
   不要在代码中硬编码后端地址

3. **HTTPS 后端**（如果可能）：
   如果后端可以配置 HTTPS，建议使用 HTTPS，避免代理层

## 相关文件

- `playground/api/proxy/[...path].js` - Serverless Function 代理实现
- `vercel.json` - Vercel 配置文件（在项目根目录）
- `playground/app.js` - 前端 API 调用逻辑
- `.env.example` - 环境变量示例

## 目录结构

```
项目根目录/
├── vercel.json          # Vercel 配置（在根目录）
├── playground/          # Vercel 根目录
│   ├── index.html
│   ├── app.js
│   ├── style.css
│   └── api/            # Serverless Functions
│       └── proxy/
│           └── [...path].js
└── ...
```

