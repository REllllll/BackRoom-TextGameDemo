# 解决浏览器缓存问题

## 问题描述

修改了 `app.js` 或 `style.css` 后，刷新浏览器时脚本没有更新，这是因为浏览器缓存了旧文件。

## 解决方案

### 方案 1：使用时间戳（已实现，推荐用于开发环境）

我已经修改了 `index.html`，使用动态加载脚本并添加时间戳：

```html
<script>
    // 动态加载脚本，添加时间戳避免缓存
    const script = document.createElement('script');
    script.src = `app.js?v=${Date.now()}`;
    document.body.appendChild(script);
</script>
```

这样每次刷新页面都会加载最新的脚本文件。

### 方案 2：配置 Nginx 禁用缓存（推荐用于开发环境）

在 nginx 配置中添加以下规则，禁用静态资源缓存：

```nginx
location ~* \.(js|css|html)$ {
    add_header Cache-Control "no-cache, no-store, must-revalidate";
    add_header Pragma "no-cache";
    add_header Expires "0";
    try_files $uri =404;
}
```

完整的配置示例请参考 `nginx.conf.example` 文件。

**应用配置后，需要重新加载 nginx：**
```bash
sudo nginx -t  # 测试配置
sudo nginx -s reload  # 重新加载配置
```

### 方案 3：手动清除浏览器缓存

如果上述方案都不适用，可以手动清除浏览器缓存：

1. **Chrome/Edge**：
   - 按 `Ctrl+Shift+Delete` (Windows/Linux) 或 `Cmd+Shift+Delete` (Mac)
   - 选择"缓存的图片和文件"
   - 点击"清除数据"

2. **Firefox**：
   - 按 `Ctrl+Shift+Delete` (Windows/Linux) 或 `Cmd+Shift+Delete` (Mac)
   - 选择"缓存"
   - 点击"立即清除"

3. **强制刷新**：
   - Windows/Linux: `Ctrl+F5` 或 `Ctrl+Shift+R`
   - Mac: `Cmd+Shift+R`

### 方案 4：使用开发者工具禁用缓存（开发时）

在浏览器开发者工具中：
1. 打开开发者工具（F12）
2. 打开 Network 标签
3. 勾选 "Disable cache" 选项
4. 保持开发者工具打开状态

## 推荐配置

- **开发环境**：使用方案 1（时间戳）或方案 2（nginx 配置）
- **生产环境**：使用版本号管理（如 `app.js?v=1.0.0`），并配置长期缓存

## 验证修复

修改 `app.js` 后：
1. 保存文件
2. 刷新浏览器（或使用 `Ctrl+F5` 强制刷新）
3. 打开浏览器开发者工具 → Network 标签
4. 查看 `app.js` 的请求，应该看到新的时间戳参数
5. 确认加载的是最新文件

