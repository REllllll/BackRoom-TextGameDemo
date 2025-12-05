# Prolog Playground

基于 HTML 的 Prolog 游戏 playground，通过 HTTP server 提供 REST API。

## 启动服务器

### 在 Docker 容器中启动

1. **启动开发容器**：
   ```bash
   ./scripts/dev.sh start
   ```

2. **进入容器**：
   ```bash
   ./scripts/dev.sh shell
   ```

3. **在容器内启动服务器**：
   ```bash
   cd /workspace
   swipl -s prolog/http_server.pl -g "game_http_server:start_server(8080)"
   ```

### 从主机访问

服务器绑定到 `0.0.0.0:8080`，允许外部访问。

- **从容器内访问**：`http://localhost:8080`
- **从主机访问**：`http://localhost:8081`（端口映射在 docker-compose.dev.yml 中配置）

如果主机 8080 端口被占用，可以修改 `docker-compose.dev.yml` 中的端口映射：
```yaml
ports:
  - "8081:8080"  # 主机端口:容器端口
```

然后从主机访问：`http://localhost:8081`

## API 端点

- `GET /api/status` - 获取游戏状态
- `POST /api/init` - 初始化游戏
- `POST /api/command` - 执行游戏命令
- `GET /api/map` - 获取地图信息
- `GET /` - 访问 playground 界面

## 使用示例

```bash
# 初始化游戏
curl -X POST http://localhost:8081/api/init

# 获取游戏状态
curl http://localhost:8081/api/status

# 执行命令
curl -X POST -H "Content-Type: application/json" \
  -d '{"command":"look"}' \
  http://localhost:8081/api/command
```

