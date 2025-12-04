# 远程访问配置指南

本指南说明如何配置服务器以允许远程设备访问 Prolog Playground。

## 当前配置

- **服务器地址**：`0.0.0.0:8080`（容器内）
- **端口映射**：`8081:8080`（主机:容器）
- **内网 IP**：`10.1.20.10`
- **公网 IP**：`141.11.133.243`（如果服务器有公网 IP）

## 配置步骤

### 1. 确保服务器正在运行

```bash
# 进入容器
./scripts/dev.sh shell

# 启动服务器
cd /workspace
./scripts/start_playground.sh
```

### 2. 检查防火墙设置

#### Ubuntu/Debian (ufw)

```bash
# 检查防火墙状态
sudo ufw status

# 如果防火墙启用，允许 8081 端口
sudo ufw allow 8081/tcp

# 或者允许特定 IP 访问
sudo ufw allow from <远程IP> to any port 8081
```

#### CentOS/RHEL (firewalld)

```bash
# 检查防火墙状态
sudo firewall-cmd --state

# 允许 8081 端口
sudo firewall-cmd --permanent --add-port=8081/tcp
sudo firewall-cmd --reload
```

#### iptables

```bash
# 允许 8081 端口
sudo iptables -A INPUT -p tcp --dport 8081 -j ACCEPT
sudo iptables-save
```

### 3. 云服务器安全组配置

如果使用云服务器（如阿里云、腾讯云、AWS 等），需要在控制台配置安全组规则：

- **协议**：TCP
- **端口**：8081
- **源**：`0.0.0.0/0`（允许所有 IP）或特定 IP 地址

### 4. 访问地址

#### 从内网访问

- **内网地址**：`http://10.1.20.10:8081`
- **本地地址**：`http://localhost:8081`

#### 从公网访问

- **公网地址**：`http://141.11.133.243:8081`

**注意**：如果服务器没有公网 IP 或公网 IP 不正确，需要：
1. 检查云服务器控制台中的公网 IP 配置
2. 配置端口转发或 NAT 规则
3. 使用 VPN 或内网穿透工具（如 ngrok、frp 等）

### 5. 测试远程访问

#### 从远程设备测试

```bash
# 测试 API 端点
curl http://141.11.133.243:8081/api/status

# 测试初始化
curl -X POST http://141.11.133.243:8081/api/init
```

#### 在浏览器中访问

打开浏览器，访问：`http://141.11.133.243:8081`

## 安全建议

### 1. 限制访问 IP（推荐）

只允许特定 IP 访问：

```bash
# ufw 示例
sudo ufw allow from <允许的IP> to any port 8081

# iptables 示例
sudo iptables -A INPUT -p tcp -s <允许的IP> --dport 8081 -j ACCEPT
```

### 2. 使用 HTTPS（生产环境）

对于生产环境，建议：
1. 配置反向代理（如 Nginx）
2. 使用 SSL/TLS 证书
3. 配置域名

### 3. 使用 VPN

对于更安全的访问，建议使用 VPN：
1. 配置 VPN 服务器
2. 远程设备通过 VPN 连接
3. 通过内网地址访问

## 故障排查

### 无法从远程访问

1. **检查服务器是否运行**：
   ```bash
   ./scripts/dev.sh run ps aux | grep swipl
   ```

2. **检查端口映射**：
   ```bash
   docker ps | grep game-dev
   netstat -tuln | grep 8081
   ```

3. **检查防火墙**：
   ```bash
   sudo ufw status
   sudo iptables -L -n | grep 8081
   ```

4. **检查云服务器安全组**：
   - 登录云服务器控制台
   - 检查安全组规则
   - 确保 8081 端口已开放

5. **测试本地访问**：
   ```bash
   curl http://localhost:8081/api/status
   ```

6. **检查服务器绑定地址**：
   确保服务器绑定到 `0.0.0.0` 而不是 `127.0.0.1`

## 使用内网穿透（无公网 IP）

如果服务器没有公网 IP，可以使用内网穿透工具：

### ngrok 示例

```bash
# 安装 ngrok
# 在服务器上运行
ngrok http 8081

# ngrok 会提供一个公网地址，如：https://xxxxx.ngrok.io
```

### frp 示例

配置 frp 客户端和服务器，将内网端口映射到公网。

## 修改端口

如果需要使用其他端口，修改 `docker-compose.dev.yml`：

```yaml
ports:
  - "0.0.0.0:8082:8080"  # 改为 8082
```

然后重启容器：
```bash
./scripts/dev.sh stop
./scripts/dev.sh start
```

