# Remote Access Guide

This guide describes how to configure a host so that remote devices can access the Prolog Playground.

## Current example setup

- **Bind address** (inside container): `0.0.0.0:8080`
- **Port mapping**: `8081:8080` (host:container)
- **LAN IP**: `10.1.20.10`
- **Public IP**: `141.11.133.243` (if the host has a public IP)

## Steps

### 1) Ensure the server is running

```bash
./scripts/dev.sh shell
cd /workspace
./scripts/start_playground.sh
```

### 2) Firewall configuration

#### Ubuntu/Debian (ufw)

```bash
sudo ufw status
sudo ufw allow 8081/tcp
sudo ufw allow from <remote-ip> to any port 8081
```

#### CentOS/RHEL (firewalld)

```bash
sudo firewall-cmd --state
sudo firewall-cmd --permanent --add-port=8081/tcp
sudo firewall-cmd --reload
```

#### iptables

```bash
sudo iptables -A INPUT -p tcp --dport 8081 -j ACCEPT
sudo iptables-save
```

### 3) Cloud security group

If you run on a cloud provider (AWS/Azure/GCP/etc), open:

- Protocol: TCP
- Port: 8081
- Source: `0.0.0.0/0` (or a restricted IP range)

### 4) Access URLs

LAN:

- `http://10.1.20.10:8081`
- `http://localhost:8081`

Public:

- `http://141.11.133.243:8081`

If you do not have a public IP, consider VPN or tunneling tools (e.g. ngrok, frp).

### 5) Test

```bash
curl http://141.11.133.243:8081/api/status
curl -X POST http://141.11.133.243:8081/api/init
```

## Security recommendations

- Restrict access to specific IPs when possible
- Use HTTPS via a reverse proxy (nginx) in production
- Prefer VPN for private access

## Troubleshooting

1. Check the server process:

```bash
./scripts/dev.sh run ps aux | grep swipl
```

2. Check port mapping / listening ports:

```bash
docker ps | grep game-dev
netstat -tuln | grep 8081
```

3. Check firewall rules:

```bash
sudo ufw status
sudo iptables -L -n | grep 8081
```

4. Ensure the server binds to `0.0.0.0` (not `127.0.0.1`).

## Changing the port

Edit `docker-compose.dev.yml` and restart:

```bash
./scripts/dev.sh stop
./scripts/dev.sh start
```



