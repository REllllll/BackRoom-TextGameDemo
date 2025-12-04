# Docker 使用说明

本项目使用 Docker Compose 来运行游戏，包含 SWI-Prolog 和 PDDL4J 环境。

## 前置要求

- Docker
- Docker Compose

## 使用方法

### 1. 构建并启动容器

```bash
docker-compose up --build
```

### 2. 在后台运行

```bash
docker-compose up -d --build
```

### 3. 进入容器交互式运行

```bash
docker-compose run --rm game swipl -s prolog/main.pl -g start
```

### 4. 进入容器 shell

```bash
docker-compose exec game bash
```

在容器内，你可以：
- 运行 Prolog: `swipl -s prolog/main.pl -g start`
- 使用 PDDL4J FF 规划器: `ff -o pddl/domains/adversary_domain.pddl -f pddl/problems/current_problem.pddl`
- 直接使用 PDDL4J: `pddl4j fr.uga.pddl4j.planners.statespace.FF domain.pddl problem.pddl`

### 5. 停止容器

```bash
docker-compose down
```

### 6. 清理（包括 volumes）

```bash
docker-compose down -v
```

## 文件说明

- `docker-compose.yml`: Docker Compose 配置文件
- `Dockerfile.game`: 包含 SWI-Prolog 和 PDDL4J 的统一镜像
- `.dockerignore`: Docker 构建时忽略的文件

## 技术细节

### PDDL4J 安装

PDDL4J 在构建时从 GitHub 克隆并构建：
- 源仓库: https://github.com/pellierd/pddl4j.git
- 构建工具: Gradle
- 最终 jar 文件位置: `/opt/pddl4j/pddl4j.jar`

### FF 规划器包装脚本

为了兼容原有的 `ff -o DOMAIN -f PROBLEM` 命令格式，创建了包装脚本 `/usr/local/bin/ff`，它会：
1. 解析 `-o` 和 `-f` 参数
2. 调用 PDDL4J 的 FF 规划器
3. 输出规划结果

### 数据持久化

- 项目文件通过 volume 挂载到容器内的 `/workspace`
- Gradle 缓存保存在 Docker volume `pddl4j-cache` 中，加速后续构建

## 故障排除

### 如果 PDDL4J 构建失败

1. 检查网络连接（需要访问 GitHub）
2. 清理并重新构建：
   ```bash
   docker-compose down -v
   docker-compose build --no-cache
   ```

### 如果规划器无法找到

确保在容器内 `ff` 命令可用：
```bash
docker-compose exec game which ff
docker-compose exec game ff --help
```

### 查看日志

```bash
docker-compose logs game
```

