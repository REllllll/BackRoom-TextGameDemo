# Docker 使用说明

本项目使用 Docker Compose 来运行游戏，包含 SWI-Prolog 和 PDDL4J 环境。

## 前置要求

- Docker
- Docker Compose

**注意**：如果您的用户没有 Docker 权限，脚本会自动使用 `sudo`。您也可以通过设置环境变量手动控制：
```bash
# 强制使用 sudo
USE_SUDO=sudo ./scripts/dev.sh start

# 强制不使用 sudo（如果用户在 docker 组中）
USE_SUDO="" ./scripts/dev.sh start
```

## 使用方法

### 开发模式（推荐）

开发模式适合日常开发和调试，容器会保持运行并提供交互式 shell。

#### 1. 启动开发容器

```bash
# 使用开发配置启动
docker-compose -f docker-compose.dev.yml up -d --build

# 或者使用默认配置但进入 shell
docker-compose up -d --build
docker-compose exec game bash
```

#### 2. 进入开发容器

```bash
# 如果使用开发配置
docker-compose -f docker-compose.dev.yml exec game-dev bash

# 如果使用默认配置
docker-compose exec game bash
```

#### 3. 在容器内开发

容器内已安装以下工具：
- **SWI-Prolog**: `swipl`
- **PDDL4J**: `ff` 和 `pddl4j` 命令
- **编辑器**: `vim`, `nano`
- **版本控制**: `git`
- **其他工具**: `curl`, `less`, `ps` 等

**常用开发命令：**

```bash
# 运行游戏
swipl -s prolog/main.pl -g start

# 测试 PDDL 规划器
ff -o pddl/domains/adversary_domain.pddl -f pddl/problems/current_problem.pddl

# 编辑文件
vim prolog/main.pl
# 或
nano prolog/main.pl

# 查看进程
ps aux

# 检查 Java 版本
java -version

# 检查 Prolog 版本
swipl --version
```

#### 4. 文件同步

项目目录已挂载到容器的 `/workspace`，你在容器内的修改会立即反映到主机，反之亦然。

### 生产模式

#### 1. 构建并启动容器（直接运行游戏）

```bash
docker-compose up --build
```

#### 2. 在后台运行

```bash
docker-compose up -d --build
```

#### 3. 进入运行中的容器

```bash
docker-compose exec game bash
```

在容器内，你可以：
- 运行 Prolog: `swipl -s prolog/main.pl -g start`
- 使用 PDDL4J FF 规划器: `ff -o pddl/domains/adversary_domain.pddl -f pddl/problems/current_problem.pddl`
- 直接使用 PDDL4J: `pddl4j fr.uga.pddl4j.planners.statespace.FF domain.pddl problem.pddl`

#### 4. 停止容器

```bash
docker-compose down
```

#### 5. 清理（包括 volumes）

```bash
docker-compose down -v
```

## 开发工作流示例

### 典型开发流程

1. **启动开发容器**
   ```bash
   docker-compose -f docker-compose.dev.yml up -d
   ```

2. **进入容器**
   ```bash
   docker-compose -f docker-compose.dev.yml exec game-dev bash
   ```

3. **在容器内编辑和测试**
   ```bash
   # 编辑 Prolog 文件
   vim prolog/game_logic.pl
   
   # 测试修改
   swipl -s prolog/main.pl -g start
   
   # 测试 PDDL 规划
   ff -o pddl/domains/adversary_domain.pddl -f pddl/problems/initial_problem.pddl
   ```

4. **文件自动同步**
   - 容器内的 `/workspace` 与主机的项目目录同步
   - 使用你喜欢的编辑器在主机上编辑，在容器内测试
   - 或直接在容器内使用 vim/nano 编辑

### 使用 VS Code 远程开发

1. 安装 "Dev Containers" 扩展
2. 在 VS Code 中打开项目
3. 按 `F1`，选择 "Dev Containers: Attach to Running Container"
4. 选择 `liminal-logic-game-dev` 容器
5. 在容器内打开终端和文件进行开发

## 文件说明

- `docker-compose.yml`: 生产模式配置（直接运行游戏）
- `docker-compose.dev.yml`: 开发模式配置（交互式 shell）
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

