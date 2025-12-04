# Liminal Logic: Escape from Level 0

一个基于 Prolog 和 PDDL 的"The Backrooms"（后室）逃脱游戏项目。

## 项目简介

本项目是一个结合了 **Prolog**（基于逻辑的知识表示）和 **PDDL**（自动规划）的文字冒险游戏。玩家扮演一名不幸"切出"现实世界的角色，掉进了 Level 0（后室的大堂），需要在理智耗尽或被实体抓住之前找到出口。

### 核心特性

- **Prolog 知识库**：使用逻辑编程表示游戏世界的静态和动态事实
- **PDDL 规划**：使用自动规划控制敌对实体（The Howler）的行为
- **动态交互**：Prolog 和 PDDL 之间的实时交互，实现智能对手
- **规则推理**：基于逻辑的约束检查（如需要手电筒才能进入黑暗房间）

## 项目结构

```
Knowledge Representation/
├── README.md                          # 项目说明文档（本文件）
├── prolog/                            # Prolog 知识库和游戏逻辑
│   ├── knowledge_base.pl             # 静态知识库（房间、连接、物品定义）
│   ├── game_state.pl                 # 动态状态管理（玩家位置、实体位置、理智值等）
│   ├── game_logic.pl                 # 游戏逻辑（移动、使用物品、交互）
│   ├── win_conditions.pl             # 胜利和失败条件
│   ├── pddl_interface.pl             # Prolog 与 PDDL 的接口模块
│   └── main.pl                       # 主程序入口
├── pddl/                              # PDDL 规划文件
│   ├── domains/
│   │   └── adversary_domain.pddl     # 实体（The Howler）的域定义
│   └── problems/
│       ├── initial_problem.pddl      # 初始问题文件模板
│       └── .gitkeep                  # 保持目录（动态生成的问题文件）
├── scripts/                           # 辅助脚本
│   ├── generate_problem.pl           # 从 Prolog 状态生成 PDDL problem
│   └── run_game.sh                   # 游戏启动脚本
└── docs/                              # 文档目录
    └── design.md                     # 设计文档
```

## 文件说明

### Prolog 文件

- **knowledge_base.pl**: 包含房间定义、房间连接、物品属性等静态事实
- **game_state.pl**: 管理动态事实（`at_player/1`, `at_entity/1`, `sanity/1`, `holding/1` 等）
- **game_logic.pl**: 实现游戏命令（`move`, `take`, `drop`, `use` 等）和规则推理
- **win_conditions.pl**: 定义胜利条件（到达 Manila Room）和失败条件（理智耗尽、被抓）
- **pddl_interface.pl**: Prolog 与 PDDL 的接口模块，负责生成 PDDL problem、调用规划器、解析结果
- **main.pl**: 主程序，整合所有模块，处理游戏循环和 PDDL 交互

### PDDL 文件

- **adversary_domain.pddl**: 定义实体动作（`move`, `listen`, `chase`）和谓词
- **initial_problem.pddl**: 初始问题模板，包含初始状态和目标

### 脚本文件

- **generate_problem.pl**: Prolog 脚本，读取当前游戏状态并生成 PDDL problem 文件
- **run_game.sh**: Shell 脚本，用于启动游戏（调用 Prolog 和 PDDL 规划器）

## 安装要求

### 必需软件

1. **SWI-Prolog** (>= 7.0)
   - 下载地址：https://www.swi-prolog.org/
   - macOS: `brew install swi-prolog`
   - Linux: `sudo apt-get install swi-prolog`
   - Windows: 从官网下载安装包

2. **PDDL 规划器**（可选，用于实体 AI）
   - Fast-Forward (FF): http://fai.cs.uni-saarland.de/hoffmann/ff.html
   - 或其他兼容的 PDDL 规划器

## 使用方法

### 快速开始

1. 确保已安装 SWI-Prolog

2. 使用启动脚本运行游戏：
   ```bash
   ./scripts/run_game.sh
   ```

3. 或者直接在 Prolog 中加载：
   ```bash
   swipl -s prolog/main.pl -g start
   ```

### 游戏命令

- `move(direction)` - 向指定方向移动（north, south, east, west）
- `take(item)` - 拾取物品
- `drop(item)` - 丢弃物品
- `use(item)` - 使用物品
- `look` - 查看当前位置和周围环境
- `quit` - 退出游戏

### 示例

```prolog
?- start.
% 游戏开始...

> look.
You are in start_point.
Exits: east -> yellow_hallway
Items here: none
Sanity: 100

> move(east).
You move to yellow_hallway.

> take(almond_water).
You pick up the almond_water.

> use(almond_water).
You drink the almond water. Your sanity increases.
```

## 游戏机制

### 地图结构

游戏包含以下房间：
- **start_point**: 起始点
- **yellow_hallway**: 黄色走廊
- **dark_corridor**: 黑暗走廊（需要手电筒）
- **electrical_room**: 电气室
- **the_hub**: 中心枢纽（危险区）
- **manila_room**: 马尼拉房间（出口）
- **supply_closet**: 储藏室
- **dead_end**: 死胡同

### 物品系统

- **Almond Water（杏仁水）**: 恢复理智值 +20
- **Flashlight（手电筒）**: 允许进入黑暗房间
- **Tape Recorder（录音机）**: 可以作为诱饵，吸引实体
- **Key（钥匙）**: 通关所需（可选）

### 胜利和失败条件

**胜利条件**：
- 到达 `manila_room`
- （可选）持有 `key`

**失败条件**：
- 理智值降至 0 或以下
- 与实体（The Howler）在同一房间

### 实体 AI（PDDL）

实体"The Howler"使用 PDDL 规划器控制：
- **move**: 在相邻房间移动
- **listen**: 监听玩家位置（基于噪音）
- **chase**: 追逐已知位置的玩家
- **roam**: 随机巡逻

## 开发说明

### 扩展游戏

1. **添加新房间**：在 `knowledge_base.pl` 中添加 `room/1` 和 `connect/3` 事实
2. **添加新物品**：在 `knowledge_base.pl` 中添加 `item/1` 和 `item_property/2` 事实
3. **修改实体行为**：编辑 `pddl/domains/adversary_domain.pddl`
4. **调整游戏规则**：修改 `game_logic.pl` 中的规则

### PDDL 集成

游戏使用 `prolog/pddl_interface.pl` 模块实现 Prolog 和 PDDL 之间的完整集成：

1. **自动生成 PDDL Problem**：从当前游戏状态自动生成 PDDL problem 文件
2. **调用规划器**：自动调用配置的 PDDL 规划器（如 Fast-Forward）
3. **解析规划结果**：从规划器输出中提取动作序列
4. **更新实体位置**：根据规划结果更新实体位置

详细说明请参考 `docs/pddl_integration.md`。

**注意**：需要安装 PDDL 规划器（如 Fast-Forward）才能使用实体 AI 功能。如果未安装规划器，实体将保持原位置。

## 技术亮点

1. **逻辑推理**：使用 Prolog 的规则系统实现复杂的游戏逻辑
2. **自动规划**：使用 PDDL 实现智能对手
3. **动态交互**：Prolog 和 PDDL 之间的实时数据交换
4. **约束检查**：基于逻辑的前置条件验证

## 报告写作建议

在撰写项目报告时，可以强调：

- **知识表示**：如何使用 Prolog 表示游戏世界的静态和动态知识
- **规则推理**：如何通过逻辑规则实现游戏机制（如黑暗房间需要手电筒）
- **自动规划**：如何使用 PDDL 实现实体的智能行为
- **系统集成**：如何将 Prolog 和 PDDL 两个系统整合在一起
- **动态重规划**：如何实现基于玩家行为的动态目标调整（如录音机诱饵）

## 许可证

本项目为课程作业项目。

## 参考资料

- The Backrooms Wiki: https://backrooms.fandom.com/
- SWI-Prolog 文档: https://www.swi-prolog.org/pldoc/
- PDDL 规范: https://planning.wiki/

## 作者

课程作业项目 - Knowledge Representation

