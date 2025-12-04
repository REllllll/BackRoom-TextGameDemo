# 设计文档

## 项目概述

**Liminal Logic: Escape from Level 0** 是一个结合 Prolog 和 PDDL 的文字冒险游戏，基于"The Backrooms"（后室）设定。

## 设计理念

### 为什么选择"The Backrooms"？

1. **天然契合**：后室本质上是由"房间"和"连接"组成的迷宫，非常适合用 Prolog 的逻辑关系建模
2. **氛围感**：文字冒险游戏擅长表现心理恐怖，不需要复杂图形
3. **复杂约束**：展示 Prolog 规则推理能力的绝佳例子（如"必须有光才能进暗室"）

## 系统架构

### 整体架构

```
┌─────────────────┐
│   Prolog 游戏   │
│   主循环        │
└────────┬────────┘
         │
         ├─── 游戏状态管理
         ├─── 玩家命令处理
         └─── 实体位置更新
                 │
                 ▼
         ┌─────────────────┐
         │  PDDL Problem   │
         │  生成器         │
         └────────┬────────┘
                  │
                  ▼
         ┌─────────────────┐
         │  PDDL 规划器    │
         │  (Fast-Forward) │
         └────────┬────────┘
                  │
                  ▼
         ┌─────────────────┐
         │  规划结果解析   │
         └────────┬────────┘
                  │
                  ▼
         ┌─────────────────┐
         │  更新实体位置   │
         └─────────────────┘
```

## 知识表示（Prolog）

### 静态知识

**房间定义**：
```prolog
room(start_point).
room(yellow_hallway).
% ...
```

**房间连接**：
```prolog
connect(start_point, east, yellow_hallway).
connect(yellow_hallway, west, start_point).
% ...
```

**物品属性**：
```prolog
item_property(almond_water, restores_sanity(20)).
item_property(flashlight, enables_dark_rooms).
```

### 动态知识

**游戏状态**：
```prolog
:- dynamic at_player/1.
:- dynamic at_entity/1.
:- dynamic sanity/1.
:- dynamic holding/1.
```

## 自动规划（PDDL）

### 域定义

实体"The Howler"的域包含以下动作：

1. **move**: 在相邻房间移动
2. **listen**: 监听玩家位置（基于噪音）
3. **chase**: 追逐已知位置的玩家
4. **roam**: 随机巡逻

### 规划目标

- **主要目标**：`(trapped player1)` - 抓住玩家
- **次要目标**：`(at howler manila_room)` - 移动到出口房间

### 动态重规划

当玩家丢弃录音机时：
1. Prolog 检测到 `item_location(tape_recorder, Location)`
2. 生成新的 PDDL problem，添加 `(noise_at Location)`
3. 更新实体目标为移动到噪音位置
4. 规划器重新规划，实体被"欺骗"去攻击录音机

## 游戏机制

### 理智值系统

- 初始值：100
- 移动消耗：-1
- 尝试进入黑暗房间失败：-5
- 与实体同房间：-10
- 使用杏仁水：+20

### 物品系统

1. **Almond Water**: 恢复理智值
2. **Flashlight**: 进入黑暗房间的前置条件
3. **Tape Recorder**: 诱饵物品，触发动态重规划
4. **Key**: 通关所需（可选）

### 约束系统

**黑暗房间约束**：
```prolog
can_enter_room(Room) :-
    is_dark(Room),
    holding(flashlight).
```

这展示了 Prolog 的规则推理能力。

## 实现细节

### Prolog 模块化

项目采用模块化设计：
- `knowledge_base`: 静态知识
- `game_state`: 状态管理
- `game_logic`: 游戏逻辑
- `win_conditions`: 胜利/失败条件
- `main`: 主程序

### PDDL 集成流程

1. 玩家执行动作
2. Prolog 更新游戏状态
3. 调用 `generate_problem.pl` 生成 PDDL problem
4. 调用 PDDL 规划器
5. 解析规划结果
6. 更新实体位置
7. 检查胜利/失败条件

## 技术亮点

1. **逻辑推理**：使用 Prolog 规则实现复杂约束
2. **自动规划**：使用 PDDL 实现智能对手
3. **动态交互**：实时数据交换
4. **重规划机制**：基于玩家行为的动态目标调整

## 未来扩展

- 添加更多房间和物品
- 实现更复杂的实体行为
- 添加谜题系统
- 实现保存/加载功能
- 添加图形界面（可选）

