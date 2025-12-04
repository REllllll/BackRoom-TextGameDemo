# Prolog 与 PDDL 集成说明

本文档说明如何使用 `pddl_interface.pl` 模块实现 Prolog 和 PDDL 之间的连接。

## 概述

`pddl_interface.pl` 模块提供了以下功能：

1. **生成 PDDL Problem 文件**：从当前 Prolog 游戏状态自动生成 PDDL problem 文件
2. **调用 PDDL 规划器**：执行外部 PDDL 规划器（如 Fast-Forward）来生成规划
3. **解析规划结果**：从规划器输出中提取动作序列
4. **更新实体位置**：根据规划结果更新 Prolog 中的实体位置

## 使用方法

### 基本使用

在游戏主循环中，`update_entity_from_pddl/0` 会自动：

1. 读取当前游戏状态（玩家位置、实体位置等）
2. 生成 PDDL problem 文件到 `pddl/problems/current_problem.pddl`
3. 调用配置的 PDDL 规划器
4. 解析规划结果并更新实体位置

```prolog
% 在 main.pl 中已经集成
update_entity :-
    update_entity_from_pddl.
```

### 手动调用

你也可以手动调用各个函数：

```prolog
% 1. 生成 PDDL problem 文件
?- generate_pddl_problem('pddl/problems/my_problem.pddl').

% 2. 调用规划器
?- call_pddl_planner('pddl/domains/adversary_domain.pddl', 
                      'pddl/problems/current_problem.pddl').

% 3. 解析规划结果
?- parse_plan_result('pddl/problems/plan.txt', Actions).

% 4. 完整流程
?- update_entity_from_pddl.
```

## 配置

### 路径配置

在 `pddl_interface.pl` 中可以配置以下路径：

```prolog
pddl_domain_path('pddl/domains/adversary_domain.pddl').
pddl_problem_path('pddl/problems/current_problem.pddl').
pddl_plan_path('pddl/problems/plan.txt').
```

这些路径是相对于项目根目录的。模块会自动解析为绝对路径。

### 规划器配置

配置要使用的 PDDL 规划器：

```prolog
% Fast-Forward
pddl_planner_command('ff').

% 如果规划器不在 PATH 中，使用完整路径
% pddl_planner_command('/usr/local/bin/ff').

% Fast-Downward（需要不同的命令格式）
% pddl_planner_command('fast-downward.py').
```

### 支持不同规划器

不同规划器的命令格式可能不同。当前实现支持 Fast-Forward 格式：

```
ff -o DOMAIN -f PROBLEM > PLAN
```

如果需要支持其他规划器，可以修改 `call_pddl_planner/2` 函数中的命令构建逻辑。

## 规划结果格式

模块会解析以下格式的动作行：

- `move howler electrical_room the_hub`
- `chase howler electrical_room the_hub player1`
- `roam howler electrical_room the_hub`
- `listen howler electrical_room player1`

也支持带括号的格式：
- `(move howler electrical_room the_hub)`

## 错误处理

模块包含以下错误处理：

1. **文件不存在**：检查 domain 和 problem 文件是否存在
2. **规划器失败**：如果规划器返回非零状态，会显示警告但不会中断游戏
3. **无规划结果**：如果规划器没有生成规划，实体保持原位置

## 调试

如果遇到问题，可以：

1. **检查生成的问题文件**：
   ```prolog
   ?- generate_pddl_problem('pddl/problems/debug.pddl').
   ```

2. **手动运行规划器**：
   ```bash
   ff -o pddl/domains/adversary_domain.pddl \
      -f pddl/problems/current_problem.pddl \
      > pddl/problems/plan.txt
   ```

3. **检查规划结果**：
   ```prolog
   ?- parse_plan_result('pddl/problems/plan.txt', Actions).
   ```

## 扩展

### 添加新的动作类型

如果 PDDL domain 中添加了新动作，需要在 `is_action_line/1` 和 `apply_entity_actions/1` 中添加相应的处理。

### 自定义目标状态

可以修改 `write_goal/1` 函数来改变实体的目标行为。

### 添加噪音检测

当玩家丢弃录音机时，`write_noise_locations/1` 会自动在 PDDL problem 中添加 `noise_at` 谓词，实体可以通过 `listen` 动作检测到。

## 注意事项

1. **规划器安装**：确保已安装并配置了 PDDL 规划器
2. **文件权限**：确保 Prolog 有权限读取 domain 文件和写入 problem/plan 文件
3. **路径问题**：如果使用相对路径，确保从正确的目录运行
4. **规划器输出格式**：不同规划器的输出格式可能不同，可能需要调整解析逻辑

## 示例工作流程

1. 玩家执行动作（如 `move(east)`）
2. Prolog 更新玩家位置
3. 调用 `update_entity_from_pddl`
4. 生成包含当前状态的 PDDL problem
5. 规划器计算实体的下一步动作
6. 解析规划结果，更新实体位置
7. 检查玩家和实体是否在同一房间（触发失败条件）

