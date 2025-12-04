# PDDL 环境测试指南

本文档说明如何检查 PDDL 环境是否正常运行并接入到 Prolog 中。

## 快速测试

### 方式一：使用 Shell 测试脚本（推荐）

运行快速检查脚本：

```bash
./scripts/test_pddl.sh
```

这个脚本会检查：
1. SWI-Prolog 是否安装
2. PDDL 规划器（如 Fast-Forward）是否安装
3. PDDL 文件是否存在
4. Prolog 模块文件是否存在
5. PDDL Domain 文件语法是否正确
6. 运行完整的 Prolog 集成测试

### 方式二：使用 Prolog 测试脚本

直接运行 Prolog 测试脚本：

```bash
swipl -s scripts/test_pddl_integration.pl -g test_pddl_integration -t halt
```

或者在 Prolog 交互式环境中：

```prolog
?- [scripts/test_pddl_integration].
?- test_pddl_integration.
```

## 测试内容详解

### 测试1: 检查 PDDL 规划器是否安装

检查配置的 PDDL 规划器（默认是 `ff`）是否在系统 PATH 中。

**如果失败**：
- 安装 Fast-Forward: http://fai.cs.uni-saarland.de/hoffmann/ff.html
- 或者修改 `prolog/pddl_interface.pl` 中的 `pddl_planner_command/1` 配置

### 测试2: 检查 PDDL 文件是否存在

检查以下文件是否存在：
- `pddl/domains/adversary_domain.pddl` - PDDL 域定义文件
- `pddl/problems/` - 问题文件目录

### 测试3: 初始化游戏状态

测试游戏状态模块是否能正确初始化，包括：
- 玩家位置
- 实体位置
- 理智值

### 测试4: 测试生成 PDDL Problem 文件

测试能否从当前 Prolog 游戏状态生成 PDDL problem 文件。

**生成的文件位置**：`pddl/problems/current_problem.pddl`

### 测试5: 测试调用规划器

测试能否成功调用 PDDL 规划器并生成规划结果。

**规划结果文件位置**：`pddl/problems/plan.txt`

### 测试6: 测试解析规划结果

测试能否从规划器输出中解析出动作序列。

### 测试7: 完整集成流程测试

测试完整的 PDDL 集成流程：
1. 生成 problem 文件
2. 调用规划器
3. 解析规划结果
4. 更新实体位置

## 手动测试步骤

如果你想手动测试每个步骤：

### 步骤1: 初始化游戏状态

```prolog
?- [prolog/main].
?- init_game_state.
?- at_player(Loc).
?- at_entity(Loc).
```

### 步骤2: 生成 PDDL Problem 文件

```prolog
?- [prolog/pddl_interface].
?- generate_pddl_problem('pddl/problems/test_problem.pddl').
```

然后检查生成的文件：
```bash
cat pddl/problems/test_problem.pddl
```

### 步骤3: 手动调用规划器

```bash
ff -o pddl/domains/adversary_domain.pddl -f pddl/problems/test_problem.pddl > pddl/problems/test_plan.txt
```

### 步骤4: 解析规划结果

```prolog
?- parse_plan_result('pddl/problems/test_plan.txt', Actions).
```

### 步骤5: 完整流程

```prolog
?- update_entity_from_pddl.
```

## 常见问题

### 问题1: 规划器未找到

**错误信息**：
```
✗ 规划器未安装或无法执行
```

**解决方案**：
1. 安装 Fast-Forward 或其他 PDDL 规划器
2. 确保规划器在系统 PATH 中
3. 或者修改 `prolog/pddl_interface.pl` 中的配置使用完整路径

### 问题2: 规划文件未生成

**可能原因**：
- 规划器未找到解决方案（这是正常的，取决于问题状态）
- 规划器命令格式不正确
- Domain 或 Problem 文件有语法错误

**检查方法**：
```bash
# 查看规划器输出
cat pddl/problems/plan.txt

# 手动测试规划器
ff -o pddl/domains/adversary_domain.pddl -f pddl/problems/current_problem.pddl
```

### 问题3: 规划结果解析失败

**可能原因**：
- 规划器输出格式与预期不符
- 规划文件为空或格式错误

**检查方法**：
```bash
# 查看规划文件内容
cat pddl/problems/plan.txt
```

### 问题4: 实体位置未更新

**可能原因**：
- 规划为空（没有找到解决方案）
- 解析的动作格式不正确
- 实体已经在目标位置

**检查方法**：
```prolog
?- at_entity(Loc).
?- update_entity_from_pddl.
?- at_entity(NewLoc).
```

## 验证集成是否成功

如果所有测试都通过，说明 PDDL 环境已成功接入 Prolog。你可以：

1. **运行游戏**：
   ```bash
   ./scripts/run_game.sh
   ```

2. **在游戏中观察实体行为**：
   - 实体应该会根据 PDDL 规划移动
   - 每次玩家移动后，实体会更新位置

3. **检查日志输出**：
   - 游戏运行时会显示 "PDDL planner executed successfully."
   - 如果规划成功，会显示 "Entity moved based on PDDL plan."

## 调试技巧

### 启用详细输出

修改 `prolog/pddl_interface.pl`，在关键位置添加调试输出：

```prolog
write('Debug: Generating problem file...'), nl,
generate_pddl_problem(ProblemPath),
write('Debug: Problem file generated at '), write(ProblemPath), nl,
```

### 检查生成的文件

```bash
# 查看生成的 problem 文件
cat pddl/problems/current_problem.pddl

# 查看规划结果
cat pddl/problems/plan.txt
```

### 使用不同的规划器

如果 Fast-Forward 不可用，可以尝试其他规划器：

1. **Fast-Downward**:
   ```prolog
   pddl_planner_command('fast-downward.py').
   ```
   注意：需要修改命令格式

2. **其他规划器**:
   根据规划器的命令格式修改 `call_pddl_planner/2` 中的命令构建逻辑

## 总结

通过运行测试脚本，你可以快速确认：
- ✅ PDDL 规划器已正确安装
- ✅ PDDL 文件存在且语法正确
- ✅ Prolog 能够生成 PDDL problem 文件
- ✅ 规划器能够成功执行并生成规划
- ✅ Prolog 能够解析规划结果并更新游戏状态

如果所有测试通过，PDDL 环境已成功接入 Prolog！

