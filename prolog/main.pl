% ============================================================================
% main.pl
% ============================================================================
% 主程序入口：整合所有模块，处理游戏循环和 PDDL 交互
% ============================================================================

:- use_module(knowledge_base).
:- use_module(game_state).
:- use_module(game_logic).
:- use_module(win_conditions).
:- use_module(pddl_interface).

% ----------------------------------------------------------------------------
% 游戏主循环 (Main Game Loop)
% ----------------------------------------------------------------------------

start :-
    write('========================================'), nl,
    write('LIMINAL LOGIC: Escape from Level 0'), nl,
    write('========================================'), nl,
    nl,
    write('You have noclipped out of reality...'), nl,
    write('You find yourself in Level 0 of the Backrooms.'), nl,
    write('The yellow wallpaper, the buzzing lights...'), nl,
    write('Find the Manila Room and escape!'), nl,
    nl,
    init_game_state,
    game_loop.

game_loop :-
    check_win,
    !.
game_loop :-
    check_lose,
    !.
game_loop :-
    look,
    nl,
    write('> '),
    read(Command),
    process_command(Command),
    update_entity,  % 更新实体位置（基于 PDDL 规划）
    nl,
    game_loop.

% ----------------------------------------------------------------------------
% 命令处理 (Command Processing)
% ----------------------------------------------------------------------------

process_command(move(Direction)) :-
    move(Direction),
    !.
process_command(take(Item)) :-
    take(Item),
    !.
process_command(drop(Item)) :-
    drop(Item),
    !.
process_command(use(Item)) :-
    use(Item),
    !.
process_command(look) :-
    look,
    !.
process_command(quit) :-
    write('Thanks for playing!'), nl,
    halt,
    !.
process_command(_) :-
    write('Unknown command. Try: move(direction), take(item), drop(item), use(item), look, quit.'), nl.

% ----------------------------------------------------------------------------
% 实体更新 (Entity Update)
% ----------------------------------------------------------------------------

update_entity :-
    % 使用 PDDL 接口模块更新实体位置
    update_entity_from_pddl.

% ----------------------------------------------------------------------------
% 辅助函数 (Helper Functions)
% ----------------------------------------------------------------------------

% PDDL 相关功能已移至 pddl_interface.pl 模块

