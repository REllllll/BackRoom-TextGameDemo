% ============================================================================
% win_conditions.pl
% ============================================================================
% 胜利和失败条件：定义游戏结束条件
% ============================================================================

:- module(win_conditions, [
    check_win/0,
    check_lose/0,
    game_over/1
]).

:- use_module(game_state).
:- use_module(knowledge_base).

% ----------------------------------------------------------------------------
% 胜利条件检查 (Win Condition Check)
% ----------------------------------------------------------------------------

check_win :-
    at_player(manila_room),
    is_exit(manila_room),
    (holding(key) -> true; true),  % 如果需要钥匙，检查是否持有
    write('========================================'), nl,
    write('YOU ESCAPED!'), nl,
    write('You noclipped out of Level 0!'), nl,
    write('========================================'), nl,
    game_over(win),
    !.

% ----------------------------------------------------------------------------
% 失败条件检查 (Lose Condition Check)
% ----------------------------------------------------------------------------

check_lose :-
    sanity(S),
    S =< 0,
    write('========================================'), nl,
    write('GAME OVER'), nl,
    write('Your sanity has been depleted.'), nl,
    write('You are lost in the Backrooms forever...'), nl,
    write('========================================'), nl,
    game_over(lose_sanity),
    !.

check_lose :-
    at_player(PlayerLoc),
    at_entity(EntityLoc),
    PlayerLoc = EntityLoc,
    write('========================================'), nl,
    write('GAME OVER'), nl,
    write('The Howler has caught you!'), nl,
    write('You are lost in the Backrooms forever...'), nl,
    write('========================================'), nl,
    game_over(lose_caught),
    !.

% ----------------------------------------------------------------------------
% 游戏结束处理 (Game Over Handler)
% ----------------------------------------------------------------------------

game_over(_Result) :-
    % TODO: 保存游戏结果，清理资源等
    true.

