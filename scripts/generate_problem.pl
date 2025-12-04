% ============================================================================
% generate_problem.pl
% ============================================================================
% 从 Prolog 游戏状态生成 PDDL problem 文件
% 读取当前游戏状态（玩家位置、实体位置等），生成对应的 PDDL problem
% ============================================================================

:- use_module('../prolog/game_state').
:- use_module('../prolog/knowledge_base').

% ----------------------------------------------------------------------------
% 主函数：生成 PDDL problem 文件
% ----------------------------------------------------------------------------
% generate_pddl_problem(+OutputFile)
% 从当前游戏状态生成 PDDL problem 并写入指定文件
% ----------------------------------------------------------------------------

generate_pddl_problem(OutputFile) :-
    open(OutputFile, write, Stream),
    
    % 写入文件头
    write(Stream, '(define (problem backrooms_current)'), nl(Stream),
    write(Stream, '  (:domain adversary)'), nl(Stream),
    nl(Stream),
    
    % 写入对象定义
    write_objects(Stream),
    nl(Stream),
    
    % 写入初始状态
    write(Stream, '  (:init'), nl(Stream),
    write_initial_state(Stream),
    write(Stream, '  )'), nl(Stream),
    nl(Stream),
    
    % 写入目标状态
    write_goal(Stream),
    nl(Stream),
    
    write(Stream, ')'), nl(Stream),
    close(Stream),
    write('PDDL problem file generated: '), write(OutputFile), nl.

% ----------------------------------------------------------------------------
% 写入对象定义
% ----------------------------------------------------------------------------

write_objects(Stream) :-
    write(Stream, '  (:objects'), nl(Stream),
    write(Stream, '    howler - entity'), nl(Stream),
    write(Stream, '    player1 - player'), nl(Stream),
    write(Stream, '    start_point yellow_hallway dark_corridor electrical_room'), nl(Stream),
    write(Stream, '    the_hub manila_room supply_closet dead_end - location'), nl(Stream),
    write(Stream, '  )'), nl(Stream).

% ----------------------------------------------------------------------------
% 写入初始状态
% ----------------------------------------------------------------------------

write_initial_state(Stream) :-
    % 写入实体位置
    (at_entity(EntityLoc) ->
        write(Stream, '    (at howler '), write(Stream, EntityLoc), write(Stream, ')'), nl(Stream)
    ; true),
    
    % 写入玩家位置
    (at_player(PlayerLoc) ->
        write(Stream, '    (at_player player1 '), write(Stream, PlayerLoc), write(Stream, ')'), nl(Stream)
    ; true),
    
    % 写入房间连接（双向）
    write_connections(Stream),
    
    % 写入噪音位置（如果有）
    write_noise_locations(Stream).

% ----------------------------------------------------------------------------
% 写入房间连接
% ----------------------------------------------------------------------------

write_connections(Stream) :-
    findall(From-To, connect(From, _, To), Connections),
    write_connection_pairs(Stream, Connections).

write_connection_pairs(Stream, []).
write_connection_pairs(Stream, [From-To|Rest]) :-
    write(Stream, '    (connected '), write(Stream, From), write(Stream, ' '), 
    write(Stream, To), write(Stream, ')'), nl(Stream),
    write(Stream, '    (connected '), write(Stream, To), write(Stream, ' '), 
    write(Stream, From), write(Stream, ')'), nl(Stream),
    write_connection_pairs(Stream, Rest).

% ----------------------------------------------------------------------------
% 写入噪音位置（如果丢弃了录音机）
% ----------------------------------------------------------------------------

write_noise_locations(Stream) :-
    % TODO: 检查是否有录音机被丢弃，如果有，写入噪音位置
    % item_location(tape_recorder, Loc) ->
    %     write(Stream, '    (noise_at '), write(Stream, Loc), write(Stream, ')'), nl(Stream)
    % ; true
    true.

% ----------------------------------------------------------------------------
% 写入目标状态
% ----------------------------------------------------------------------------

write_goal(Stream) :-
    write(Stream, '  (:goal (or'), nl(Stream),
    write(Stream, '    (trapped player1)'), nl(Stream),
    write(Stream, '    (at howler manila_room)'), nl(Stream),
    write(Stream, '  ))'), nl(Stream).

% ----------------------------------------------------------------------------
% 使用示例
% ----------------------------------------------------------------------------
% ?- generate_pddl_problem('../pddl/problems/current_problem.pddl').

