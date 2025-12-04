% ============================================================================
% pddl_interface.pl
% ============================================================================
% Prolog 和 PDDL 之间的接口模块
% 功能：
%   1. 从 Prolog 游戏状态生成 PDDL problem 文件
%   2. 调用 PDDL 规划器（如 Fast-Forward）
%   3. 解析规划结果，更新实体位置
% ============================================================================

:- module(pddl_interface, [
    generate_pddl_problem/1,
    call_pddl_planner/2,
    update_entity_from_pddl/0,
    parse_plan_result/2,
    pddl_planner_command/1,
    pddl_domain_path/1,
    pddl_problem_path/1,
    pddl_plan_path/1,
    get_pddl_path/2,
    project_root/1
]).

:- use_module(game_state).
:- use_module(knowledge_base).

% ----------------------------------------------------------------------------
% 配置路径
% ----------------------------------------------------------------------------

% PDDL 文件路径（相对于项目根目录）
% 注意：这些路径在运行时会被解析为绝对路径
pddl_domain_path('pddl/domains/adversary_domain.pddl').
pddl_problem_path('pddl/problems/current_problem.pddl').
pddl_plan_path('pddl/problems/plan.txt').

% PDDL 规划器命令（可以根据实际安装的规划器调整）
% 常见选项：
% - Fast-Forward: 'ff -o DOMAIN -f PROBLEM'
% - Fast-Downward: 'fast-downward.py DOMAIN PROBLEM --search "astar(lmcut())"'
% - 其他规划器...
% 如果规划器不在 PATH 中，请使用完整路径，例如：
% pddl_planner_command('/usr/local/bin/ff').
pddl_planner_command('ff').

% 获取项目根目录的绝对路径
% 基于 pddl_interface.pl 的位置计算，确保无论从哪里调用都能正确获取
project_root(Root) :-
    % 使用 source_file 获取模块文件路径（最可靠的方法）
    (source_file(pddl_interface:project_root(_), ModuleFile) ->
        absolute_file_name(ModuleFile, AbsFile),
        file_directory_name(AbsFile, PrologDir),
        file_directory_name(PrologDir, Root)
    ;
        % 备用方法：基于当前文件上下文
        prolog_load_context(file, CurrentFile),
        absolute_file_name(CurrentFile, AbsFile),
        file_directory_name(AbsFile, PrologDir),
        file_directory_name(PrologDir, Root)
    ).

% 获取 PDDL 文件的绝对路径
get_pddl_path(RelativePath, AbsolutePath) :-
    project_root(Root),
    atomic_list_concat([Root, '/', RelativePath], AbsolutePath).

% ----------------------------------------------------------------------------
% 主函数：从 PDDL 更新实体位置
% ----------------------------------------------------------------------------
% update_entity_from_pddl
% 生成 PDDL problem，调用规划器，解析结果并更新实体位置
% ----------------------------------------------------------------------------

update_entity_from_pddl :-
    % 1. 生成当前状态的 PDDL problem 文件
    pddl_problem_path(ProblemPathRel),
    get_pddl_path(ProblemPathRel, ProblemPath),
    generate_pddl_problem(ProblemPath),
    
    % 2. 调用 PDDL 规划器
    pddl_domain_path(DomainPathRel),
    get_pddl_path(DomainPathRel, DomainPath),
    call_pddl_planner(DomainPath, ProblemPath),
    
    % 3. 解析规划结果并更新实体位置
    pddl_plan_path(PlanPathRel),
    get_pddl_path(PlanPathRel, PlanPath),
    (exists_file(PlanPath) ->
        parse_plan_result(PlanPath, Actions),
        (Actions = [] ->
            write('PDDL planner found no actions. Entity stays in place.'), nl
        ;
            apply_entity_actions(Actions),
            write('Entity moved based on PDDL plan.'), nl
        )
    ;
        % 如果规划失败，实体可能保持原位置或执行默认行为
        write('PDDL planner did not generate a plan. Entity stays in place.'), nl
    ),
    !.

% ----------------------------------------------------------------------------
% 生成 PDDL Problem 文件
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
    close(Stream).

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

write_connection_pairs(_Stream, []).
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
    (item_location(tape_recorder, Loc) ->
        write(Stream, '    (noise_at '), write(Stream, Loc), write(Stream, ')'), nl(Stream)
    ; true).

% ----------------------------------------------------------------------------
% 写入目标状态
% ----------------------------------------------------------------------------

write_goal(Stream) :-
    write(Stream, '  (:goal (or'), nl(Stream),
    write(Stream, '    (trapped player1)'), nl(Stream),
    write(Stream, '    (at howler manila_room)'), nl(Stream),
    write(Stream, '  ))'), nl(Stream).

% ----------------------------------------------------------------------------
% 调用 PDDL 规划器
% ----------------------------------------------------------------------------
% call_pddl_planner(+DomainPath, +ProblemPath)
% 调用外部 PDDL 规划器，生成规划结果
% ----------------------------------------------------------------------------

call_pddl_planner(DomainPath, ProblemPath) :-
    pddl_planner_command(PlannerCmd),
    pddl_plan_path(PlanPath),
    
    % 检查文件是否存在
    (exists_file(DomainPath) ->
        true
    ;
        write('Error: PDDL domain file not found: '), write(DomainPath), nl,
        fail
    ),
    (exists_file(ProblemPath) ->
        true
    ;
        write('Error: PDDL problem file not found: '), write(ProblemPath), nl,
        fail
    ),
    
    % 构建规划器命令
    % 注意：不同规划器的命令格式可能不同
    % Fast-Forward 格式: ff -o DOMAIN -f PROBLEM > PLAN
    % 如果规划器不在 PATH 中，可能需要完整路径
    atomic_list_concat([PlannerCmd, ' -o ', DomainPath, ' -f ', ProblemPath, ' > ', PlanPath, ' 2>&1'], Command),
    
    % 执行命令
    shell(Command, Status),
    (Status = 0 ->
        write('PDDL planner executed successfully.'), nl
    ;
        write('Warning: PDDL planner returned non-zero status: '), write(Status), nl,
        write('This may indicate no plan was found, or the planner is not installed.'), nl
    ).

% ----------------------------------------------------------------------------
% 解析规划结果
% ----------------------------------------------------------------------------
% parse_plan_result(+PlanPath, -Actions)
% 从规划器输出文件中解析动作序列
% 返回动作列表，每个动作格式为: action(Name, Args)
% ----------------------------------------------------------------------------

parse_plan_result(PlanPath, Actions) :-
    open(PlanPath, read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    filter_action_lines(Lines, ActionLines),
    parse_action_lines(ActionLines, Actions).

% 读取文件的所有行
read_lines(Stream, Lines) :-
    read_line_to_string(Stream, Line),
    (Line = end_of_file ->
        Lines = []
    ;
        Lines = [Line|Rest],
        read_lines(Stream, Rest)
    ).

% 过滤出动作行（通常以动作名开头，如 "move", "chase" 等）
filter_action_lines([], []).
filter_action_lines([Line|Rest], Filtered) :-
    (Line = end_of_file ->
        Filtered = []
    ;
        % 修剪字符串两端的空白字符
        trim_string(Line, Trimmed),
        (is_action_line(Trimmed) ->
            Filtered = [Trimmed|RestFiltered]
        ;
            Filtered = RestFiltered
        ),
        filter_action_lines(Rest, RestFiltered)
    ).

% 判断是否是动作行
% 不同规划器的输出格式可能不同，这里处理常见格式
is_action_line(Line) :-
    string_lower(Line, LowerLine),
    (sub_string(LowerLine, 0, 4, _, "move") -> true
    ; sub_string(LowerLine, 0, 5, _, "chase") -> true
    ; sub_string(LowerLine, 0, 5, _, "roam") -> true
    ; sub_string(LowerLine, 0, 6, _, "listen") -> true
    ; false).

% 解析动作行
parse_action_lines([], []).
parse_action_lines([Line|Rest], [Action|Actions]) :-
    parse_action_line(Line, Action),
    parse_action_lines(Rest, Actions).

% 解析单个动作行
% 格式示例: "move howler electrical_room the_hub"
% 或: "(move howler electrical_room the_hub)"
parse_action_line(Line, action(Name, Args)) :-
    % 移除可能的括号和空白字符
    string_chars(Line, Chars),
    exclude(==('('), Chars, Chars1),
    exclude(==(')'), Chars1, Chars2),
    string_chars(Trimmed, Chars2),
    trim_string(Trimmed, FinalTrimmed),
    split_string(FinalTrimmed, ' ', ' ', Parts),
    Parts = [NameStr|ArgStrs],
    (ArgStrs = [] ->
        Args = []
    ;
        % 过滤空字符串
        exclude(==(""), ArgStrs, FilteredArgs),
        maplist(atom_string, Args, FilteredArgs)
    ),
    atom_string(Name, NameStr).

% ----------------------------------------------------------------------------
% 应用实体动作
% ----------------------------------------------------------------------------
% apply_entity_actions(+Actions)
% 根据解析的动作序列更新实体位置
% ----------------------------------------------------------------------------

apply_entity_actions([]).
apply_entity_actions([action(move, [_, _, To])|Rest]) :-
    (string(To) ->
        atom_string(ToAtom, To)
    ;
        ToAtom = To
    ),
    set_entity_location(ToAtom),
    apply_entity_actions(Rest).
apply_entity_actions([action(chase, [_, _, To, _])|Rest]) :-
    (string(To) ->
        atom_string(ToAtom, To)
    ;
        ToAtom = To
    ),
    set_entity_location(ToAtom),
    apply_entity_actions(Rest).
apply_entity_actions([action(roam, [_, _, To])|Rest]) :-
    (string(To) ->
        atom_string(ToAtom, To)
    ;
        ToAtom = To
    ),
    set_entity_location(ToAtom),
    apply_entity_actions(Rest).
apply_entity_actions([action(listen, _)|Rest]) :-
    % 监听动作不改变位置
    apply_entity_actions(Rest).
apply_entity_actions([_|Rest]) :-
    % 忽略未知动作
    apply_entity_actions(Rest).

% ----------------------------------------------------------------------------
% 辅助函数：检查文件是否存在
% ----------------------------------------------------------------------------

exists_file(File) :-
    access_file(File, read).

% ----------------------------------------------------------------------------
% 辅助函数：字符串处理
% ----------------------------------------------------------------------------

% 修剪字符串两端的空白字符
% 使用 normalize_space 来标准化空白字符，然后手动移除首尾空白
trim_string(String, Trimmed) :-
    % 先标准化空白字符（将多个连续空白替换为单个空格）
    normalize_space(string(Normalized), String),
    % 移除首尾空白
    string_chars(Normalized, Chars),
    trim_chars_left(Chars, Chars1),
    trim_chars_right(Chars1, TrimmedChars),
    string_chars(Trimmed, TrimmedChars).

% 移除左侧空白字符
trim_chars_left([], []).
trim_chars_left([H|T], Result) :-
    (char_type(H, space) ->
        trim_chars_left(T, Result)
    ;
        Result = [H|T]
    ).

% 移除右侧空白字符
trim_chars_right([], []).
trim_chars_right(List, Result) :-
    reverse(List, Reversed),
    trim_chars_left(Reversed, TrimmedReversed),
    reverse(TrimmedReversed, Result).

