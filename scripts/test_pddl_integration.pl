% ============================================================================
% test_pddl_integration.pl
% ============================================================================
% PDDL 集成测试脚本
% 用于检查 PDDL 环境是否正常运行并接入到 Prolog 中
% ============================================================================

% 定义文件搜索路径以避免相对路径警告
:- prolog_load_context(file, ThisFile),
   absolute_file_name(ThisFile, AbsThisFile),
   file_directory_name(AbsThisFile, ScriptDir),
   file_directory_name(ScriptDir, ProjectRoot),
   atomic_list_concat([ProjectRoot, '/prolog'], PrologDir),
   asserta(user:file_search_path(project, ProjectRoot)),
   asserta(user:file_search_path(prolog, PrologDir)).

% 使用定义的搜索路径加载模块
:- use_module(prolog(knowledge_base)).
:- use_module(prolog(game_state)).
:- use_module(prolog(pddl_interface)).

% ----------------------------------------------------------------------------
% 主测试函数
% ----------------------------------------------------------------------------

test_pddl_integration :-
    write('========================================'), nl,
    write('PDDL 集成测试'), nl,
    write('========================================'), nl,
    nl,
    
    % 测试1: 检查PDDL规划器是否安装
    test_planner_installed,
    nl,
    
    % 测试2: 检查PDDL文件是否存在
    test_pddl_files_exist,
    nl,
    
    % 测试3: 初始化游戏状态
    test_init_game_state,
    nl,
    
    % 测试4: 测试生成PDDL problem文件
    test_generate_problem,
    nl,
    
    % 测试5: 测试调用规划器
    test_call_planner,
    nl,
    
    % 测试6: 测试解析规划结果
    test_parse_plan,
    nl,
    
    % 测试7: 完整流程测试
    test_full_integration,
    nl,
    
    write('========================================'), nl,
    write('所有测试完成！'), nl,
    write('========================================'), nl.

% ----------------------------------------------------------------------------
% 测试1: 检查PDDL规划器是否安装
% ----------------------------------------------------------------------------

test_planner_installed :-
    write('[测试1] 检查PDDL规划器是否安装...'), nl,
    pddl_interface:pddl_planner_command(PlannerCmd),
    write('  规划器命令: '), write(PlannerCmd), nl,
    
    % 尝试执行规划器命令（使用 --help 或 -h 来检查）
    atomic_list_concat([PlannerCmd, ' -h 2>&1'], TestCmd),
    catch(
        (shell(TestCmd, Status),
         (Status = 0 ->
             write('  ✓ 规划器已安装并可执行'), nl
         ;
             write('  ⚠ 规划器命令存在但返回非零状态（可能正常）'), nl
         )),
        Error,
        (write('  ✗ 规划器未安装或无法执行: '), write(Error), nl,
         write('  提示: 请安装 Fast-Forward (ff) 或其他PDDL规划器'), nl,
         fail)
    ).

% ----------------------------------------------------------------------------
% 测试2: 检查PDDL文件是否存在
% ----------------------------------------------------------------------------

test_pddl_files_exist :-
    write('[测试2] 检查PDDL文件是否存在...'), nl,
    
    % 先测试 project_root 是否工作
    (pddl_interface:project_root(Root) ->
        write('  项目根目录: '), write(Root), nl
    ;
        write('  ⚠ 无法获取项目根目录'), nl
    ),
    
    (pddl_interface:pddl_domain_path(DomainPathRel) ->
        write('  Domain文件相对路径: '), write(DomainPathRel), nl,
        (pddl_interface:get_pddl_path(DomainPathRel, DomainPath) ->
            write('  Domain文件绝对路径: '), write(DomainPath), nl,
            
            (exists_file(DomainPath) ->
                write('  ✓ Domain文件存在'), nl
            ;
                write('  ✗ Domain文件不存在'), nl,
                write('  尝试检查文件: '), write(DomainPath), nl,
                % 尝试列出目录内容以便调试
                file_directory_name(DomainPath, DomainDir),
                (exists_file(DomainDir) ->
                    write('  目录存在，列出内容:'), nl,
                    directory_files(DomainDir, Files),
                    forall(member(File, Files), (write('    - '), write(File), nl))
                ;
                    write('  目录也不存在: '), write(DomainDir), nl
                ),
                fail
            )
        ;
            write('  ✗ 无法解析Domain文件路径'), nl,
            fail
        )
    ;
        write('  ✗ 无法获取Domain文件相对路径'), nl,
        fail
    ),
    
    (pddl_interface:pddl_problem_path(ProblemPathRel) ->
        write('  Problem文件相对路径: '), write(ProblemPathRel), nl,
        (pddl_interface:get_pddl_path(ProblemPathRel, ProblemPath) ->
            write('  Problem文件绝对路径: '), write(ProblemPath), nl,
            write('  (Problem文件会在运行时生成)'), nl
        ;
            write('  ⚠ 无法解析Problem文件路径'), nl
        )
    ;
        write('  ⚠ 无法获取Problem文件相对路径'), nl
    ).

% ----------------------------------------------------------------------------
% 测试3: 初始化游戏状态
% ----------------------------------------------------------------------------

test_init_game_state :-
    write('[测试3] 初始化游戏状态...'), nl,
    
    (init_game_state ->
        write('  ✓ 游戏状态初始化成功'), nl,
        at_player(PlayerLoc),
        write('  玩家位置: '), write(PlayerLoc), nl,
        at_entity(EntityLoc),
        write('  实体位置: '), write(EntityLoc), nl,
        sanity(S),
        write('  理智值: '), write(S), nl
    ;
        write('  ✗ 游戏状态初始化失败'), nl,
        fail
    ).

% ----------------------------------------------------------------------------
% 测试4: 测试生成PDDL problem文件
% ----------------------------------------------------------------------------

test_generate_problem :-
    write('[测试4] 测试生成PDDL problem文件...'), nl,
    
    pddl_interface:pddl_problem_path(ProblemPathRel),
    pddl_interface:get_pddl_path(ProblemPathRel, ProblemPath),
    
    (generate_pddl_problem(ProblemPath) ->
        write('  ✓ PDDL problem文件生成成功'), nl,
        write('  文件路径: '), write(ProblemPath), nl,
        
        % 读取并显示文件的前几行
        (open(ProblemPath, read, Stream) ->
            read_line_to_string(Stream, Line1),
            read_line_to_string(Stream, Line2),
            read_line_to_string(Stream, Line3),
            close(Stream),
            write('  文件预览:'), nl,
            write('    '), write(Line1), nl,
            write('    '), write(Line2), nl,
            write('    '), write(Line3), nl
        ;
            true
        )
    ;
        write('  ✗ PDDL problem文件生成失败'), nl,
        fail
    ).

% ----------------------------------------------------------------------------
% 测试5: 测试调用规划器
% ----------------------------------------------------------------------------

test_call_planner :-
    write('[测试5] 测试调用PDDL规划器...'), nl,
    
    pddl_interface:pddl_domain_path(DomainPathRel),
    pddl_interface:get_pddl_path(DomainPathRel, DomainPath),
    pddl_interface:pddl_problem_path(ProblemPathRel),
    pddl_interface:get_pddl_path(ProblemPathRel, ProblemPath),
    
    (call_pddl_planner(DomainPath, ProblemPath) ->
        write('  ✓ 规划器调用成功'), nl,
        
        % 检查是否生成了规划文件
        pddl_interface:pddl_plan_path(PlanPathRel),
        pddl_interface:get_pddl_path(PlanPathRel, PlanPath),
        (exists_file(PlanPath) ->
            write('  ✓ 规划文件已生成: '), write(PlanPath), nl
        ;
            write('  ⚠ 规划文件未生成（可能没有找到规划）'), nl
        )
    ;
        write('  ✗ 规划器调用失败'), nl,
        write('  提示: 请检查规划器是否正确安装'), nl,
        fail
    ).

% ----------------------------------------------------------------------------
% 测试6: 测试解析规划结果
% ----------------------------------------------------------------------------

test_parse_plan :-
    write('[测试6] 测试解析规划结果...'), nl,
    
    pddl_interface:pddl_plan_path(PlanPathRel),
    pddl_interface:get_pddl_path(PlanPathRel, PlanPath),
    
    (exists_file(PlanPath) ->
        (parse_plan_result(PlanPath, Actions) ->
            write('  ✓ 规划结果解析成功'), nl,
            length(Actions, ActionCount),
            write('  找到动作数量: '), write(ActionCount), nl,
            (Actions = [] ->
                write('  ⚠ 规划为空（可能没有找到解决方案）'), nl
            ;
                write('  动作列表:'), nl,
                write_actions(Actions)
            )
        ;
            write('  ✗ 规划结果解析失败'), nl,
            write('  提示: 规划文件格式可能不正确'), nl,
            fail
        )
    ;
        write('  ⚠ 规划文件不存在，跳过解析测试'), nl
    ).

write_actions([]).
write_actions([action(Name, Args)|Rest]) :-
    write('    - '), write(Name), write('('), write_args(Args), write(')'), nl,
    write_actions(Rest).

write_args([]).
write_args([Arg]) :-
    write(Arg).
write_args([Arg|Rest]) :-
    write(Arg), write(', '),
    write_args(Rest).

% ----------------------------------------------------------------------------
% 测试7: 完整流程测试
% ----------------------------------------------------------------------------

test_full_integration :-
    write('[测试7] 完整集成流程测试...'), nl,
    
    % 记录初始实体位置
    at_entity(InitialLoc),
    write('  初始实体位置: '), write(InitialLoc), nl,
    
    % 执行完整的PDDL更新流程
    (update_entity_from_pddl ->
        write('  ✓ 完整流程执行成功'), nl,
        
        % 检查实体位置是否更新
        at_entity(NewLoc),
        write('  更新后实体位置: '), write(NewLoc), nl,
        (NewLoc = InitialLoc ->
            write('  ⚠ 实体位置未改变（可能规划为空或实体已到达目标）'), nl
        ;
            write('  ✓ 实体位置已更新'), nl
        )
    ;
        write('  ✗ 完整流程执行失败'), nl,
        fail
    ).

% ----------------------------------------------------------------------------
% 辅助函数
% ----------------------------------------------------------------------------

% 使用 pddl_interface 模块中的函数
% 不再需要重新定义，直接使用模块前缀

exists_file(File) :-
    access_file(File, read).

