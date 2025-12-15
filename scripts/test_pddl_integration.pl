% ============================================================================
% test_pddl_integration.pl
% ============================================================================
% PDDL integration test script
% Checks whether the PDDL environment works and is properly integrated with Prolog
% ============================================================================

% Define file search paths to avoid relative path warnings
:- prolog_load_context(file, ThisFile),
   absolute_file_name(ThisFile, AbsThisFile),
   file_directory_name(AbsThisFile, ScriptDir),
   file_directory_name(ScriptDir, ProjectRoot),
   atomic_list_concat([ProjectRoot, '/prolog'], PrologDir),
   asserta(user:file_search_path(project, ProjectRoot)),
   asserta(user:file_search_path(prolog, PrologDir)).

% Load modules using the defined search paths
:- use_module(prolog(knowledge_base)).
:- use_module(prolog(game_state)).
:- use_module(prolog(pddl_interface)).

% ----------------------------------------------------------------------------
% Main test entry
% ----------------------------------------------------------------------------

test_pddl_integration :-
    write('========================================'), nl,
    write('PDDL Integration Test'), nl,
    write('========================================'), nl,
    nl,
    
    % Test 1: check whether a PDDL planner is installed
    test_planner_installed,
    nl,
    
    % Test 2: check whether PDDL files exist
    test_pddl_files_exist,
    nl,
    
    % Test 3: initialize game state
    test_init_game_state,
    nl,
    
    % Test 4: test generating a PDDL problem file
    test_generate_problem,
    nl,
    
    % Test 5: test calling the planner
    test_call_planner,
    nl,
    
    % Test 6: test parsing plan output
    test_parse_plan,
    nl,
    
    % Test 7: end-to-end integration test
    test_full_integration,
    nl,
    
    write('========================================'), nl,
    write('All tests completed!'), nl,
    write('========================================'), nl.

% ----------------------------------------------------------------------------
% Test 1: check whether a PDDL planner is installed
% ----------------------------------------------------------------------------

test_planner_installed :-
    write('[Test 1] Checking whether a PDDL planner is installed...'), nl,
    pddl_interface:pddl_planner_command(PlannerCmd),
    write('  Planner command: '), write(PlannerCmd), nl,
    
    % Try executing the planner command (use --help or -h to probe)
    atomic_list_concat([PlannerCmd, ' -h 2>&1'], TestCmd),
    catch(
        (shell(TestCmd, Status),
         (Status = 0 ->
             write('  ✓ Planner is installed and executable'), nl
         ;
             write('  ⚠ Planner command exists but returned a non-zero status (may be normal)'), nl
         )),
        Error,
        (write('  ✗ Planner is not installed or not executable: '), write(Error), nl,
         write('  Hint: install Fast-Forward (ff) or another PDDL planner'), nl,
         fail)
    ).

% ----------------------------------------------------------------------------
% Test 2: check whether PDDL files exist
% ----------------------------------------------------------------------------

test_pddl_files_exist :-
    write('[Test 2] Checking whether PDDL files exist...'), nl,
    
    % First, test whether project_root works
    (pddl_interface:project_root(Root) ->
        write('  Project root: '), write(Root), nl
    ;
        write('  ⚠ Unable to determine project root'), nl
    ),
    
    (pddl_interface:pddl_domain_path(DomainPathRel) ->
        write('  Domain file relative path: '), write(DomainPathRel), nl,
        (pddl_interface:get_pddl_path(DomainPathRel, DomainPath) ->
            write('  Domain file absolute path: '), write(DomainPath), nl,
            
            (exists_file(DomainPath) ->
                write('  ✓ Domain file exists'), nl
            ;
                write('  ✗ Domain file does not exist'), nl,
                write('  Tried file path: '), write(DomainPath), nl,
                % Try listing directory contents for debugging
                file_directory_name(DomainPath, DomainDir),
                (exists_file(DomainDir) ->
                    write('  Directory exists; listing contents:'), nl,
                    directory_files(DomainDir, Files),
                    forall(member(File, Files), (write('    - '), write(File), nl))
                ;
                    write('  Directory also does not exist: '), write(DomainDir), nl
                ),
                fail
            )
        ;
            write('  ✗ Unable to resolve domain file path'), nl,
            fail
        )
    ;
        write('  ✗ Unable to get domain file relative path'), nl,
        fail
    ),
    
    (pddl_interface:pddl_problem_path(ProblemPathRel) ->
        write('  Problem file relative path: '), write(ProblemPathRel), nl,
        (pddl_interface:get_pddl_path(ProblemPathRel, ProblemPath) ->
            write('  Problem file absolute path: '), write(ProblemPath), nl,
            write('  (Problem file is generated at runtime)'), nl
        ;
            write('  ⚠ Unable to resolve problem file path'), nl
        )
    ;
        write('  ⚠ Unable to get problem file relative path'), nl
    ).

% ----------------------------------------------------------------------------
% Test 3: initialize game state
% ----------------------------------------------------------------------------

test_init_game_state :-
    write('[Test 3] Initializing game state...'), nl,
    
    (init_game_state ->
        write('  ✓ Game state initialized successfully'), nl,
        at_player(PlayerLoc),
        write('  Player location: '), write(PlayerLoc), nl,
        at_entity(EntityLoc),
        write('  Entity location: '), write(EntityLoc), nl,
        sanity(S),
        write('  Sanity: '), write(S), nl
    ;
        write('  ✗ Failed to initialize game state'), nl,
        fail
    ).

% ----------------------------------------------------------------------------
% Test 4: test generating a PDDL problem file
% ----------------------------------------------------------------------------

test_generate_problem :-
    write('[Test 4] Testing PDDL problem generation...'), nl,
    
    pddl_interface:pddl_problem_path(ProblemPathRel),
    pddl_interface:get_pddl_path(ProblemPathRel, ProblemPath),
    
    (generate_pddl_problem(ProblemPath) ->
        write('  ✓ PDDL problem generated successfully'), nl,
        write('  File path: '), write(ProblemPath), nl,
        
        % Read and show the first few lines
        (open(ProblemPath, read, Stream) ->
            read_line_to_string(Stream, Line1),
            read_line_to_string(Stream, Line2),
            read_line_to_string(Stream, Line3),
            close(Stream),
            write('  File preview:'), nl,
            write('    '), write(Line1), nl,
            write('    '), write(Line2), nl,
            write('    '), write(Line3), nl
        ;
            true
        )
    ;
        write('  ✗ Failed to generate PDDL problem file'), nl,
        fail
    ).

% ----------------------------------------------------------------------------
% Test 5: test calling the planner
% ----------------------------------------------------------------------------

test_call_planner :-
    write('[Test 5] Testing planner invocation...'), nl,
    
    pddl_interface:pddl_domain_path(DomainPathRel),
    pddl_interface:get_pddl_path(DomainPathRel, DomainPath),
    pddl_interface:pddl_problem_path(ProblemPathRel),
    pddl_interface:get_pddl_path(ProblemPathRel, ProblemPath),
    
    (call_pddl_planner(DomainPath, ProblemPath) ->
        write('  ✓ Planner call succeeded'), nl,
        
        % Check whether a plan file was generated
        pddl_interface:pddl_plan_path(PlanPathRel),
        pddl_interface:get_pddl_path(PlanPathRel, PlanPath),
        (exists_file(PlanPath) ->
            write('  ✓ Plan file generated: '), write(PlanPath), nl
        ;
            write('  ⚠ Plan file not generated (planner may not have found a plan)'), nl
        )
    ;
        write('  ✗ Planner call failed'), nl,
        write('  Hint: please check that the planner is installed and configured correctly'), nl,
        fail
    ).

% ----------------------------------------------------------------------------
% Test 6: test parsing plan output
% ----------------------------------------------------------------------------

test_parse_plan :-
    write('[Test 6] Testing plan parsing...'), nl,
    
    pddl_interface:pddl_plan_path(PlanPathRel),
    pddl_interface:get_pddl_path(PlanPathRel, PlanPath),
    
    (exists_file(PlanPath) ->
        (parse_plan_result(PlanPath, Actions) ->
            write('  ✓ Plan parsed successfully'), nl,
            length(Actions, ActionCount),
            write('  Number of actions: '), write(ActionCount), nl,
            (Actions = [] ->
                write('  ⚠ Plan is empty (planner may not have found a solution)'), nl
            ;
                write('  Actions:'), nl,
                write_actions(Actions)
            )
        ;
            write('  ✗ Failed to parse plan output'), nl,
            write('  Hint: the plan file format may be unexpected'), nl,
            fail
        )
    ;
        write('  ⚠ Plan file does not exist; skipping parse test'), nl
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
% Test 7: end-to-end integration test
% ----------------------------------------------------------------------------

test_full_integration :-
    write('[Test 7] End-to-end integration test...'), nl,
    
    % Record initial entity location
    at_entity(InitialLoc),
    write('  Initial entity location: '), write(InitialLoc), nl,
    
    % Execute full PDDL update flow
    (update_entity_from_pddl ->
        write('  ✓ End-to-end flow executed successfully'), nl,
        
        % Check whether entity location was updated
        at_entity(NewLoc),
        write('  Entity location after update: '), write(NewLoc), nl,
        (NewLoc = InitialLoc ->
            write('  ⚠ Entity location did not change (plan may be empty or entity already at goal)'), nl
        ;
            write('  ✓ Entity location updated'), nl
        )
    ;
        write('  ✗ End-to-end flow failed'), nl,
        fail
    ).

% ----------------------------------------------------------------------------
% Helper functions
% ----------------------------------------------------------------------------

% Use functions from the pddl_interface module
% No need to re-define them; use the module prefix directly

exists_file(File) :-
    access_file(File, read).

