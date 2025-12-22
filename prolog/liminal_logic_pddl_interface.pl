% ============================================================================
% liminal_logic_pddl_interface.pl
% ============================================================================
% Interface module between Prolog and PDDL.
% Responsibilities:
%   1. Generate a PDDL problem file from the current Prolog game state
%   2. Invoke a PDDL planner (e.g. Fast-Forward)
%   3. Parse the plan output and update the entity location
% ============================================================================

:- module(pddl_interface, [
    generate_pddl_problem/1,
    generate_pddl_problem_with_door_attempt/3,
    call_pddl_planner/2,
    update_entity_from_pddl/0,
    update_entity_on_failed_move/1,
    update_entity_simple/0,
    parse_plan_result/2,
    pddl_planner_command/1,
    pddl_domain_path/1,
    pddl_problem_path/1,
    pddl_plan_path/1,
    get_pddl_path/2,
    project_root/1
]).

:- use_module(liminal_logic_game_state).
:- use_module(liminal_logic_knowledge_base).
:- use_module(liminal_logic_win_conditions).

% ----------------------------------------------------------------------------
% Path configuration
% ----------------------------------------------------------------------------

% PDDL file paths (relative to the project root).
% Note: these paths are resolved to absolute paths at runtime.
pddl_domain_path('pddl/domains/adversary_domain.pddl').
pddl_problem_path('pddl/problems/current_problem.pddl').
pddl_plan_path('pddl/problems/plan.txt').

% PDDL planner command (adjust based on what is installed).
% Common options:
% - Fast-Forward: 'ff -o DOMAIN -f PROBLEM'
% - Fast-Downward: 'fast-downward.py DOMAIN PROBLEM --search "astar(lmcut())"'
% - Other planners...
% If the planner is not on PATH, use an absolute path, e.g.:
% pddl_planner_command('/usr/local/bin/ff').
pddl_planner_command('ff').

% Get the absolute project root path.
% Derived from the location of liminal_logic_pddl_interface.pl so it works regardless of cwd.
project_root(Root) :-
    % Use source_file to get the module file path (most reliable)
    (source_file(pddl_interface:project_root(_), ModuleFile) ->
        absolute_file_name(ModuleFile, AbsFile),
        file_directory_name(AbsFile, PrologDir),
        file_directory_name(PrologDir, Root)
    ;
        % Fallback: derive from current file context
        prolog_load_context(file, CurrentFile),
        absolute_file_name(CurrentFile, AbsFile),
        file_directory_name(AbsFile, PrologDir),
        file_directory_name(PrologDir, Root)
    ).

% Get absolute path for a PDDL file
get_pddl_path(RelativePath, AbsolutePath) :-
    project_root(Root),
    atomic_list_concat([Root, '/', RelativePath], AbsolutePath).

% ----------------------------------------------------------------------------
% Main entry: update entity location from PDDL
% ----------------------------------------------------------------------------
% update_entity_from_pddl
% Generate a PDDL problem, call the planner, parse the result, and update the entity location
% ----------------------------------------------------------------------------

% Skip one entity update this turn (e.g. dropping the tape recorder: act starting next turn)
update_entity_from_pddl :-
    consume_suppress_entity_update_once,
    !.

update_entity_from_pddl :-
    % If the entity has reached the noise location, clear the bait to avoid being locked onto it
    (active_noise_at(NoiseLoc), at_entity(NoiseLoc) ->
        clear_active_noise
    ;
        true
    ),
    % Check whether the Howler has started chasing
    (howler_chasing ->
        % Howler is chasing: use the optimized planning logic
        at_entity(EntityLoc),
        at_player(PlayerLoc),
        % Check whether there is a cached plan
        (cached_entity_plan(CachedPlan) ->
            % Cached plan exists; decide whether replanning is needed
            (need_replan(CachedPlan, EntityLoc, PlayerLoc) ->
                % Need to replan
                clear_cached_entity_plan,
                generate_and_execute_plan
            ;
                % Use cached plan and execute the next step
                execute_next_step_from_plan(CachedPlan)
            )
        ;
            % No cache; generate a new plan
            generate_and_execute_plan
        )
    ;
        % Howler is not yet chasing:
        % - By default, only update the entity when the player moves (so look doesn't move it)
        % - Some non-move commands (e.g. dropping the tape recorder) should still trigger updates
        at_player(PlayerLoc),
        player_previous_location(PlayerPrevLoc),
        ( (entity_update_requested ; PlayerLoc \= PlayerPrevLoc) ->
            % Entity update explicitly requested this turn; clear the request flag
            clear_entity_update_request,
            % Player location changed; proceed with entity update
            % 1. Generate the PDDL problem for the current state
            pddl_problem_path(ProblemPathRel),
            get_pddl_path(ProblemPathRel, ProblemPath),
            generate_pddl_problem(ProblemPath),
            
            % 2. Call the PDDL planner
            pddl_domain_path(DomainPathRel),
            get_pddl_path(DomainPathRel, DomainPath),
            call_pddl_planner(DomainPath, ProblemPath),
            
            % 3. Parse the plan output and update entity location
            pddl_plan_path(PlanPathRel),
            get_pddl_path(PlanPathRel, PlanPath),
            (exists_file(PlanPath) ->
                parse_plan_result(PlanPath, Actions),
                (Actions = [] ->
                    % Planner found no actions
                    write('PDDL planner found no actions. Entity stays in place.'), nl
                ;
                    write('Parsed actions: '), write(Actions), nl,
                    % Execute only one action per turn: take the first supported action (move/stay/chase)
                    filter_supported_actions(Actions, SupportedActions),
                    (SupportedActions = [FirstAction|_] ->
                        apply_entity_actions([FirstAction])
                    ;
                        write('No supported entity actions found in plan. Entity stays in place.'), nl
                    ),
                    write('Entity moved based on PDDL plan.'), nl
                )
            ;
                % If planning fails, keep the entity in its current location
                write('PDDL planner did not generate a plan. Entity stays in place.'), nl
            )
        ;
            % Player location did not change (e.g. look), and no forced update request: do not update entity
            true
        )
    ),
    !.

% ----------------------------------------------------------------------------
% Optimized planning logic
% ----------------------------------------------------------------------------

% Generate and execute a plan (execute only the first step)
generate_and_execute_plan :-
    % 1. Generate the PDDL problem for the current state
    pddl_problem_path(ProblemPathRel),
    get_pddl_path(ProblemPathRel, ProblemPath),
    generate_pddl_problem(ProblemPath),
    
    % 2. Call the PDDL planner
    pddl_domain_path(DomainPathRel),
    get_pddl_path(DomainPathRel, DomainPath),
    call_pddl_planner(DomainPath, ProblemPath),
    
    % 3. Parse the plan output
    pddl_plan_path(PlanPathRel),
    get_pddl_path(PlanPathRel, PlanPath),
    (exists_file(PlanPath) ->
        parse_plan_result(PlanPath, Actions),
        filter_supported_actions(Actions, SupportedActions),
        (SupportedActions = [] ->
            % Planner found no actions; fall back to simple logic
            update_entity_towards_player,
            clear_cached_entity_plan
        ;
            % Execute only the first step; cache remaining actions (if any) for the next turn
            SupportedActions = [FirstAction|RestActions],
            (RestActions = [] ->
                clear_cached_entity_plan
            ;
                set_cached_entity_plan(RestActions)
            ),
            apply_entity_actions([FirstAction])
        )
    ;
        % If planning fails, fall back to simple logic
        update_entity_towards_player,
        clear_cached_entity_plan
    ),
    !.

% Execute the next step from the cached plan
execute_next_step_from_plan([]) :-
    % Plan finished; clear cache
    clear_cached_entity_plan,
    % Continue with simple logic
    update_entity_towards_player.
execute_next_step_from_plan([NextAction|RestActions]) :-
    % Execute next step
    apply_entity_actions([NextAction]),
    % Update cache (remove the executed step)
    set_cached_entity_plan(RestActions).

% Check whether replanning is needed
need_replan(_CachedPlan, EntityLoc, PlayerLoc) :-
    % If the entity and player are already in the same room, no replanning is needed
    EntityLoc = PlayerLoc,
    !,
    fail.
need_replan(CachedPlan, _EntityLoc, _PlayerLoc) :-
    % If the cached plan is empty, replan
    CachedPlan = [],
    !.
need_replan([FirstAction|_], EntityLoc, _PlayerLoc) :-
    % Check whether the first planned action is still valid
    (FirstAction = action(move, [_, From, _To]) ->
        (string(From) ->
            atom_string(FromAtom, From)
        ;
            FromAtom = From
        ),
        % If the entity is not at the planned starting location, replan
        EntityLoc \= FromAtom
    ;
        % Other action types: always replan
        true
    ),
    !.
need_replan(_, _, _) :-
    % By default, no replanning is required
    fail.

% ----------------------------------------------------------------------------
% Handling failed move attempts
% update_entity_on_failed_move/1
% When the player attempts to move but fails (e.g. missing a key), the Howler
% should still take an action.
% Args:
%   Direction - the direction the player attempted to move
% ----------------------------------------------------------------------------

update_entity_on_failed_move(Direction) :-
    % Get the player's current location and the target room of the attempted move
    at_player(PlayerLoc),
    connect(PlayerLoc, Direction, TargetRoom),
    at_entity(EntityLoc),
    % 1. Generate a PDDL problem including the player's door-attempt action
    pddl_problem_path(ProblemPathRel),
    get_pddl_path(ProblemPathRel, ProblemPath),
    generate_pddl_problem_with_door_attempt(ProblemPath, PlayerLoc, TargetRoom),
    
    % 2. Call the PDDL planner
    pddl_domain_path(DomainPathRel),
    get_pddl_path(DomainPathRel, DomainPath),
    call_pddl_planner(DomainPath, ProblemPath),
    
    % 3. Parse the plan and update entity location
    pddl_plan_path(PlanPathRel),
    get_pddl_path(PlanPathRel, PlanPath),
    (exists_file(PlanPath) ->
        parse_plan_result(PlanPath, Actions),
        (Actions = [] ->
            % Planner found no actions; fall back to simple logic
            (EntityLoc = PlayerLoc ->
                check_entity_player_same_room
            ;
                (find_path_to_player(EntityLoc, PlayerLoc, NextRoom) ->
                    set_entity_location(NextRoom),
                    write('The Howler heard the door attempt and moves towards you!'), nl,
                    check_entity_player_same_room
                ;
                    write('The Howler heard the door attempt but cannot find a path.'), nl
                )
            )
        ;
            write('Parsed actions: '), write(Actions), nl,
            % Execute only one action per turn: take the first supported action (move/stay/chase)
            filter_supported_actions(Actions, SupportedActions),
            (SupportedActions = [FirstAction|_] ->
                apply_entity_actions([FirstAction])
            ;
                write('No supported entity actions found in plan. Entity stays in place.'), nl
            ),
            write('Entity moved based on PDDL plan (player attempted door).'), nl
        )
    ;
        % If planning fails, fall back to simple logic
        (EntityLoc = PlayerLoc ->
            check_entity_player_same_room
        ;
            (find_path_to_player(EntityLoc, PlayerLoc, NextRoom) ->
                set_entity_location(NextRoom),
                write('The Howler heard the door attempt and moves towards you!'), nl,
                check_entity_player_same_room
            ;
                write('The Howler heard the door attempt but cannot find a path.'), nl
            )
        )
    ),
    !.

% ----------------------------------------------------------------------------
% Generate PDDL Problem File
% ----------------------------------------------------------------------------
% generate_pddl_problem(+OutputFile)
% Generate a PDDL problem from the current game state and write it to OutputFile
% ----------------------------------------------------------------------------

generate_pddl_problem(OutputFile) :-
    generate_pddl_problem_with_door_attempt(OutputFile, false, false).

% ----------------------------------------------------------------------------
% Generate a PDDL Problem file including the player's door-attempt action
% generate_pddl_problem_with_door_attempt(+OutputFile, +FromRoom, +ToRoom)
% Generates a PDDL problem from the current game state and includes information
% about the player's attempted door action.
% If FromRoom and ToRoom are false or unbound, no door-attempt info is included.
% ----------------------------------------------------------------------------

generate_pddl_problem_with_door_attempt(OutputFile, FromRoom, ToRoom) :-
    open(OutputFile, write, Stream),
    
    % Check whether we should include a door-attempt action
    (FromRoom \= false, ToRoom \= false, FromRoom \= _, ToRoom \= _ ->
        HasDoorAttempt = true
    ;
        HasDoorAttempt = false
    ),
    
    % Write header
    write(Stream, '(define (problem backrooms_current)'), nl(Stream),
    write(Stream, '  (:domain adversary)'), nl(Stream),
    nl(Stream),
    
    % Write object declarations
    write_objects(Stream),
    nl(Stream),
    
    % Write initial state (including door-attempt info)
    write(Stream, '  (:init'), nl(Stream),
    write_initial_state(Stream, HasDoorAttempt, FromRoom, ToRoom),
    write(Stream, '  )'), nl(Stream),
    nl(Stream),
    
    % Write goal state (if there is a door-attempt action, pass the info)
    (HasDoorAttempt ->
        write_goal_with_door_attempt(Stream, FromRoom)
    ;
        write_goal(Stream)
    ),
    nl(Stream),
    
    write(Stream, ')'), nl(Stream),
    close(Stream).

% ----------------------------------------------------------------------------
% Write object declarations
% ----------------------------------------------------------------------------

write_objects(Stream) :-
    write(Stream, '  (:objects'), nl(Stream),
    write(Stream, '    howler - entity'), nl(Stream),
    write(Stream, '    player1 - player'), nl(Stream),
    write(Stream, '    start_point yellow_hallway dark_corridor electrical_room'), nl(Stream),
    write(Stream, '    the_hub manila_room supply_closet dead_end - location'), nl(Stream),
    write(Stream, '  )'), nl(Stream).

% ----------------------------------------------------------------------------
% Write initial state
% ----------------------------------------------------------------------------

write_initial_state(Stream) :-
    write_initial_state(Stream, false, _, _).

write_initial_state(Stream, HasDoorAttempt, FromRoom, ToRoom) :-
    % Entity location
    (at_entity(EntityLoc) ->
        write(Stream, '    (at howler '), write(Stream, EntityLoc), write(Stream, ')'), nl(Stream)
    ; true),
    
    % Player location (current)
    (at_player(PlayerLoc) ->
        write(Stream, '    (at_player player1 '), write(Stream, PlayerLoc), write(Stream, ')'), nl(Stream)
    ; true),
    
    % Room connections (bidirectional)
    write_connections(Stream),
    
    % Noise location (if present)
    write_noise_locations(Stream),
    
    % Check whether the Howler has started chasing
    at_entity(EntityLoc),
    at_player(PlayerLoc),
    (howler_chasing ->
        % Howler is chasing: set player_known to the player's current location
        (PlayerLoc \= EntityLoc ->
            write(Stream, '    (player_known player1 '), write(Stream, PlayerLoc), write(Stream, ')'), nl(Stream)
        ; true)
    ;
        % Howler is not yet chasing: use the original logic.
        % Only set player_known when leaving a room adjacent to the Howler,
        % and the move is not into the Howler's room.
        % If the player's previous location was adjacent, the current location is not adjacent,
        % and the current location is not the Howler's room.
        (player_previous_location(PlayerPrevLoc),
         PlayerPrevLoc \= PlayerLoc,
         connect(EntityLoc, _, PlayerPrevLoc),
         \+ connect(EntityLoc, _, PlayerLoc),
         PlayerLoc \= EntityLoc ->
            % Player left an adjacent room and did not enter the Howler's room: set player_known
            write(Stream, '    (player_known player1 '), write(Stream, PlayerPrevLoc), write(Stream, ')'), nl(Stream)
        ; true)
    ),
    
    % If the player attempted a door action, write it into the state.
    % Note: attempting a door action from an adjacent room does not move the Howler.
    % The Howler only moves when the player leaves an adjacent room.
    (HasDoorAttempt ->
        write(Stream, '    (player_attempted_door player1 '), 
        write(Stream, FromRoom), write(Stream, ' '), 
        write(Stream, ToRoom), write(Stream, ')'), nl(Stream)
    ; true).

% ----------------------------------------------------------------------------
% Write room connections
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
% Write noise location (if the tape recorder was dropped)
% ----------------------------------------------------------------------------

write_noise_locations(Stream) :-
    (active_noise_at(Loc) ->
        write(Stream, '    (noise_at '), write(Stream, Loc), write(Stream, ')'), nl(Stream)
    ; true).

% ----------------------------------------------------------------------------
% Write goal state
% ----------------------------------------------------------------------------

write_goal(Stream) :-
    % Get player and entity locations
    at_player(PlayerLoc),
    at_entity(EntityLoc),
    % If active noise exists (tape recorder bait), prioritize setting the goal to the noise location
    (active_noise_at(NoiseLoc), NoiseLoc \= EntityLoc ->
        write(Stream, '  (:goal (at howler '), write(Stream, NoiseLoc), write(Stream, '))'), nl(Stream)
    ;
    % Check whether the Howler has started chasing
    (howler_chasing ->
        % Howler is chasing: chase the player's current location
        (PlayerLoc \= EntityLoc ->
            write(Stream, '  (:goal (at howler '), write(Stream, PlayerLoc), write(Stream, '))'), nl(Stream)
        ;
            % Already in the same room; stay in place
            write(Stream, '  (:goal (at howler '), write(Stream, EntityLoc), write(Stream, '))'), nl(Stream)
        )
    ;
        % Howler is not yet chasing: use the original logic.
        % Check whether there is player_known (set in write_initial_state).
        % If player_known exists, chase that location; otherwise stay in place.
        (player_previous_location(PlayerPrevLoc),
         PlayerPrevLoc \= PlayerLoc,
         connect(EntityLoc, _, PlayerPrevLoc),
         \+ connect(EntityLoc, _, PlayerLoc),
         PlayerLoc \= EntityLoc ->
            % Player left an adjacent room and did not enter the Howler's room: chase previous location
            write(Stream, '  (:goal (at howler '), write(Stream, PlayerPrevLoc), write(Stream, '))'), nl(Stream)
        ;
            % No known player location; stay in place
            write(Stream, '  (:goal (at howler '), write(Stream, EntityLoc), write(Stream, '))'), nl(Stream)
        )
    )).

% ----------------------------------------------------------------------------
% Write goal state (when the player attempted a door action)
% ----------------------------------------------------------------------------

write_goal_with_door_attempt(Stream, _PlayerLoc) :-
    % When the player attempts a door action, the Howler does not move (stays put).
    % The Howler only moves when the player leaves an adjacent room.
    at_entity(EntityLoc),
    write(Stream, '  (:goal (at howler '), write(Stream, EntityLoc), write(Stream, '))'), nl(Stream).

% ----------------------------------------------------------------------------
% Call the PDDL planner
% ----------------------------------------------------------------------------
% call_pddl_planner(+DomainPath, +ProblemPath)
% Invoke an external PDDL planner and generate a plan output file
% ----------------------------------------------------------------------------

call_pddl_planner(DomainPath, ProblemPath) :-
    pddl_planner_command(PlannerCmd),
    pddl_plan_path(PlanPath),
    
    % Check input files exist
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
    
    % Build planner command
    % Note: different planners may require different command formats.
    % Fast-Forward format: ff -o DOMAIN -f PROBLEM > PLAN
    % If the planner is not on PATH, an absolute path may be required.
    atomic_list_concat([PlannerCmd, ' -o ', DomainPath, ' -f ', ProblemPath, ' > ', PlanPath, ' 2>&1'], Command),
    
    % Execute command
    shell(Command, Status),
    (Status = 0 ->
        write('PDDL planner executed successfully.'), nl
    ;
        write('Warning: PDDL planner returned non-zero status: '), write(Status), nl,
        write('This may indicate no plan was found, or the planner is not installed.'), nl
    ).

% ----------------------------------------------------------------------------
% Parse plan output
% ----------------------------------------------------------------------------
% parse_plan_result(+PlanPath, -Actions)
% Parse the action sequence from a planner output file.
% Returns a list of actions, each in the form: action(Name, Args)
% ----------------------------------------------------------------------------

parse_plan_result(PlanPath, Actions) :-
    open(PlanPath, read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    filter_action_lines(Lines, ActionLines),
    parse_action_lines(ActionLines, Actions).

% Read all lines from a file
read_lines(Stream, Lines) :-
    read_line_to_string(Stream, Line),
    (Line = end_of_file ->
        Lines = []
    ;
        Lines = [Line|Rest],
        read_lines(Stream, Rest)
    ).

% Filter action lines (usually start with action names like "move", "chase", etc.)
filter_action_lines([], []).
filter_action_lines([Line|Rest], Filtered) :-
    (Line = end_of_file ->
        Filtered = []
    ;
        % Trim whitespace on both ends
        trim_string(Line, Trimmed),
        (is_action_line(Trimmed) ->
            Filtered = [Trimmed|RestFiltered]
        ;
            Filtered = RestFiltered
        ),
        filter_action_lines(Rest, RestFiltered)
    ).

% Determine whether a line is an action line.
% Different planners may output different formats; handle common ones here.
% Possible formats: "stay ...", "move ...", "chase ...", "0: (move ...)", "(move ...)"
is_action_line(Line) :-
    string_lower(Line, LowerLine),
    (sub_string(LowerLine, _, _, _, "stay") -> true
    ; sub_string(LowerLine, _, _, _, "move") -> true
    ; sub_string(LowerLine, _, _, _, "chase") -> true
    ; false).

% Parse action lines
parse_action_lines([], []).
parse_action_lines([Line|Rest], [Action|Actions]) :-
    parse_action_line(Line, Action),
    parse_action_lines(Rest, Actions).

% Parse a single action line.
% Examples: "move howler electrical_room the_hub"
%          "(move howler electrical_room the_hub)"
%          "0: (move howler electrical_room the_hub) [0]"
parse_action_line(Line, action(Name, Args)) :-
    % Remove an optional line-number prefix (e.g. "0: ")
    % Use split_string to separate the prefix from the action content
    split_string(Line, ":", " ", Parts),
    (Parts = [_Prefix, ActionPart|_] ->
        % Has a prefix; use the action part
        AfterColon = ActionPart
    ;
        % No prefix; use the full line
        AfterColon = Line
    ),
    % Remove optional parentheses and whitespace
    string_chars(AfterColon, Chars),
    exclude(==('('), Chars, Chars1),
    exclude(==(')'), Chars1, Chars2),
    % Remove brackets and their contents (e.g. "[0]")
    remove_brackets(Chars2, Chars3),
    string_chars(Trimmed, Chars3),
    trim_string(Trimmed, FinalTrimmed),
    split_string(FinalTrimmed, ' ', ' ', Parts2),
    % Filter out empty strings
    exclude(==(""), Parts2, FilteredParts),
    (FilteredParts = [] ->
        % If no valid parts exist, fail
        fail
    ;
        FilteredParts = [NameStr|ArgStrs],
        (ArgStrs = [] ->
            Args = []
        ;
            % Filter out empty strings
            exclude(==(""), ArgStrs, FilteredArgs),
            maplist(atom_string, Args, FilteredArgs)
        ),
        atom_string(Name, NameStr)
    ).

% Remove bracketed content
remove_brackets([], []).
remove_brackets(['['|Rest], Result) :-
    remove_until_bracket(Rest, Result).
remove_brackets([H|T], [H|Result]) :-
    remove_brackets(T, Result).

remove_until_bracket([], []).
remove_until_bracket([']'|Rest], Rest).
remove_until_bracket([_|Rest], Result) :-
    remove_until_bracket(Rest, Result).

% ----------------------------------------------------------------------------
% Apply entity actions
% ----------------------------------------------------------------------------
% apply_entity_actions(+Actions)
% Update entity location based on the parsed action sequence
% ----------------------------------------------------------------------------

% Keep only actions supported by the current implementation (stable choice for "one step per turn")
supported_entity_action(action(stay, _)).
supported_entity_action(action(move, _)).
supported_entity_action(action(chase, _)).

filter_supported_actions([], []).
filter_supported_actions([A|Rest], [A|FilteredRest]) :-
    supported_entity_action(A),
    !,
    filter_supported_actions(Rest, FilteredRest).
filter_supported_actions([_|Rest], Filtered) :-
    filter_supported_actions(Rest, Filtered).

apply_entity_actions([]).
apply_entity_actions([action(stay, [_, _])|_Rest]) :-
    % Stay action does not change location
    write('The Howler stays in place.'), nl.
apply_entity_actions([action(move, [_, From, To])|_Rest]) :-
    % Execute only the first move action
    (string(To) ->
        atom_string(ToAtom, To)
    ;
        ToAtom = To
    ),
    (string(From) ->
        atom_string(FromAtom, From)
    ;
        FromAtom = From
    ),
    set_entity_location(ToAtom),
    write('The Howler moves from '), write(FromAtom), write(' to '), write(ToAtom), write('.'), nl,
    % If we reached the noise location, clear the bait
    (active_noise_at(ToAtom) ->
        clear_active_noise,
        write('The noise dies out.'), nl
    ;
        true
    ),
    % Check whether the entity and player are in the same room
    check_entity_player_same_room.
apply_entity_actions([action(chase, [_, _, To, _])|_Rest]) :-
    % Compatibility with the older chase action (backwards compatible)
    (string(To) ->
        atom_string(ToAtom, To)
    ;
        ToAtom = To
    ),
    set_entity_location(ToAtom),
    write('The Howler chases to '), write(ToAtom), write('.'), nl,
    % Check whether the entity and player are in the same room
    check_entity_player_same_room.
apply_entity_actions([_|Rest]) :-
    % Ignore unknown actions
    apply_entity_actions(Rest).

% ----------------------------------------------------------------------------
% Helper: check whether the entity and player are in the same room
% ----------------------------------------------------------------------------

check_entity_player_same_room :-
    at_player(PlayerLoc),
    at_entity(EntityLoc),
    PlayerLoc = EntityLoc,
    check_lose,  % If in the same room, trigger game over
    !.
check_entity_player_same_room.  % If not in the same room, continue

% ----------------------------------------------------------------------------
% Helper: check whether a file exists
% ----------------------------------------------------------------------------

exists_file(File) :-
    access_file(File, read).

% ----------------------------------------------------------------------------
% Helper: string utilities
% ----------------------------------------------------------------------------

% Trim whitespace at both ends.
% Uses normalize_space to normalize whitespace, then manually removes leading/trailing spaces.
trim_string(String, Trimmed) :-
    % First normalize whitespace (collapse multiple spaces into one)
    normalize_space(string(Normalized), String),
    % Remove leading/trailing whitespace
    string_chars(Normalized, Chars),
    trim_chars_left(Chars, Chars1),
    trim_chars_right(Chars1, TrimmedChars),
    string_chars(Trimmed, TrimmedChars).

% Remove leading whitespace characters
trim_chars_left([], []).
trim_chars_left([H|T], Result) :-
    (char_type(H, space) ->
        trim_chars_left(T, Result)
    ;
        Result = [H|T]
    ).

% Remove trailing whitespace characters
trim_chars_right([], []).
trim_chars_right(List, Result) :-
    reverse(List, Reversed),
    trim_chars_left(Reversed, TrimmedReversed),
    reverse(TrimmedReversed, Result).

% ----------------------------------------------------------------------------
% Simple entity movement logic (fallback)
% Used when the PDDL planner is unavailable
% ----------------------------------------------------------------------------
% update_entity_towards_player/0
% Uses simple Prolog rules to move the entity toward the player's current location
% Mechanism: find the shortest path from the Howler to the player, then move one step
% ----------------------------------------------------------------------------

update_entity_towards_player :-
    at_entity(EntityLoc),
    at_player(PlayerLoc),
    (EntityLoc = PlayerLoc ->
        % Already in the same room; check whether to trigger game over
        check_entity_player_same_room
    ;
        % Not in the same room; find the shortest path and move one step
        (find_path_to_player(EntityLoc, PlayerLoc, NextRoom) ->
            set_entity_location(NextRoom),
            write('The Howler moves towards you!'), nl,
            check_entity_player_same_room
        ;
            % No path found; keep current location
            write('The Howler cannot find a path to you.'), nl
        )
    ),
    !.

% ----------------------------------------------------------------------------
% update_entity_simple/0
% Use simple Prolog rules for the entity to chase the player
% Mechanism: if the player is in an adjacent room, move into the player's room
% ----------------------------------------------------------------------------

update_entity_simple :-
    at_entity(EntityLoc),
    at_player(PlayerLoc),
    player_previous_location(PlayerPrevLoc),
    % Check whether the player's location actually changed (only move changes it)
    (PlayerLoc = PlayerPrevLoc ->
        % Player location did not change (e.g. look); do not update entity
        true
    ;
        % Player location changed; check whether the entity should move
        (EntityLoc = PlayerLoc ->
            % Entity and player are in the same room; trigger game over
            check_entity_player_same_room
        ;
            % Check whether the player's previous location was adjacent
            % This makes the entity start chasing on the turn after the player moves
            (connect(EntityLoc, _, PlayerPrevLoc) ->
                % Player was adjacent last turn; move the entity to the player's previous location
                % Since the player already moved, chase their last-known adjacent location
                set_entity_location(PlayerPrevLoc),
                write('The Howler heard you and moves towards where you were!'), nl,
                % Check whether the entity and player are in the same room
                check_entity_player_same_room
            ;
                % Player was not adjacent last turn; try to find a path to the player's previous location
                (find_path_to_player(EntityLoc, PlayerPrevLoc, NextRoom) ->
                    set_entity_location(NextRoom),
                    write('The Howler is searching...'), nl,
                    % Check whether the entity and player are in the same room
                    check_entity_player_same_room
                ;
                    % No path found; keep current location
                    true
                )
            )
        )
    ),
    !.

% Find a path to the player (simple breadth-first search)
find_path_to_player(From, To, NextRoom) :-
    find_path_to_player_bfs([From], To, [], Path),
    (Path = [From, NextRoom|_] -> true; false).

find_path_to_player_bfs([Current|_], Current, Path, [Current|Path]) :-
    !.
find_path_to_player_bfs([Current|Rest], Target, Path, Result) :-
    findall(Next, (connect(Current, _, Next), \+ member(Next, [Current|Rest])), NextRooms),
    append(Rest, NextRooms, NewQueue),
    find_path_to_player_bfs(NewQueue, Target, [Current|Path], Result).
find_path_to_player_bfs([], _, _, _) :-
    fail.

