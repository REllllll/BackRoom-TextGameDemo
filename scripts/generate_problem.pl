% ============================================================================
% generate_problem.pl
% ============================================================================
% Generate a PDDL problem file from the current Prolog game state.
% Reads the current game state (player location, entity location, etc.) and
% writes the corresponding PDDL problem.
% ============================================================================

:- use_module('../prolog/game_state').
:- use_module('../prolog/knowledge_base').

% ----------------------------------------------------------------------------
% Main entry: generate a PDDL problem file
% ----------------------------------------------------------------------------
% generate_pddl_problem(+OutputFile)
% Generate a PDDL problem from the current game state and write it to OutputFile
% ----------------------------------------------------------------------------

generate_pddl_problem(OutputFile) :-
    open(OutputFile, write, Stream),
    
    % Write header
    write(Stream, '(define (problem backrooms_current)'), nl(Stream),
    write(Stream, '  (:domain adversary)'), nl(Stream),
    nl(Stream),
    
    % Write object declarations
    write_objects(Stream),
    nl(Stream),
    
    % Write initial state
    write(Stream, '  (:init'), nl(Stream),
    write_initial_state(Stream),
    write(Stream, '  )'), nl(Stream),
    nl(Stream),
    
    % Write goal state
    write_goal(Stream),
    nl(Stream),
    
    write(Stream, ')'), nl(Stream),
    close(Stream),
    write('PDDL problem file generated: '), write(OutputFile), nl.

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
    % Entity location
    (at_entity(EntityLoc) ->
        write(Stream, '    (at howler '), write(Stream, EntityLoc), write(Stream, ')'), nl(Stream)
    ; true),
    
    % Player location
    (at_player(PlayerLoc) ->
        write(Stream, '    (at_player player1 '), write(Stream, PlayerLoc), write(Stream, ')'), nl(Stream)
    ; true),
    
    % Room connections (bidirectional)
    write_connections(Stream),
    
    % Noise location (if present)
    write_noise_locations(Stream).

% ----------------------------------------------------------------------------
% Write room connections
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
% Write noise location (if the tape recorder has been dropped)
% ----------------------------------------------------------------------------

write_noise_locations(Stream) :-
    (active_noise_at(Loc) ->
        write(Stream, '    (noise_at '), write(Stream, Loc), write(Stream, ')'), nl(Stream)
    ; true).

% ----------------------------------------------------------------------------
% Write goal state
% ----------------------------------------------------------------------------

write_goal(Stream) :-
    write(Stream, '  (:goal (or'), nl(Stream),
    write(Stream, '    (trapped player1)'), nl(Stream),
    write(Stream, '    (at howler manila_room)'), nl(Stream),
    write(Stream, '  ))'), nl(Stream).

% ----------------------------------------------------------------------------
% Usage example
% ----------------------------------------------------------------------------
% ?- generate_pddl_problem('../pddl/problems/current_problem.pddl').

