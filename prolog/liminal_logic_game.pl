% ============================================================================
% liminal_logic_game.pl
% ============================================================================
% Main entry point: wires modules together, runs the game loop, and integrates
% with the PDDL planner.
% ============================================================================

:- use_module(liminal_logic_knowledge_base).
:- use_module(liminal_logic_game_state).
:- use_module(liminal_logic_game_logic).
:- use_module(liminal_logic_win_conditions).
:- use_module(liminal_logic_pddl_interface).

% ----------------------------------------------------------------------------
% Main Game Loop
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
    write('Available commands:'), nl,
    write('--- Available Commands ---'), nl,
    write('move(Direction)  - Move in a direction (north, south, east, west)'), nl,
    write('take(Item)       - Pick up an item'), nl,
    write('drop(Item)       - Drop an item you are holding'), nl,
    write('use(Item)        - Use an item'), nl,
    write('inventory        - Show items you are carrying (alias: inv)'), nl,
    write('look             - Look around and see your current location'), nl,
    write('quit             - Exit the game'), nl,
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
    update_entity,  % Update entity position (based on PDDL planning)
    nl,
    game_loop.

% ----------------------------------------------------------------------------
% Command Processing
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
process_command(inventory) :-
    inventory,
    !.
process_command(inv) :-
    inventory,
    !.
process_command(look) :-
    look,
    !.
process_command(quit) :-
    write('Thanks for playing!'), nl,
    halt,
    !.
process_command(_) :-
    write('Unknown command. Try: move(direction), take(item), drop(item), use(item), inventory, look, quit.'), nl.

% ----------------------------------------------------------------------------
% Entity Update
% ----------------------------------------------------------------------------

update_entity :-
    % Update entity position via the PDDL interface module
    update_entity_from_pddl.

% ----------------------------------------------------------------------------
% Helper Functions
% ----------------------------------------------------------------------------

% PDDL-related logic has been moved to the liminal_logic_pddl_interface.pl module

