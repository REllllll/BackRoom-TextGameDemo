% ============================================================================
% liminal_logic_game_logic.pl
% ============================================================================
% Game logic: implements player commands and rule reasoning.
% Includes commands such as: move, take, drop, use, etc.
% ============================================================================

:- module(game_logic, [
    move/1,
    take/1,
    drop/1,
    use/1,
    look/0,
    inventory/0,
    inv/0,
    can_move/2,
    can_enter_room/1
]).

:- use_module(liminal_logic_game_state).
:- use_module(liminal_logic_knowledge_base).
:- use_module(liminal_logic_win_conditions).

% ----------------------------------------------------------------------------
% Move Command
% ----------------------------------------------------------------------------

move(Direction) :-
    at_player(CurrentRoom),
    connect(CurrentRoom, Direction, NextRoom),
    can_enter_room(NextRoom),
    at_entity(EntityLoc),
    % If the player moves into a room adjacent to the Howler, it will howl.
    (connect(EntityLoc, _, NextRoom) ->
        write('You move to '), write(NextRoom), write('.'), nl,
        write('*** A LOUD HOWL ECHOES FROM THE ADJACENT ROOM! ***'), nl,
        write('The Howler has heard you!'), nl,
        add_sanity(-5)  % Heard a howl; sanity decreases
    ;
        write('You move to '), write(NextRoom), write('.'), nl
    ),
    % Check whether the player enters dark_corridor
    (NextRoom = dark_corridor ->
        set_player_entered_dark_corridor
    ;
        true
    ),
    % Check whether the player leaves dark_corridor (start the Howler chase on first exit)
    (CurrentRoom = dark_corridor, player_entered_dark_corridor, \+ howler_chasing ->
        set_howler_chasing,
        write('*** The Howler begins its pursuit! ***'), nl
    ;
        true
    ),
    set_player_location(NextRoom),
    add_sanity(-1),  % Moving consumes sanity
    check_entity_proximity,
    !.
move(_) :-
    write('You cannot move in that direction, or the room is inaccessible.'), nl.

% ----------------------------------------------------------------------------
% Room Entry Check
% ----------------------------------------------------------------------------

can_enter_room(Room) :-
    % First, check whether a key is required
    (requires_key(Room), \+ holding(key) ->
        write('The door is locked. You need a key to enter.'), nl,
        add_sanity(-5),  % Failed attempt to enter a locked room decreases sanity
        fail  % Explicitly fail; do not allow entry
    ;
        true  % No key needed, or key is held; continue checking
    ),
    % Then, check whether the room is dark
    (is_dark(Room), \+ holding(flashlight) ->
        write('It is too dark to enter. You need a flashlight.'), nl,
        add_sanity(-5),  % Failed attempt to enter a dark room decreases sanity
        fail  % Explicitly fail; do not allow entry
    ;
        true  % Not dark, or flashlight is held; allow entry
    ).

% ----------------------------------------------------------------------------
% Move Feasibility Check
% ----------------------------------------------------------------------------

can_move(From, To) :-
    connect(From, _, To),
    can_enter_room(To).

% ----------------------------------------------------------------------------
% Take Item
% ----------------------------------------------------------------------------

take(Item) :-
    at_player(Location),
    item_location(Item, Location),
    count_holding(Count),
    Count < 2,
    take_item(Item),
    % If the tape recorder is picked up, the active noise (bait) should end immediately
    (Item = tape_recorder -> clear_active_noise ; true),
    write('You pick up the '), write(Item), write('.'), nl,
    !.
take(_Item) :-
    count_holding(Count),
    Count >= 2,
    write('Your hands are full. You can only carry 2 items. Drop something first.'), nl,
    !.
take(Item) :-
    write('The '), write(Item), write(' is not here.'), nl.

% ----------------------------------------------------------------------------
% Drop Item
% ----------------------------------------------------------------------------

drop(Item) :-
    holding(Item),
    drop_item(Item),
    write('You drop the '), write(Item), write('.'), nl,
    % If the tape recorder is dropped, trigger a noise event
    (Item = tape_recorder -> trigger_noise_event; true),
    !.
drop(_) :-
    write('You are not holding that item.'), nl.

% ----------------------------------------------------------------------------
% Use Item
% ----------------------------------------------------------------------------

use(almond_water) :-
    holding(almond_water),
    item_property(almond_water, restores_sanity(Amount)),
    add_sanity(Amount),
    retract(holding(almond_water)),
    write('You drink the almond water. Your sanity increases.'), nl,
    !.
use(flashlight) :-
    holding(flashlight),
    write('The flashlight is already on.'), nl,
    !.
use(Item) :-
    holding(Item),
    write('You cannot use the '), write(Item), write(' in that way.'), nl,
    !.
use(_) :-
    write('You are not holding that item.'), nl.

% ----------------------------------------------------------------------------
% Look Around
% ----------------------------------------------------------------------------

look :-
    at_player(Location),
    write('You are in '), write(Location), write('.'), nl,
    write('Exits: '),
    findall(Dir-Room, connect(Location, Dir, Room), Exits),
    write_exits(Exits),
    nl,
    write('Items here: '),
    findall(Item, item_location(Item, Location), Items),
    (Items = [] -> write('none'); write_items(Items)),
    nl,
    sanity(S),
    write('Sanity: '), write(S), nl.

% ----------------------------------------------------------------------------
% Inventory
% ----------------------------------------------------------------------------

inventory :-
    findall(Item, holding(Item), Items),
    count_holding(Count),
    write('Inventory ('), write(Count), write('/2): '),
    (Items = [] -> write('empty'); write_items(Items)),
    nl.

% Alias for inventory (convenience)
inv :- inventory.

write_exits([]).
write_exits([Dir-Room|Rest]) :-
    write(Dir), write(' -> '), write(Room),
    (Rest = [] -> true; write(', ')),
    write_exits(Rest).

write_items([]).
write_items([Item|Rest]) :-
    write(Item),
    (Rest = [] -> true; write(', ')),
    write_items(Rest).

% ----------------------------------------------------------------------------
% Entity Proximity Check
% ----------------------------------------------------------------------------

check_entity_proximity :-
    at_player(PlayerLoc),
    at_entity(EntityLoc),
    (PlayerLoc = EntityLoc ->
        write('WARNING: The Howler is in the same room!'), nl,
        add_sanity(-10),
        check_lose  % Check and trigger game over
    ;
        true
    ).

% ----------------------------------------------------------------------------
% Trigger Noise Event
% ----------------------------------------------------------------------------

trigger_noise_event :-
    at_player(Location),
    % Set the noise location (bait target)
    set_active_noise_at(Location),
    % Mark that an entity update is needed next turn (even if the player did not move)
    request_entity_update,
    % On the same turn as drop, prevent the entity from acting immediately
    % (it will start moving toward the new goal on the next turn).
    suppress_entity_update_once,
    % Clear cached plan when the target changes to avoid reusing an old path
    clear_cached_entity_plan,
    write('The tape recorder makes a loud noise!'), nl,
    write('The Howler might be attracted to this location...'), nl,
    true.

