% ============================================================================
% game_state.pl
% ============================================================================
% Dynamic state management: runtime facts for the game.
% Includes: player location, entity location, sanity, held items, etc.
% ============================================================================

:- module(game_state, [
    init_game_state/0,
    at_player/1,
    at_entity/1,
    sanity/1,
    holding/1,
    count_holding/1,
    item_location/2,
    active_noise_at/1,
    set_active_noise_at/1,
    clear_active_noise/0,
    entity_update_requested/0,
    request_entity_update/0,
    clear_entity_update_request/0,
    suppress_entity_update_once/0,
    consume_suppress_entity_update_once/0,
    set_player_location/1,
    set_entity_location/1,
    set_sanity/1,
    add_sanity/1,
    take_item/1,
    drop_item/1,
    update_item_location/2,
    player_previous_location/1,
    set_player_previous_location/1,
    game_over_status/1,
    set_game_over_status/1,
    is_game_over/0,
    player_entered_dark_corridor/0,
    set_player_entered_dark_corridor/0,
    howler_chasing/0,
    set_howler_chasing/0,
    cached_entity_plan/1,
    set_cached_entity_plan/1,
    clear_cached_entity_plan/0
]).

% ----------------------------------------------------------------------------
% Dynamic Facts Declaration
% ----------------------------------------------------------------------------

:- dynamic at_player/1.
:- dynamic at_entity/1.
:- dynamic sanity/1.
:- dynamic holding/1.
:- dynamic item_location/2.
:- dynamic player_previous_location/1.
:- dynamic game_over_status/1.
:- dynamic player_entered_dark_corridor/0.
:- dynamic howler_chasing/0.
:- dynamic cached_entity_plan/1.
:- dynamic active_noise_at/1.
:- dynamic entity_update_requested/0.
:- dynamic suppress_entity_update_once_flag/0.

% ----------------------------------------------------------------------------
% Initialize Game State
% ----------------------------------------------------------------------------

init_game_state :-
    % Clear all dynamic facts
    retractall(at_player(_)),
    retractall(at_entity(_)),
    retractall(sanity(_)),
    retractall(holding(_)),
    retractall(item_location(_, _)),
    retractall(player_previous_location(_)),
    retractall(game_over_status(_)),
    retractall(player_entered_dark_corridor),
    retractall(howler_chasing),
    retractall(cached_entity_plan(_)),
    retractall(active_noise_at(_)),
    retractall(entity_update_requested),
    retractall(suppress_entity_update_once_flag),
    
    % Set initial state
    asserta(at_player(start_point)),
    asserta(at_entity(dead_end)),
    asserta(sanity(100)),
    asserta(player_previous_location(start_point)),  % Initially, previous location is also the start point
    
    % Set initial item locations
    asserta(item_location(key, dark_corridor)),
    asserta(item_location(almond_water, yellow_hallway)),
    asserta(item_location(flashlight, electrical_room)),
    asserta(item_location(tape_recorder, supply_closet)),
    
    write('Game state initialized.'), nl.

% ----------------------------------------------------------------------------
% Noise state (tape recorder bait)
% ----------------------------------------------------------------------------

% Set the current active noise location (keep only one)
set_active_noise_at(Location) :-
    retractall(active_noise_at(_)),
    asserta(active_noise_at(Location)).

% Clear active noise
clear_active_noise :-
    retractall(active_noise_at(_)).

% ----------------------------------------------------------------------------
% Entity update request (so non-move actions like drop can also trigger updates)
% ----------------------------------------------------------------------------

request_entity_update :-
    (entity_update_requested ->
        true
    ;
        asserta(entity_update_requested)
    ).

clear_entity_update_request :-
    retractall(entity_update_requested).

% ----------------------------------------------------------------------------
% Skip entity update once for this turn (used by dropping the tape recorder:
% the entity acts starting next turn)
% ----------------------------------------------------------------------------
suppress_entity_update_once :-
    (suppress_entity_update_once_flag ->
        true
    ;
        asserta(suppress_entity_update_once_flag)
    ).

% Consume (check and clear) the one-time skip flag:
% if present, succeed and clear; if absent, fail.
consume_suppress_entity_update_once :-
    retract(suppress_entity_update_once_flag),
    !.

% ----------------------------------------------------------------------------
% Player Location Operations
% ----------------------------------------------------------------------------

set_player_location(Location) :-
    % Save current location as previous location (if present)
    (at_player(CurrentLoc) ->
        (CurrentLoc \= Location ->
            % Location actually changed; save previous location
            retractall(player_previous_location(_)),
            asserta(player_previous_location(CurrentLoc))
        ;
            % Location did not change; no need to update previous location
            true
        )
    ;
        % If there is no current location (during init), use it as previous location
        (\+ player_previous_location(_) ->
            asserta(player_previous_location(Location))
        ;
            true
        )
    ),
    retractall(at_player(_)),
    asserta(at_player(Location)).

% Set player's previous location (for tests or special scenarios)
set_player_previous_location(Location) :-
    retractall(player_previous_location(_)),
    asserta(player_previous_location(Location)).

% ----------------------------------------------------------------------------
% Entity Location Operations
% ----------------------------------------------------------------------------

set_entity_location(Location) :-
    retractall(at_entity(_)),
    asserta(at_entity(Location)).

% ----------------------------------------------------------------------------
% Sanity Operations
% ----------------------------------------------------------------------------

set_sanity(Value) :-
    retractall(sanity(_)),
    asserta(sanity(Value)).

add_sanity(Delta) :-
    sanity(Current),
    NewValue is Current + Delta,
    set_sanity(NewValue).

% ----------------------------------------------------------------------------
% Item Operations
% ----------------------------------------------------------------------------

% Count held items
count_holding(Count) :-
    findall(Item, holding(Item), Items),
    length(Items, Count).

% Take item (max 2)
take_item(Item) :-
    count_holding(Count),
    Count < 2,
    asserta(holding(Item)),
    retractall(item_location(Item, _)).

% Drop item (remove only the specified item)
drop_item(Item) :-
    holding(Item),
    at_player(Location),
    retract(holding(Item)),
    asserta(item_location(Item, Location)).

update_item_location(Item, Location) :-
    retractall(item_location(Item, _)),
    asserta(item_location(Item, Location)).

% ----------------------------------------------------------------------------
% Game Over Status Operations
% ----------------------------------------------------------------------------

set_game_over_status(Status) :-
    retractall(game_over_status(_)),
    asserta(game_over_status(Status)).

is_game_over :-
    game_over_status(_).

% ----------------------------------------------------------------------------
% Dark Corridor and Howler chase state
% ----------------------------------------------------------------------------

% Check whether the player has entered dark_corridor
player_entered_dark_corridor :-
    player_entered_dark_corridor.

% Mark that the player has entered dark_corridor
set_player_entered_dark_corridor :-
    (player_entered_dark_corridor ->
        true  % Already set; no need to set again
    ;
        asserta(player_entered_dark_corridor)
    ).

% Check whether the Howler has started chasing
howler_chasing :-
    howler_chasing.

% Mark that the Howler has started chasing
set_howler_chasing :-
    (howler_chasing ->
        true  % Already chasing; no need to set again
    ;
        asserta(howler_chasing)
    ).

% ----------------------------------------------------------------------------
% Plan cache operations
% ----------------------------------------------------------------------------

% Get cached plan
cached_entity_plan(Plan) :-
    cached_entity_plan(Plan).

% Set cached plan
set_cached_entity_plan(Plan) :-
    retractall(cached_entity_plan(_)),
    asserta(cached_entity_plan(Plan)).

% Clear cached plan
clear_cached_entity_plan :-
    retractall(cached_entity_plan(_)).

