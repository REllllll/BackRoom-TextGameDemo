% ============================================================================
% game_state.pl
% ============================================================================
% 动态状态管理：管理游戏运行时的动态事实
% 包括：玩家位置、实体位置、理智值、持有物品等
% ============================================================================

:- module(game_state, [
    init_game_state/0,
    at_player/1,
    at_entity/1,
    sanity/1,
    holding/1,
    item_location/2,
    set_player_location/1,
    set_entity_location/1,
    set_sanity/1,
    add_sanity/1,
    take_item/1,
    drop_item/1,
    update_item_location/2
]).

% ----------------------------------------------------------------------------
% 动态事实声明 (Dynamic Facts Declaration)
% ----------------------------------------------------------------------------

:- dynamic at_player/1.
:- dynamic at_entity/1.
:- dynamic sanity/1.
:- dynamic holding/1.
:- dynamic item_location/2.

% ----------------------------------------------------------------------------
% 初始化游戏状态 (Initialize Game State)
% ----------------------------------------------------------------------------

init_game_state :-
    % 清除所有动态事实
    retractall(at_player(_)),
    retractall(at_entity(_)),
    retractall(sanity(_)),
    retractall(holding(_)),
    retractall(item_location(_, _)),
    
    % 设置初始状态
    asserta(at_player(start_point)),
    asserta(at_entity(electrical_room)),
    asserta(sanity(100)),
    
    % TODO: 设置物品初始位置
    % asserta(item_location(almond_water, supply_closet)),
    % asserta(item_location(flashlight, electrical_room)),
    % asserta(item_location(tape_recorder, yellow_hallway)),
    
    write('Game state initialized.'), nl.

% ----------------------------------------------------------------------------
% 玩家位置操作 (Player Location Operations)
% ----------------------------------------------------------------------------

set_player_location(Location) :-
    retractall(at_player(_)),
    asserta(at_player(Location)).

% ----------------------------------------------------------------------------
% 实体位置操作 (Entity Location Operations)
% ----------------------------------------------------------------------------

set_entity_location(Location) :-
    retractall(at_entity(_)),
    asserta(at_entity(Location)).

% ----------------------------------------------------------------------------
% 理智值操作 (Sanity Operations)
% ----------------------------------------------------------------------------

set_sanity(Value) :-
    retractall(sanity(_)),
    asserta(sanity(Value)).

add_sanity(Delta) :-
    sanity(Current),
    NewValue is Current + Delta,
    set_sanity(NewValue).

% ----------------------------------------------------------------------------
% 物品操作 (Item Operations)
% ----------------------------------------------------------------------------

take_item(Item) :-
    retractall(holding(_)),
    asserta(holding(Item)),
    retractall(item_location(Item, _)).

drop_item(Item) :-
    holding(Item),
    at_player(Location),
    retractall(holding(_)),
    asserta(item_location(Item, Location)).

update_item_location(Item, Location) :-
    retractall(item_location(Item, _)),
    asserta(item_location(Item, Location)).

