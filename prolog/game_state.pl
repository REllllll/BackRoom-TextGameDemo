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
    count_holding/1,
    item_location/2,
    set_player_location/1,
    set_entity_location/1,
    set_sanity/1,
    add_sanity/1,
    take_item/1,
    drop_item/1,
    update_item_location/2,
    player_previous_location/1,
    set_player_previous_location/1
]).

% ----------------------------------------------------------------------------
% 动态事实声明 (Dynamic Facts Declaration)
% ----------------------------------------------------------------------------

:- dynamic at_player/1.
:- dynamic at_entity/1.
:- dynamic sanity/1.
:- dynamic holding/1.
:- dynamic item_location/2.
:- dynamic player_previous_location/1.

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
    retractall(player_previous_location(_)),
    
    % 设置初始状态
    asserta(at_player(start_point)),
    asserta(at_entity(dead_end)),
    asserta(sanity(100)),
    asserta(player_previous_location(start_point)),  % 初始时上一位置也是起始点
    
    % 设置物品初始位置
    asserta(item_location(key, dark_corridor)),
    asserta(item_location(almond_water, yellow_hallway)),
    asserta(item_location(flashlight, electrical_room)),
    asserta(item_location(tape_recorder, supply_closet)),
    
    write('Game state initialized.'), nl.

% ----------------------------------------------------------------------------
% 玩家位置操作 (Player Location Operations)
% ----------------------------------------------------------------------------

set_player_location(Location) :-
    % 保存当前位置为上一位置（如果存在）
    (at_player(CurrentLoc) ->
        (CurrentLoc \= Location ->
            % 位置确实改变了，保存上一位置
            retractall(player_previous_location(_)),
            asserta(player_previous_location(CurrentLoc))
        ;
            % 位置没有改变，不需要更新上一位置
            true
        )
    ;
        % 如果没有当前位置（初始化时），使用当前位置作为上一位置
        (\+ player_previous_location(_) ->
            asserta(player_previous_location(Location))
        ;
            true
        )
    ),
    retractall(at_player(_)),
    asserta(at_player(Location)).

% 设置玩家上一位置（用于测试或特殊场景）
set_player_previous_location(Location) :-
    retractall(player_previous_location(_)),
    asserta(player_previous_location(Location)).

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

% 统计持有的物品数量
count_holding(Count) :-
    findall(Item, holding(Item), Items),
    length(Items, Count).

% 拾取物品（最多2个）
take_item(Item) :-
    count_holding(Count),
    Count < 2,
    asserta(holding(Item)),
    retractall(item_location(Item, _)).

% 丢弃物品（只删除指定的物品）
drop_item(Item) :-
    holding(Item),
    at_player(Location),
    retract(holding(Item)),
    asserta(item_location(Item, Location)).

update_item_location(Item, Location) :-
    retractall(item_location(Item, _)),
    asserta(item_location(Item, Location)).

