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
% 动态事实声明 (Dynamic Facts Declaration)
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
    retractall(game_over_status(_)),
    retractall(player_entered_dark_corridor),
    retractall(howler_chasing),
    retractall(cached_entity_plan(_)),
    
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

% ----------------------------------------------------------------------------
% 游戏结束状态操作 (Game Over Status Operations)
% ----------------------------------------------------------------------------

set_game_over_status(Status) :-
    retractall(game_over_status(_)),
    asserta(game_over_status(Status)).

is_game_over :-
    game_over_status(_).

% ----------------------------------------------------------------------------
% Dark Corridor 和 Howler 追逐状态操作
% ----------------------------------------------------------------------------

% 检查玩家是否已经进入过dark_corridor
player_entered_dark_corridor :-
    player_entered_dark_corridor.

% 设置玩家已进入dark_corridor
set_player_entered_dark_corridor :-
    (player_entered_dark_corridor ->
        true  % 已经设置过，不需要重复设置
    ;
        asserta(player_entered_dark_corridor)
    ).

% 检查Howler是否已经开始追逐
howler_chasing :-
    howler_chasing.

% 设置Howler开始追逐
set_howler_chasing :-
    (howler_chasing ->
        true  % 已经开始追逐，不需要重复设置
    ;
        asserta(howler_chasing)
    ).

% ----------------------------------------------------------------------------
% 规划路径缓存操作
% ----------------------------------------------------------------------------

% 获取缓存的规划路径
cached_entity_plan(Plan) :-
    cached_entity_plan(Plan).

% 设置缓存的规划路径
set_cached_entity_plan(Plan) :-
    retractall(cached_entity_plan(_)),
    asserta(cached_entity_plan(Plan)).

% 清除缓存的规划路径
clear_cached_entity_plan :-
    retractall(cached_entity_plan(_)).

