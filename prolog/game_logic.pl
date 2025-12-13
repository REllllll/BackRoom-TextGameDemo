% ============================================================================
% game_logic.pl
% ============================================================================
% 游戏逻辑：实现游戏命令和规则推理
% 包括：移动、拾取、丢弃、使用物品等命令
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

:- use_module(game_state).
:- use_module(knowledge_base).
:- use_module(win_conditions).

% ----------------------------------------------------------------------------
% 移动命令 (Move Command)
% ----------------------------------------------------------------------------

move(Direction) :-
    at_player(CurrentRoom),
    connect(CurrentRoom, Direction, NextRoom),
    can_enter_room(NextRoom),
    at_entity(EntityLoc),
    % 检查玩家是否移动到相邻房间（Howler 会吼叫）
    (connect(EntityLoc, _, NextRoom) ->
        write('You move to '), write(NextRoom), write('.'), nl,
        write('*** A LOUD HOWL ECHOES FROM THE ADJACENT ROOM! ***'), nl,
        write('The Howler has heard you!'), nl,
        add_sanity(-5)  % 听到吼叫，理智值下降
    ;
        write('You move to '), write(NextRoom), write('.'), nl
    ),
    % 检查玩家是否进入dark_corridor
    (NextRoom = dark_corridor ->
        set_player_entered_dark_corridor
    ;
        true
    ),
    % 检查玩家是否从dark_corridor离开（第一次离开时启动Howler追逐）
    (CurrentRoom = dark_corridor, player_entered_dark_corridor, \+ howler_chasing ->
        set_howler_chasing,
        write('*** The Howler begins its pursuit! ***'), nl
    ;
        true
    ),
    set_player_location(NextRoom),
    add_sanity(-1),  % 移动消耗理智值
    check_entity_proximity,
    !.
move(_) :-
    write('You cannot move in that direction, or the room is inaccessible.'), nl.

% ----------------------------------------------------------------------------
% 进入房间检查 (Room Entry Check)
% ----------------------------------------------------------------------------

can_enter_room(Room) :-
    % 首先检查是否需要钥匙
    (requires_key(Room), \+ holding(key) ->
        write('The door is locked. You need a key to enter.'), nl,
        add_sanity(-5),  % 尝试进入需要钥匙的房间失败，理智值下降
        fail  % 明确失败，不允许进入
    ;
        true  % 不需要钥匙，或者持有钥匙，继续检查
    ),
    % 然后检查是否黑暗
    (is_dark(Room), \+ holding(flashlight) ->
        write('It is too dark to enter. You need a flashlight.'), nl,
        add_sanity(-5),  % 尝试进入黑暗房间失败，理智值下降
        fail  % 明确失败，不允许进入
    ;
        true  % 不是黑暗的，或者持有手电筒，允许进入
    ).

% ----------------------------------------------------------------------------
% 移动可行性检查 (Move Feasibility Check)
% ----------------------------------------------------------------------------

can_move(From, To) :-
    connect(From, _, To),
    can_enter_room(To).

% ----------------------------------------------------------------------------
% 拾取物品 (Take Item)
% ----------------------------------------------------------------------------

take(Item) :-
    at_player(Location),
    item_location(Item, Location),
    count_holding(Count),
    Count < 2,
    take_item(Item),
    % 如果拾起录音机，噪音（诱饵）应立即失效
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
% 丢弃物品 (Drop Item)
% ----------------------------------------------------------------------------

drop(Item) :-
    holding(Item),
    drop_item(Item),
    write('You drop the '), write(Item), write('.'), nl,
    % 如果丢弃录音机，触发噪音事件
    (Item = tape_recorder -> trigger_noise_event; true),
    !.
drop(_) :-
    write('You are not holding that item.'), nl.

% ----------------------------------------------------------------------------
% 使用物品 (Use Item)
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
% 查看周围 (Look Around)
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
% 查看背包/仓库 (Inventory)
% ----------------------------------------------------------------------------

inventory :-
    findall(Item, holding(Item), Items),
    count_holding(Count),
    write('Inventory ('), write(Count), write('/2): '),
    (Items = [] -> write('empty'); write_items(Items)),
    nl.

% inventory 的别名（方便输入）
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
% 实体接近检查 (Entity Proximity Check)
% ----------------------------------------------------------------------------

check_entity_proximity :-
    at_player(PlayerLoc),
    at_entity(EntityLoc),
    (PlayerLoc = EntityLoc ->
        write('WARNING: The Howler is in the same room!'), nl,
        add_sanity(-10),
        check_lose  % 检查并触发游戏结束
    ;
        true
    ).

% ----------------------------------------------------------------------------
% 触发噪音事件 (Trigger Noise Event)
% ----------------------------------------------------------------------------

trigger_noise_event :-
    at_player(Location),
    % 设置噪音位置（诱饵目标）
    set_active_noise_at(Location),
    % 标记下一回合需要更新实体（即使玩家没有移动）
    request_entity_update,
    % drop 当回合不让实体立刻行动（下一回合才开始按新目标走一步）
    suppress_entity_update_once,
    % 目标改变时清空缓存规划，避免沿用旧路径
    clear_cached_entity_plan,
    write('The tape recorder makes a loud noise!'), nl,
    write('The Howler might be attracted to this location...'), nl,
    true.

