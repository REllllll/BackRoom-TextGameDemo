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
    can_move/2,
    can_enter_room/1
]).

:- use_module(game_state).
:- use_module(knowledge_base).

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
    set_player_location(NextRoom),
    add_sanity(-1),  % 移动消耗理智值
    check_entity_proximity,
    !.
move(_) :-
    write('You cannot move in that direction, or the room is inaccessible.'), nl.

% ----------------------------------------------------------------------------
% 进入房间检查 (Room Entry Check)
% ----------------------------------------------------------------------------

% 检查是否需要钥匙（优先检查）
can_enter_room(Room) :-
    requires_key(Room),
    \+ holding(key),
    write('The door is locked. You need a key to enter.'), nl,
    add_sanity(-5),  % 尝试进入需要钥匙的房间失败，理智值下降
    fail.
% 检查是否黑暗（在钥匙检查之后）
can_enter_room(Room) :-
    is_dark(Room),
    \+ holding(flashlight),
    write('It is too dark to enter. You need a flashlight.'), nl,
    add_sanity(-5),  % 尝试进入黑暗房间失败，理智值下降
    fail.
% 如果通过所有检查，允许进入
can_enter_room(_Room).

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
        add_sanity(-10)
    ;
        true
    ).

% ----------------------------------------------------------------------------
% 触发噪音事件 (Trigger Noise Event)
% ----------------------------------------------------------------------------

trigger_noise_event :-
    at_player(_Location),
    write('The tape recorder makes a loud noise!'), nl,
    write('The Howler might be attracted to this location...'), nl,
    % TODO: 更新 PDDL problem，将实体目标改为当前位置
    true.

