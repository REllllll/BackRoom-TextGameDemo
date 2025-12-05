% ============================================================================
% knowledge_base.pl
% ============================================================================
% 静态知识库：定义游戏世界的静态事实
% 包括：房间定义、房间连接、物品属性等
% ============================================================================

:- module(knowledge_base, [
    room/1,
    connect/3,
    is_dark/1,
    is_exit/1,
    requires_key/1,
    item/1,
    item_property/2
]).

% ----------------------------------------------------------------------------
% 房间定义 (Room Definitions)
% ----------------------------------------------------------------------------

room(start_point).
room(yellow_hallway).
room(dark_corridor).
room(electrical_room).
room(the_hub).
room(manila_room).
room(supply_closet).
room(dead_end).

% ----------------------------------------------------------------------------
% 房间连接 (Room Connections)
% ----------------------------------------------------------------------------

% 从起点到黄色走廊
connect(start_point, east, yellow_hallway).

% 黄色走廊的连接
connect(yellow_hallway, west, start_point).
connect(yellow_hallway, north, dark_corridor).
connect(yellow_hallway, east, the_hub).
connect(yellow_hallway, south, supply_closet).

% 黑暗走廊的连接
connect(dark_corridor, south, yellow_hallway).
connect(dark_corridor, east, dead_end).

% 死胡同
connect(dead_end, west, dark_corridor).

% 中心枢纽
connect(the_hub, west, yellow_hallway).
connect(the_hub, east, manila_room).
connect(the_hub, south, electrical_room).

% 电气室
connect(electrical_room, north, the_hub).

% 马尼拉房间（出口）
connect(manila_room, west, the_hub).

% 储藏室
connect(supply_closet, north, yellow_hallway).

% ----------------------------------------------------------------------------
% 房间属性 (Room Properties)
% ----------------------------------------------------------------------------

% 黑暗房间（需要手电筒）
is_dark(dark_corridor).
is_dark(dead_end).

% 出口房间
is_exit(manila_room).

% 需要钥匙的房间
requires_key(manila_room).

% 危险区域（实体经常巡逻）
% is_dangerous(the_hub).

% ----------------------------------------------------------------------------
% 物品定义 (Item Definitions)
% ----------------------------------------------------------------------------

item(almond_water).
item(flashlight).
item(tape_recorder).
item(key).

% ----------------------------------------------------------------------------
% 物品属性 (Item Properties)
% ----------------------------------------------------------------------------

% 杏仁水：恢复理智值
item_property(almond_water, restores_sanity(20)).

% 手电筒：允许进入黑暗房间
item_property(flashlight, enables_dark_rooms).

% 录音机：可以作为诱饵
item_property(tape_recorder, creates_noise).

% 钥匙：通关所需
item_property(key, required_for_exit).

% ----------------------------------------------------------------------------
% 物品初始位置 (Initial Item Locations)
% ----------------------------------------------------------------------------

% TODO: 定义物品的初始位置
% initial_location(almond_water, supply_closet).
% initial_location(flashlight, electrical_room).
% initial_location(tape_recorder, yellow_hallway).

