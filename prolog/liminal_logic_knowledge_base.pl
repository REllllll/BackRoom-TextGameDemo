% ============================================================================
% liminal_logic_knowledge_base.pl
% ============================================================================
% Static knowledge base: defines static facts about the game world.
% Includes: room definitions, room connections, item properties, etc.
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
% Room definitions
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
% Room connections
% ----------------------------------------------------------------------------

% From start_point to yellow_hallway
connect(start_point, east, yellow_hallway).

% Connections from yellow_hallway
connect(yellow_hallway, west, start_point).
connect(yellow_hallway, north, dark_corridor).
connect(yellow_hallway, east, the_hub).
connect(yellow_hallway, south, supply_closet).

% Connections from dark_corridor
connect(dark_corridor, south, yellow_hallway).
connect(dark_corridor, east, dead_end).

% Dead end
connect(dead_end, west, dark_corridor).

% Central hub
connect(the_hub, west, yellow_hallway).
connect(the_hub, east, manila_room).
connect(the_hub, south, electrical_room).

% Electrical room
connect(electrical_room, north, the_hub).
connect(electrical_room, west, supply_closet).

% Manila room (exit)
connect(manila_room, west, the_hub).

% Supply closet
connect(supply_closet, north, yellow_hallway).
connect(supply_closet, east, electrical_room).

% ----------------------------------------------------------------------------
% Room properties
% ----------------------------------------------------------------------------

% Dark rooms (require flashlight)
is_dark(dark_corridor).
is_dark(dead_end).

% Exit room
is_exit(manila_room).

% Rooms that require a key
requires_key(manila_room).

% Dangerous area (the entity patrols frequently)
% is_dangerous(the_hub).

% ----------------------------------------------------------------------------
% Item definitions
% ----------------------------------------------------------------------------

item(almond_water).
item(flashlight).
item(tape_recorder).
item(key).

% ----------------------------------------------------------------------------
% Item properties
% ----------------------------------------------------------------------------

% Almond water: restores sanity
item_property(almond_water, restores_sanity(20)).

% Flashlight: allows entering dark rooms
item_property(flashlight, enables_dark_rooms).

% Tape recorder: can be used as bait
item_property(tape_recorder, creates_noise).

% Key: required to win
item_property(key, required_for_exit).

% ----------------------------------------------------------------------------
% Initial item locations
% ----------------------------------------------------------------------------

% TODO: define initial item locations
% initial_location(almond_water, supply_closet).
% initial_location(flashlight, electrical_room).
% initial_location(tape_recorder, yellow_hallway).

