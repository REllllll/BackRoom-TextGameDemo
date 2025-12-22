% ============================================================================
% http_server.pl
% ============================================================================
% HTTP server module: provides REST API endpoints.
% The frontend is served by nginx; this server only handles the API.
% ============================================================================

:- module(game_http_server, [
    start_server/0,
    start_server/1
]).

% Note: do not use the http_server module here because it conflicts with our
% module name.
% :- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(liminal_logic_knowledge_base).
:- use_module(liminal_logic_game_state).
:- use_module(liminal_logic_game_logic).
:- use_module(liminal_logic_win_conditions).
:- use_module(liminal_logic_pddl_interface).

% ----------------------------------------------------------------------------
% Server configuration
% ----------------------------------------------------------------------------

% Default port
default_port(8080).

% ----------------------------------------------------------------------------
% Start server
% ----------------------------------------------------------------------------

start_server :-
    default_port(Port),
    start_server(Port).

start_server(Port) :-
    % Register API routes
    http_handler(root(api/status), api_status, [method(get)]),
    http_handler(root(api/init), api_init, [method(post)]),
    http_handler(root(api/command), api_command, [method(post)]),
    http_handler(root(api/map), api_map, [method(get)]),
    http_handler(root(api/rooms), api_room_info, [method(get), prefix]),
    
    % Start server (bind to 0.0.0.0 to allow external access)
    use_module(library(http/http_server)),
    % Bind to all interfaces using Host:Port
    Address = '0.0.0.0':Port,
    http_server([port(Address)]),
    format('API server started on port ~w~n', [Port]),
    format('Server listening on 0.0.0.0:~w~n', [Port]),
    format('API endpoints available at http://0.0.0.0:~w/api/*~n', [Port]),
    format('Frontend should be served by nginx~n', []),
    % Keep server running
    thread_get_message(_).

% ----------------------------------------------------------------------------
% API: Get game status
% GET /api/status
% ----------------------------------------------------------------------------

api_status(_Request) :-
    % Get player location
    (at_player(PlayerLoc) -> PlayerLocation = PlayerLoc; PlayerLocation = null),
    
    % Get entity location
    (at_entity(EntityLoc) -> EntityLocation = EntityLoc; EntityLocation = null),
    
    % Get sanity
    (sanity(Sanity) -> SanityValue = Sanity; SanityValue = 100),
    
    % Get held items (inventory)
    % Note: do NOT bind Item via holding(Item) here.
    % It would interfere with the later findall(..., item_location(...)) due to
    % variable reuse and can incorrectly report "no items in room".
    % Always return a list; the frontend already handles list/singleton cases.
    findall(HeldItem, holding(HeldItem), HoldingItems),
    
    % Get items in the current room
    (PlayerLocation \= null ->
        findall(RoomItem, item_location(RoomItem, PlayerLocation), ItemsHere)
    ;
        ItemsHere = []
    ),
    
    % Get exits from the current room
    (PlayerLocation \= null ->
        findall(json{direction: Dir, to: Room}, connect(PlayerLocation, Dir, Room), Exits)
    ;
        Exits = []
    ),
    
    % Determine game status
    (check_win_condition -> GameStatus = win
    ; check_lose_condition -> GameStatus = lose
    ; GameStatus = playing),
    
    % Build response
    Reply = json{
        player_location: PlayerLocation,
        entity_location: EntityLocation,
        sanity: SanityValue,
        holding: HoldingItems,
        items_here: ItemsHere,
        exits: Exits,
        game_status: GameStatus
    },
    
    cors_reply_json(Reply).

% ----------------------------------------------------------------------------
% API: Initialize game
% POST /api/init
% ----------------------------------------------------------------------------

api_init(_Request) :-
    % Capture init_game_state output to avoid polluting the HTTP response
    with_output_to(string(_), init_game_state),
    Reply = json{
        success: true,
        message: 'Game initialized'
    },
    cors_reply_json(Reply).

% ----------------------------------------------------------------------------
% API: Execute game command
% POST /api/command
% Body: {"command": "move(east)"} or {"command": "look"}
% ----------------------------------------------------------------------------

api_command(Request) :-
    http_read_json_dict(Request, CommandDict),
    get_dict(command, CommandDict, CommandStr),
    
    % Parse command string
    catch(
        term_string(Command, CommandStr),
        _,
        (Command = CommandStr)
    ),
    
    % Capture output (command execution + entity update)
    with_output_to(string(Output), (
        process_command_with_output(Command, Success),
        % Check if the game is already over (before entity update)
        (is_game_over ->
            % Game is over; do not update entity
            true
        ;
            % Update entity position
            % If the Howler is chasing, any player action triggers Howler movement
            (howler_chasing ->
                catch(
                    update_entity_from_pddl,
                    Error,
                    (format('Error updating entity: ~w~n', [Error]))
                )
            ;
                % If the Howler is not yet chasing, use the original logic.
                % If the command succeeds, or if it's a move attempt that fails
                % (the player tried to move but was blocked), the Howler should still act.
                (Success = true -> 
                    catch(
                        update_entity_from_pddl,
                        Error,
                        (format('Error updating entity: ~w~n', [Error]))
                    )
                ; 
                    % Command failed, but if it's a move attempt, the Howler should still act
                    (Command = move(Direction) ->
                        catch(
                            update_entity_on_failed_move(Direction),
                            Error,
                            (format('Error updating entity: ~w~n', [Error]))
                        )
                    ;
                        true
                    )
                )
            )
        )
    )),
    
    % Determine game status
    (check_win_condition -> GameStatus = win
    ; check_lose_condition -> GameStatus = lose
    ; GameStatus = playing),
    
    % Get updated state
    (at_player(PlayerLoc) -> PlayerLocation = PlayerLoc; PlayerLocation = null),
    (at_entity(EntityLoc) -> EntityLocation = EntityLoc; EntityLocation = null),
    (sanity(S) -> SanityValue = S; SanityValue = 100),
    (findall(Item, holding(Item), HoldingItems) -> 
        (HoldingItems = [] -> HoldingItemList = []; HoldingItemList = HoldingItems)
    ; 
        HoldingItemList = []
    ),
    
    % Get items in the current room
    (PlayerLocation \= null ->
        findall(Item, item_location(Item, PlayerLocation), ItemsHere)
    ;
        ItemsHere = []
    ),
    
    % Get exits from the current room
    (PlayerLocation \= null ->
        findall(json{direction: Dir, to: Room}, connect(PlayerLocation, Dir, Room), Exits)
    ;
        Exits = []
    ),
    
    Reply = json{
        success: Success,
        output: Output,
        game_status: GameStatus,
        player_location: PlayerLocation,
        entity_location: EntityLocation,
        sanity: SanityValue,
        holding: HoldingItemList,
        items_here: ItemsHere,
        exits: Exits
    },
    
    cors_reply_json(Reply).

% ----------------------------------------------------------------------------
% API: Get map data
% GET /api/map
% ----------------------------------------------------------------------------

api_map(_Request) :-
    % Get all rooms
    findall(Room, room(Room), Rooms),
    
    % Get all connections
    findall(
        json{from: From, direction: Dir, to: To},
        connect(From, Dir, To),
        Connections
    ),
    
    % Get room properties
    findall(Room, is_dark(Room), DarkRooms),
    findall(Room, is_exit(Room), ExitRooms),
    
    Reply = json{
        rooms: Rooms,
        connections: Connections,
        dark_rooms: DarkRooms,
        exit_rooms: ExitRooms
    },
    
    cors_reply_json(Reply).

% ----------------------------------------------------------------------------
% API: Get room info
% GET /api/rooms/:room
% ----------------------------------------------------------------------------

api_room_info(Request) :-
    memberchk(path(Path), Request),
    (atomic_list_concat([_, 'rooms', RoomStr], '/', Path) ->
        atom_string(Room, RoomStr)
    ;
        Room = ''
    ),
    
    (room(Room) ->
        % Get room exits
        findall(json{direction: Dir, to: RoomTo}, connect(Room, Dir, RoomTo), Exits),
        
        % Get room items
        findall(Item, item_location(Item, Room), Items),
        
        % Get room properties
        (is_dark(Room) -> IsDark = true; IsDark = false),
        (is_exit(Room) -> IsExit = true; IsExit = false),
        
        Reply = json{
            room: Room,
            exits: Exits,
            items: Items,
            is_dark: IsDark,
            is_exit: IsExit
        }
    ;
        Reply = json{
            error: 'Room not found'
        }
    ),
    
    cors_reply_json(Reply).

% ----------------------------------------------------------------------------
% Helper: process command and capture output
% ----------------------------------------------------------------------------

process_command_with_output(Command, Success) :-
    catch(
        (
            process_command(Command),
            Success = true
        ),
        Error,
        (
            format('Error: ~w~n', [Error]),
            Success = false
        )
    ).

% ----------------------------------------------------------------------------
% Command processing (copied from liminal_logic_game.pl)
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
process_command(look) :-
    look,
    !.
process_command(_) :-
    write('Unknown command. Try: move(direction), take(item), drop(item), use(item), look.'), nl.

% ----------------------------------------------------------------------------
% Check game status (no output messages)
% ----------------------------------------------------------------------------

check_win_condition :-
    % First check whether the game has already ended (via status flag)
    game_over_status(win),
    !.
check_win_condition :-
    at_player(manila_room),
    is_exit(manila_room),
    holding(key).  % Must be holding the key to win

check_lose_condition :-
    % First check whether the game has already ended (via status flag)
    game_over_status(Status),
    (Status = lose_sanity; Status = lose_caught),
    !.
check_lose_condition :-
    sanity(S),
    S =< 0,
    !.
check_lose_condition :-
    at_player(PlayerLoc),
    at_entity(EntityLoc),
    PlayerLoc = EntityLoc.

% ----------------------------------------------------------------------------
% CORS support
% ----------------------------------------------------------------------------

cors_reply_json(JSON) :-
    cors_enable,
    reply_json(JSON).

