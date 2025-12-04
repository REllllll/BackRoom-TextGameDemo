% ============================================================================
% http_server.pl
% ============================================================================
% HTTP Server 模块：提供 REST API 接口供 HTML playground 使用
% ============================================================================

:- module(game_http_server, [
    start_server/0,
    start_server/1
]).

% 注意：不使用 http_server 库，因为它与我们的模块名冲突
% :- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_path)).
:- use_module(knowledge_base).
:- use_module(game_state).
:- use_module(game_logic).
:- use_module(win_conditions).
:- use_module(pddl_interface).

% ----------------------------------------------------------------------------
% 服务器配置
% ----------------------------------------------------------------------------

% 默认端口
default_port(8080).

% Playground 静态文件目录
playground_dir('playground').

% 获取项目根目录
project_root(Root) :-
    (source_file(game_http_server:project_root(_), ModuleFile) ->
        absolute_file_name(ModuleFile, AbsFile),
        file_directory_name(AbsFile, PrologDir),
        file_directory_name(PrologDir, Root)
    ;
        prolog_load_context(file, CurrentFile),
        absolute_file_name(CurrentFile, AbsFile),
        file_directory_name(AbsFile, PrologDir),
        file_directory_name(PrologDir, Root)
    ).

% ----------------------------------------------------------------------------
% 启动服务器
% ----------------------------------------------------------------------------

start_server :-
    default_port(Port),
    start_server(Port).

start_server(Port) :-
    % 注册路由（API 路由需要在静态文件路由之前）
    http_handler(root(api/status), api_status, [method(get)]),
    http_handler(root(api/init), api_init, [method(post)]),
    http_handler(root(api/command), api_command, [method(post)]),
    http_handler(root(api/map), api_map, [method(get)]),
    http_handler(root(api/rooms), api_room_info, [method(get), prefix]),
    http_handler(root(.), serve_playground, [prefix]),
    
    % 启动服务器（绑定到 0.0.0.0 以允许外部访问）
    use_module(library(http/http_server)),
    % 使用 Host:Port 格式绑定到所有接口
    Address = '0.0.0.0':Port,
    http_server([port(Address)]),
    format('HTTP server started on port ~w~n', [Port]),
    format('Server listening on 0.0.0.0:~w~n', [Port]),
    format('Open http://localhost:~w in your browser (from container)~n', [Port]),
    format('Or access from host machine via mapped port~n', []),
    % 保持服务器运行
    thread_get_message(_).

% ----------------------------------------------------------------------------
% 静态文件服务（Playground）
% ----------------------------------------------------------------------------

serve_playground(Request) :-
    playground_dir(Dir),
    project_root(Root),
    atomic_list_concat([Root, '/', Dir], PlaygroundPath),
    http_reply_from_files(PlaygroundPath, [index('index.html')], Request).

% ----------------------------------------------------------------------------
% API: 获取游戏状态
% GET /api/status
% ----------------------------------------------------------------------------

api_status(_Request) :-
    % 获取玩家位置
    (at_player(PlayerLoc) -> PlayerLocation = PlayerLoc; PlayerLocation = null),
    
    % 获取实体位置
    (at_entity(EntityLoc) -> EntityLocation = EntityLoc; EntityLocation = null),
    
    % 获取理智值
    (sanity(Sanity) -> SanityValue = Sanity; SanityValue = 100),
    
    % 获取持有物品
    (holding(Item) -> HoldingItem = Item; HoldingItem = null),
    
    % 获取当前房间的物品
    (PlayerLocation \= null ->
        findall(Item, item_location(Item, PlayerLocation), ItemsHere)
    ;
        ItemsHere = []
    ),
    
    % 获取当前房间的出口
    (PlayerLocation \= null ->
        findall(json{direction: Dir, to: Room}, connect(PlayerLocation, Dir, Room), Exits)
    ;
        Exits = []
    ),
    
    % 检查游戏状态
    (check_win_condition -> GameStatus = win
    ; check_lose_condition -> GameStatus = lose
    ; GameStatus = playing),
    
    % 构建响应
    Reply = json{
        player_location: PlayerLocation,
        entity_location: EntityLocation,
        sanity: SanityValue,
        holding: HoldingItem,
        items_here: ItemsHere,
        exits: Exits,
        game_status: GameStatus
    },
    
    cors_reply_json(Reply).

% ----------------------------------------------------------------------------
% API: 初始化游戏
% POST /api/init
% ----------------------------------------------------------------------------

api_init(_Request) :-
    % 捕获 init_game_state 的输出，避免干扰 HTTP 响应
    with_output_to(string(_), init_game_state),
    Reply = json{
        success: true,
        message: 'Game initialized'
    },
    cors_reply_json(Reply).

% ----------------------------------------------------------------------------
% API: 执行游戏命令
% POST /api/command
% Body: {"command": "move(east)"} 或 {"command": "look"}
% ----------------------------------------------------------------------------

api_command(Request) :-
    http_read_json_dict(Request, CommandDict),
    get_dict(command, CommandDict, CommandStr),
    
    % 解析命令字符串
    catch(
        term_string(Command, CommandStr),
        _,
        (Command = CommandStr)
    ),
    
    % 捕获输出
    with_output_to(string(Output), (
        process_command_with_output(Command, Success)
    )),
    
    % 更新实体位置（如果命令成功）
    (Success = true -> 
        catch(
            with_output_to(string(_), update_entity_from_pddl),
            _,
            true
        )
    ; 
        true
    ),
    
    % 检查游戏状态
    (check_win_condition -> GameStatus = win
    ; check_lose_condition -> GameStatus = lose
    ; GameStatus = playing),
    
    % 获取更新后的状态
    (at_player(PlayerLoc) -> PlayerLocation = PlayerLoc; PlayerLocation = null),
    (at_entity(EntityLoc) -> EntityLocation = EntityLoc; EntityLocation = null),
    (sanity(S) -> SanityValue = S; SanityValue = 100),
    (holding(Item) -> HoldingItem = Item; HoldingItem = null),
    
    Reply = json{
        success: Success,
        output: Output,
        game_status: GameStatus,
        player_location: PlayerLocation,
        entity_location: EntityLocation,
        sanity: SanityValue,
        holding: HoldingItem
    },
    
    cors_reply_json(Reply).

% ----------------------------------------------------------------------------
% API: 获取地图信息
% GET /api/map
% ----------------------------------------------------------------------------

api_map(_Request) :-
    % 获取所有房间
    findall(Room, room(Room), Rooms),
    
    % 获取所有连接
    findall(
        json{from: From, direction: Dir, to: To},
        connect(From, Dir, To),
        Connections
    ),
    
    % 获取房间属性
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
% API: 获取房间信息
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
        % 获取房间连接
        findall(json{direction: Dir, to: RoomTo}, connect(Room, Dir, RoomTo), Exits),
        
        % 获取房间物品
        findall(Item, item_location(Item, Room), Items),
        
        % 获取房间属性
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
% 辅助函数：处理命令并捕获输出
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
% 命令处理（从 main.pl 复制）
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
% 检查游戏状态（不输出消息）
% ----------------------------------------------------------------------------

check_win_condition :-
    at_player(manila_room),
    is_exit(manila_room).

check_lose_condition :-
    (sanity(S), S =< 0; true),
    !.
check_lose_condition :-
    at_player(PlayerLoc),
    at_entity(EntityLoc),
    PlayerLoc = EntityLoc.

% ----------------------------------------------------------------------------
% CORS 支持
% ----------------------------------------------------------------------------

cors_reply_json(JSON) :-
    cors_enable,
    reply_json(JSON).

