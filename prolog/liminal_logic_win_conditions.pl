% ============================================================================
% liminal_logic_win_conditions.pl
% ============================================================================
% Win/lose conditions: defines game end conditions.
% ============================================================================

:- module(win_conditions, [
    check_win/0,
    check_lose/0,
    game_over/1
]).

:- use_module(liminal_logic_game_state).
:- use_module(liminal_logic_knowledge_base).

% ----------------------------------------------------------------------------
% Win Condition Check
% ----------------------------------------------------------------------------

check_win :-
    at_player(manila_room),
    is_exit(manila_room),
    holding(key),  % Must be holding the key to win
    write('========================================'), nl,
    write('YOU ESCAPED!'), nl,
    write('You noclipped out of Level 0!'), nl,
    write('========================================'), nl,
    game_over(win),
    !.

% ----------------------------------------------------------------------------
% Lose Condition Check
% ----------------------------------------------------------------------------

check_lose :-
    sanity(S),
    S =< 0,
    write('========================================'), nl,
    write('GAME OVER'), nl,
    write('Your sanity has been depleted.'), nl,
    write('You are lost in the Backrooms forever...'), nl,
    write('========================================'), nl,
    game_over(lose_sanity),
    !.

check_lose :-
    at_player(PlayerLoc),
    at_entity(EntityLoc),
    PlayerLoc = EntityLoc,
    write('========================================'), nl,
    write('GAME OVER'), nl,
    write('The Howler has caught you!'), nl,
    write('You are lost in the Backrooms forever...'), nl,
    write('========================================'), nl,
    game_over(lose_caught),
    !.

% ----------------------------------------------------------------------------
% Game Over Handler
% ----------------------------------------------------------------------------

game_over(Result) :-
    % Set game over status
    set_game_over_status(Result),
    % TODO: persist results, clean up resources, etc.
    true.

