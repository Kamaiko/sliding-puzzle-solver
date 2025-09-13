/**
 * board.pl - Représentation et manipulation de l'état du taquin
 * 
 * Ce module définit la structure de données pour représenter
 * l'état du taquin et les prédicats de base pour le manipuler.
 * 
 * Représentation: Liste de 9 éléments [1,2,3,4,5,6,7,8,0]
 * où 0 représente la case vide.
 * 
 * Position dans la liste:
 * 0 1 2
 * 3 4 5  
 * 6 7 8
 */

:- module(board, [
    is_valid_state/1,
    is_goal_state/2,
    get_empty_position/2,
    get_position/3,
    set_position/4,
    swap_positions/4
]).

%% is_valid_state(+State)
%  Vérifie si un état est valide (contient les chiffres 0-8 exactement une fois)
is_valid_state(State) :-
    length(State, 9),
    sort(State, [0,1,2,3,4,5,6,7,8]).

%% is_goal_state(+State, +GoalState)
%  Vérifie si l'état actuel correspond à l'état but
is_goal_state(State, GoalState) :-
    State == GoalState.

%% get_empty_position(+State, -Position)
%  Trouve la position de la case vide (0) dans l'état
get_empty_position(State, Position) :-
    nth0(Position, State, 0).

%% get_position(+State, +Position, -Value)
%  Obtient la valeur à une position donnée
get_position(State, Position, Value) :-
    nth0(Position, State, Value).

%% set_position(+State, +Position, +Value, -NewState)
%  Crée un nouvel état avec une valeur modifiée à une position
set_position(State, Position, Value, NewState) :-
    length(State, Length),
    length(NewState, Length),
    set_position_helper(State, Position, Value, 0, NewState).

set_position_helper([], _, _, _, []).
set_position_helper([_|T], Pos, Value, Pos, [Value|NewT]) :-
    !,
    NextPos is Pos + 1,
    set_position_helper(T, Pos, Value, NextPos, NewT).
set_position_helper([H|T], Pos, Value, CurrentPos, [H|NewT]) :-
    NextPos is CurrentPos + 1,
    set_position_helper(T, Pos, Value, NextPos, NewT).

%% swap_positions(+State, +Pos1, +Pos2, -NewState)
%  Échange les valeurs à deux positions dans l'état
swap_positions(State, Pos1, Pos2, NewState) :-
    get_position(State, Pos1, Val1),
    get_position(State, Pos2, Val2),
    set_position(State, Pos1, Val2, TempState),
    set_position(TempState, Pos2, Val1, NewState).

%% position_to_coordinates(+Position, -Row, -Col)
%  Convertit une position linéaire en coordonnées (row, col)
position_to_coordinates(Position, Row, Col) :-
    Row is Position // 3,
    Col is Position mod 3.

%% coordinates_to_position(+Row, +Col, -Position)
%  Convertit des coordonnées en position linéaire
coordinates_to_position(Row, Col, Position) :-
    Position is Row * 3 + Col.