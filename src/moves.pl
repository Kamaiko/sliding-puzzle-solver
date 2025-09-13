/**
 * moves.pl - Génération des mouvements valides dans le taquin
 * 
 * Ce module gère la génération de tous les mouvements possibles
 * à partir d'un état donné du taquin.
 */

:- module(moves, [
    generate_moves/2,
    valid_moves/2,
    apply_move/3,
    move_direction/2
]).

:- use_module(board).

%% generate_moves(+State, -Moves)
%  Génère tous les mouvements valides à partir d'un état
generate_moves(State, Moves) :-
    board:get_empty_position(State, EmptyPos),
    valid_moves(EmptyPos, ValidPositions),
    generate_moves_helper(State, EmptyPos, ValidPositions, Moves).

generate_moves_helper(_, _, [], []).
generate_moves_helper(State, EmptyPos, [Pos|RestPos], [NewState|RestMoves]) :-
    board:swap_positions(State, EmptyPos, Pos, NewState),
    generate_moves_helper(State, EmptyPos, RestPos, RestMoves).

%% valid_moves(+EmptyPosition, -ValidPositions)
%  Détermine les positions valides où la case vide peut se déplacer
valid_moves(EmptyPos, ValidPositions) :-
    findall(Pos, can_move(EmptyPos, Pos), ValidPositions).

%% can_move(+EmptyPosition, -NewPosition)
%  Vérifie si un mouvement est possible
can_move(EmptyPos, NewPos) :-
    board:position_to_coordinates(EmptyPos, Row, Col),
    % Mouvement vers le haut
    (   UpRow is Row - 1, UpRow >= 0,
        board:coordinates_to_position(UpRow, Col, NewPos)
    ;   % Mouvement vers le bas
        DownRow is Row + 1, DownRow =< 2,
        board:coordinates_to_position(DownRow, Col, NewPos)
    ;   % Mouvement vers la gauche
        LeftCol is Col - 1, LeftCol >= 0,
        board:coordinates_to_position(Row, LeftCol, NewPos)
    ;   % Mouvement vers la droite
        RightCol is Col + 1, RightCol =< 2,
        board:coordinates_to_position(Row, RightCol, NewPos)
    ).

%% apply_move(+State, +Move, -NewState)
%  Applique un mouvement à un état (Move est la nouvelle position de l'espace vide)
apply_move(State, Move, NewState) :-
    board:get_empty_position(State, EmptyPos),
    board:swap_positions(State, EmptyPos, Move, NewState).

%% move_direction(+FromPos, +ToPos, -Direction)
%  Détermine la direction d'un mouvement
move_direction(FromPos, ToPos, Direction) :-
    board:position_to_coordinates(FromPos, Row1, Col1),
    board:position_to_coordinates(ToPos, Row2, Col2),
    (   Row1 < Row2 -> Direction = bas
    ;   Row1 > Row2 -> Direction = haut
    ;   Col1 < Col2 -> Direction = droite
    ;   Col1 > Col2 -> Direction = gauche
    ).

%% get_move_name(+OldState, +NewState, -MoveName)
%  Détermine le nom du mouvement effectué
get_move_name(OldState, NewState, MoveName) :-
    board:get_empty_position(OldState, OldEmpty),
    board:get_empty_position(NewState, NewEmpty),
    move_direction(OldEmpty, NewEmpty, MoveName).

%% is_reverse_move(+Move1, +Move2)
%  Vérifie si deux mouvements sont inverses l'un de l'autre
is_reverse_move(haut, bas).
is_reverse_move(bas, haut).
is_reverse_move(gauche, droite).
is_reverse_move(droite, gauche).