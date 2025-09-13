% =============================================================================
% GAME.PL - Logique du jeu de Taquin (États et Mouvements)
% =============================================================================
% Ce module contient la représentation et la manipulation des états du taquin :
% - Définition des états initial et final
% - Validation des configurations
% - Génération et application des mouvements valides
% - Utilitaires de manipulation d'états
% =============================================================================

% États de référence pour le TP1
initial_state([1,2,3,5,0,6,4,7,8]).
goal_state([1,2,3,4,5,6,7,8,0]).

% État personnalisé pour le second cas de test (plus complexe - 6+ mouvements)
custom_initial_state([2,8,3,1,6,4,7,0,5]).
custom_goal_state([1,2,3,8,0,4,7,6,5]).

% valid_state(+State)
% Vérifie qu'un état est valide (9 éléments, chiffres 0-8 uniques)
valid_state(State) :-
    length(State, 9),
    sort(State, [0,1,2,3,4,5,6,7,8]).

% find_blank(+State, -Position)
% Trouve la position de la case vide (0) dans l'état
find_blank(State, Position) :-
    nth0(Position, State, 0).

% position_to_coords(+Position, -Row, -Col)
% Convertit position linéaire (0-8) en coordonnées matricielles (0-2, 0-2)
position_to_coords(Pos, Row, Col) :-
    Row is Pos // 3,
    Col is Pos mod 3.

% coords_to_position(+Row, +Col, -Position)
% Convertit coordonnées matricielles en position linéaire
coords_to_position(Row, Col, Pos) :-
    Pos is Row * 3 + Col.

% valid_move(+BlankPos, +Direction)
% Vérifie si un mouvement est valide selon les limites du plateau 3x3
valid_move(BlankPos, up) :-
    BlankPos >= 3.
valid_move(BlankPos, down) :-
    BlankPos =< 5.
valid_move(BlankPos, left) :-
    BlankPos mod 3 =\= 0.
valid_move(BlankPos, right) :-
    BlankPos mod 3 =\= 2.

% get_target_position(+BlankPos, +Direction, -TargetPos)
% Calcule la position cible selon la direction du mouvement
get_target_position(BlankPos, up, TargetPos) :-
    TargetPos is BlankPos - 3.
get_target_position(BlankPos, down, TargetPos) :-
    TargetPos is BlankPos + 3.
get_target_position(BlankPos, left, TargetPos) :-
    TargetPos is BlankPos - 1.
get_target_position(BlankPos, right, TargetPos) :-
    TargetPos is BlankPos + 1.

% swap_tiles(+State, +Pos1, +Pos2, -NewState)
% Échange les tuiles aux positions Pos1 et Pos2
swap_tiles(State, Pos1, Pos2, NewState) :-
    nth0(Pos1, State, Tile1),
    nth0(Pos2, State, Tile2),
    replace_nth0(State, Pos1, Tile2, TempState),
    replace_nth0(TempState, Pos2, Tile1, NewState).

% replace_nth0(+List, +Index, +Element, -NewList)
% Remplace l'élément à l'index donné dans la liste
replace_nth0(List, Index, Element, NewList) :-
    length(Prefix, Index),
    append(Prefix, [_|Suffix], List),
    append(Prefix, [Element|Suffix], NewList).

% apply_move(+State, +Direction, -NewState)
% Applique un mouvement dans la direction donnée
apply_move(State, Direction, NewState) :-
    find_blank(State, BlankPos),
    valid_move(BlankPos, Direction),
    get_target_position(BlankPos, Direction, TargetPos),
    swap_tiles(State, BlankPos, TargetPos, NewState).

% generate_moves(+State, -Successors)
% Génère tous les mouvements valides depuis un état
generate_moves(State, Successors) :-
    find_blank(State, BlankPos),
    findall(NewState,
        (member(Direction, [up, down, left, right]),
         valid_move(BlankPos, Direction),
         apply_move(State, Direction, NewState)),
        Successors).

% is_goal(+State)
% Vérifie si l'état est l'état but
is_goal(State) :-
    goal_state(Goal),
    State = Goal.

% is_custom_goal(+State)
% Vérifie si l'état est l'état but personnalisé
is_custom_goal(State) :-
    custom_goal_state(Goal),
    State = Goal.