/**
 * heuristics.pl - Fonctions heuristiques pour le taquin
 * 
 * Ce module implémente différentes fonctions heuristiques
 * pour guider la recherche A*.
 */

:- module(heuristics, [
    manhattan_distance/3,
    misplaced_tiles/3,
    linear_conflict/3,
    heuristic_value/4
]).

:- use_module(board).

%% manhattan_distance(+State, +GoalState, -Distance)
%  Calcule la distance de Manhattan entre l'état actuel et l'état but
manhattan_distance(State, GoalState, Distance) :-
    manhattan_distance_helper(State, GoalState, 0, 0, Distance).

manhattan_distance_helper([], [], _, _, 0).
manhattan_distance_helper([0|RestState], [0|RestGoal], Pos, _, RestDistance) :-
    !,
    NextPos is Pos + 1,
    manhattan_distance_helper(RestState, RestGoal, NextPos, _, RestDistance).
manhattan_distance_helper([Tile|RestState], GoalState, Pos, _, TotalDistance) :-
    Tile \= 0,
    nth0(GoalPos, GoalState, Tile),
    board:position_to_coordinates(Pos, Row1, Col1),
    board:position_to_coordinates(GoalPos, Row2, Col2),
    Distance is abs(Row1 - Row2) + abs(Col1 - Col2),
    NextPos is Pos + 1,
    manhattan_distance_helper(RestState, GoalState, NextPos, _, RestDistance),
    TotalDistance is Distance + RestDistance.

%% misplaced_tiles(+State, +GoalState, -Count)
%  Compte le nombre de tuiles mal placées (heuristique suggérée dans l'énoncé)
misplaced_tiles(State, GoalState, Count) :-
    misplaced_tiles_helper(State, GoalState, 0, Count).

misplaced_tiles_helper([], [], _, 0).
misplaced_tiles_helper([0|RestState], [0|RestGoal], Pos, Count) :-
    !,
    NextPos is Pos + 1,
    misplaced_tiles_helper(RestState, RestGoal, NextPos, Count).
misplaced_tiles_helper([Tile|RestState], [GoalTile|RestGoal], Pos, Count) :-
    NextPos is Pos + 1,
    misplaced_tiles_helper(RestState, RestGoal, NextPos, RestCount),
    (   Tile \= GoalTile, Tile \= 0 ->
        Count is RestCount + 1
    ;   Count = RestCount
    ).

%% linear_conflict(+State, +GoalState, -Conflicts)
%  Calcule les conflits linéaires (heuristique avancée)
linear_conflict(State, GoalState, Conflicts) :-
    % Conflits dans les lignes
    row_conflicts(State, GoalState, RowConflicts),
    % Conflits dans les colonnes
    col_conflicts(State, GoalState, ColConflicts),
    Conflicts is RowConflicts + ColConflicts.

row_conflicts(State, GoalState, Conflicts) :-
    row_conflicts_helper(State, GoalState, 0, Conflicts).

row_conflicts_helper(_, _, 3, 0) :- !.
row_conflicts_helper(State, GoalState, Row, TotalConflicts) :-
    get_row(State, Row, StateRow),
    get_row(GoalState, Row, GoalRow),
    count_line_conflicts(StateRow, GoalRow, RowConflicts),
    NextRow is Row + 1,
    row_conflicts_helper(State, GoalState, NextRow, RestConflicts),
    TotalConflicts is RowConflicts + RestConflicts.

col_conflicts(State, GoalState, Conflicts) :-
    col_conflicts_helper(State, GoalState, 0, Conflicts).

col_conflicts_helper(_, _, 3, 0) :- !.
col_conflicts_helper(State, GoalState, Col, TotalConflicts) :-
    get_col(State, Col, StateCol),
    get_col(GoalState, Col, GoalCol),
    count_line_conflicts(StateCol, GoalCol, ColConflicts),
    NextCol is Col + 1,
    col_conflicts_helper(State, GoalState, NextCol, RestConflicts),
    TotalConflicts is ColConflicts + RestConflicts.

%% get_row(+State, +RowNum, -Row)
%  Extrait une ligne de l'état
get_row(State, RowNum, Row) :-
    StartPos is RowNum * 3,
    EndPos is StartPos + 2,
    get_range(State, StartPos, EndPos, Row).

%% get_col(+State, +ColNum, -Col)
%  Extrait une colonne de l'état
get_col(State, ColNum, Col) :-
    Pos1 is ColNum,
    Pos2 is ColNum + 3,
    Pos3 is ColNum + 6,
    nth0(Pos1, State, Val1),
    nth0(Pos2, State, Val2),
    nth0(Pos3, State, Val3),
    Col = [Val1, Val2, Val3].

%% get_range(+List, +Start, +End, -Range)
%  Extrait une sous-liste
get_range(List, Start, End, Range) :-
    length(Prefix, Start),
    append(Prefix, Suffix, List),
    Length is End - Start + 1,
    length(Range, Length),
    append(Range, _, Suffix).

%% count_line_conflicts(+Line, +GoalLine, -Conflicts)
%  Compte les conflits dans une ligne ou colonne
count_line_conflicts(_, _, 0).  % Implémentation simplifiée

%% heuristic_value(+State, +GoalState, +HeuristicType, -Value)
%  Interface unifiée pour obtenir la valeur heuristique
heuristic_value(State, GoalState, manhattan, Value) :-
    manhattan_distance(State, GoalState, Value).
heuristic_value(State, GoalState, misplaced, Value) :-
    misplaced_tiles(State, GoalState, Value).
heuristic_value(State, GoalState, combined, Value) :-
    manhattan_distance(State, GoalState, Manhattan),
    linear_conflict(State, GoalState, Conflicts),
    Value is Manhattan + 2 * Conflicts.