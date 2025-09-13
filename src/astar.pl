% =============================================================================
% ASTAR.PL - Algorithme A* et Heuristiques pour le Taquin
% =============================================================================
% Ce module contient le cœur algorithmique du solveur :
% - Algorithme A* complet avec file de priorité
% - Heuristique des tuiles mal placées (principale selon TP1)
% - Heuristique de Manhattan (optionnelle pour comparaison)
% - Gestion des états visités et reconstruction du chemin
% =============================================================================

:- use_module(game).

% Structure d'un nœud A* : node(State, F, G, Parent)
% State = état du taquin [1,2,3,5,0,6,4,7,8]
% F = f(n) = g(n) + h(n) (coût total estimé)
% G = g(n) = profondeur depuis l'état initial (coût réel)
% Parent = nœud parent pour reconstruction du chemin

% misplaced_tiles(+State, +Goal, -Count)
% Heuristique principale : compte les tuiles mal placées (excluant case vide)
misplaced_tiles(State, Goal, Count) :-
    misplaced_tiles_helper(State, Goal, 0, Count).

misplaced_tiles_helper([], [], Count, Count).
misplaced_tiles_helper([H1|T1], [H2|T2], Acc, Count) :-
    (   (H1 \= H2, H1 \= 0, H2 \= 0) ->
        Acc1 is Acc + 1
    ;   Acc1 = Acc
    ),
    misplaced_tiles_helper(T1, T2, Acc1, Count).

% manhattan_distance(+State, +Goal, -Distance)
% Heuristique optionnelle : distance de Manhattan
manhattan_distance(State, Goal, Distance) :-
    manhattan_helper(State, Goal, 0, 0, Distance).

manhattan_helper([], _, _, Distance, Distance).
manhattan_helper([Tile|Rest], Goal, Index, Acc, Distance) :-
    (   Tile =\= 0 ->
        nth0(GoalIndex, Goal, Tile),
        tile_manhattan_distance(Index, GoalIndex, TileDist),
        Acc1 is Acc + TileDist
    ;   Acc1 = Acc
    ),
    Index1 is Index + 1,
    manhattan_helper(Rest, Goal, Index1, Acc1, Distance).

% tile_manhattan_distance(+Pos1, +Pos2, -Distance)
% Distance de Manhattan entre deux positions sur le plateau 3x3
tile_manhattan_distance(Pos1, Pos2, Distance) :-
    Row1 is Pos1 // 3, Col1 is Pos1 mod 3,
    Row2 is Pos2 // 3, Col2 is Pos2 mod 3,
    Distance is abs(Row1 - Row2) + abs(Col1 - Col2).

% heuristic_value(+State, +Goal, +Type, -Value)
% Interface unifiée pour sélection d'heuristique
heuristic_value(State, Goal, misplaced, Value) :-
    misplaced_tiles(State, Goal, Value).
heuristic_value(State, Goal, manhattan, Value) :-
    manhattan_distance(State, Goal, Value).

% astar_search(+InitialState, +GoalState, +HeuristicType, -Path, -Cost, -Expanded)
% Algorithme A* principal
astar_search(Initial, Goal, HType, Path, Cost, Expanded) :-
    heuristic_value(Initial, Goal, HType, H),
    InitialNode = node(Initial, H, 0, nil),
    astar_loop([InitialNode], Goal, HType, [], FinalNode, Expanded),
    reconstruct_path(FinalNode, Path),
    FinalNode = node(_, _, Cost, _).

% astar_loop(+OpenList, +Goal, +HType, +ClosedList, -FinalNode, -Expanded)
% Boucle principale de l'algorithme A*
astar_loop([Node|_], Goal, _, _, Node, 1) :-
    Node = node(State, _, _, _),
    State = Goal, !.
astar_loop([CurrentNode|RestOpen], Goal, HType, Closed, FinalNode, Expanded) :-
    CurrentNode = node(CurrentState, _, G, _),
    \+ member(CurrentState, Closed),
    generate_moves(CurrentState, Successors),
    G1 is G + 1,
    create_successor_nodes(Successors, Goal, HType, G1, CurrentNode, NewNodes),
    append(RestOpen, NewNodes, UpdatedOpen),
    sort_by_f_value(UpdatedOpen, SortedOpen),
    Expanded1 is 1,
    astar_loop(SortedOpen, Goal, HType, [CurrentState|Closed], FinalNode, RestExpanded),
    Expanded is Expanded1 + RestExpanded.
astar_loop([CurrentNode|RestOpen], Goal, HType, Closed, FinalNode, Expanded) :-
    CurrentNode = node(CurrentState, _, _, _),
    member(CurrentState, Closed),
    astar_loop(RestOpen, Goal, HType, Closed, FinalNode, Expanded).

% create_successor_nodes(+States, +Goal, +HType, +G, +Parent, -Nodes)
% Crée les nœuds successeurs avec évaluation f(n)
create_successor_nodes([], _, _, _, _, []).
create_successor_nodes([State|RestStates], Goal, HType, G, Parent, [Node|RestNodes]) :-
    heuristic_value(State, Goal, HType, H),
    F is G + H,
    Node = node(State, F, G, Parent),
    create_successor_nodes(RestStates, Goal, HType, G, Parent, RestNodes).

% sort_by_f_value(+Nodes, -SortedNodes)
% Trie les nœuds par valeur f(n) croissante (file de priorité)
sort_by_f_value(Nodes, SortedNodes) :-
    predsort(compare_f_values, Nodes, SortedNodes).

% compare_f_values(-Order, +Node1, +Node2)
% Comparaison pour le tri par valeur f(n)
compare_f_values(Order, node(_, F1, _, _), node(_, F2, _, _)) :-
    compare(Order, F1, F2).

% reconstruct_path(+FinalNode, -Path)
% Reconstruit le chemin solution par backtracking
reconstruct_path(node(State, _, _, nil), [State]) :- !.
reconstruct_path(node(State, _, _, Parent), [State|RestPath]) :-
    reconstruct_path(Parent, RestPath).

% solve_puzzle(+TestCase, -Result)
% Interface principale pour résoudre un cas de test
solve_puzzle(case1, result(Path, Cost, Expanded)) :-
    initial_state(Initial),
    goal_state(Goal),
    astar_search(Initial, Goal, misplaced, Path, Cost, Expanded).

solve_puzzle(case2, result(Path, Cost, Expanded)) :-
    custom_initial_state(Initial),
    custom_goal_state(Goal),
    astar_search(Initial, Goal, misplaced, Path, Cost, Expanded).