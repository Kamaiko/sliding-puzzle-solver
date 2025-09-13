/**
 * search.pl - Implémentation de l'algorithme A* pour le taquin
 * 
 * Ce module implémente l'algorithme de recherche A* avec
 * gestion de la file de priorité et des états visités.
 */

:- module(search, [
    astar_search/6,
    init_search/0,
    cleanup_search/0
]).

:- use_module(board).
:- use_module(moves).
:- use_module(heuristics).

% Tables dynamiques pour l'état de la recherche
:- dynamic(open_list/4).      % open_list(State, F, G, Parent)
:- dynamic(closed_list/1).    % closed_list(State)
:- dynamic(search_stats/2).   % search_stats(expanded, generated)

%% astar_search(+InitialState, +GoalState, +HeuristicType, -Path, -Cost, -Expanded)
%  Algorithme A* principal
astar_search(InitialState, GoalState, HeuristicType, Path, Cost, Expanded) :-
    init_search,
    heuristics:heuristic_value(InitialState, GoalState, HeuristicType, H),
    F is 0 + H,
    assert(open_list(InitialState, F, 0, nil)),
    assert(search_stats(expanded, 0)),
    assert(search_stats(generated, 1)),
    astar_loop(GoalState, HeuristicType, Solution),
    (   Solution \= fail ->
        reconstruct_path(Solution, [], Path),
        length(Path, Length),
        Cost is Length - 1,
        retract(search_stats(expanded, Expanded))
    ;   Path = [], Cost = -1, Expanded = 0
    ),
    cleanup_search.

%% astar_loop(+GoalState, +HeuristicType, -Solution)
%  Boucle principale de l'algorithme A*
astar_loop(GoalState, HeuristicType, Solution) :-
    % Trouver le nœud avec le plus petit f dans open_list
    find_min_f_node(MinState, MinF, MinG, MinParent),
    !,
    retract(open_list(MinState, MinF, MinG, MinParent)),
    
    % Vérifier si c'est l'état but
    (   board:is_goal_state(MinState, GoalState) ->
        Solution = node(MinState, MinF, MinG, MinParent)
    ;   % Expansion du nœud
        assert(closed_list(MinState)),
        increment_expanded,
        expand_node(MinState, MinG, MinParent, GoalState, HeuristicType),
        astar_loop(GoalState, HeuristicType, Solution)
    ).
astar_loop(_, _, fail).  % Plus de nœuds à explorer

%% find_min_f_node(-State, -F, -G, -Parent)
%  Trouve le nœud avec la plus petite valeur f
find_min_f_node(MinState, MinF, MinG, MinParent) :-
    findall(f(F, State, G, Parent), open_list(State, F, G, Parent), Nodes),
    Nodes \= [],
    sort(Nodes, [f(MinF, MinState, MinG, MinParent)|_]).

%% expand_node(+State, +G, +Parent, +GoalState, +HeuristicType)
%  Expanse un nœud en générant ses successeurs
expand_node(State, G, Parent, GoalState, HeuristicType) :-
    moves:generate_moves(State, Successors),
    NewG is G + 1,
    process_successors(Successors, NewG, State, GoalState, HeuristicType).

%% process_successors(+Successors, +G, +Parent, +GoalState, +HeuristicType)
%  Traite tous les successeurs d'un nœud
process_successors([], _, _, _, _).
process_successors([Successor|Rest], G, Parent, GoalState, HeuristicType) :-
    increment_generated,
    process_successor(Successor, G, Parent, GoalState, HeuristicType),
    process_successors(Rest, G, Parent, GoalState, HeuristicType).

%% process_successor(+State, +G, +Parent, +GoalState, +HeuristicType)
%  Traite un successeur individuel
process_successor(State, G, Parent, GoalState, HeuristicType) :-
    \+ closed_list(State),  % Ignorer si déjà dans closed_list
    heuristics:heuristic_value(State, GoalState, HeuristicType, H),
    F is G + H,
    
    (   open_list(State, OldF, OldG, _) ->
        % État déjà dans open_list, mettre à jour si meilleur chemin
        (   G < OldG ->
            retract(open_list(State, OldF, OldG, _)),
            assert(open_list(State, F, G, Parent))
        ;   true
        )
    ;   % Nouvel état, ajouter à open_list
        assert(open_list(State, F, G, Parent))
    ).

%% reconstruct_path(+Node, +AccPath, -Path)
%  Reconstruit le chemin de la solution
reconstruct_path(node(State, _, _, nil), AccPath, [State|AccPath]) :- !.
reconstruct_path(node(State, _, _, Parent), AccPath, Path) :-
    reconstruct_path(Parent, [State|AccPath], Path).

%% init_search/0
%  Initialise les structures de données pour la recherche
init_search :-
    cleanup_search.

%% cleanup_search/0
%  Nettoie les structures de données après la recherche
cleanup_search :-
    retractall(open_list(_, _, _, _)),
    retractall(closed_list(_)),
    retractall(search_stats(_, _)).

%% increment_expanded/0
%  Incrémente le compteur de nœuds expansés
increment_expanded :-
    retract(search_stats(expanded, Count)),
    NewCount is Count + 1,
    assert(search_stats(expanded, NewCount)).

%% increment_generated/0
%  Incrémente le compteur de nœuds générés
increment_generated :-
    retract(search_stats(generated, Count)),
    NewCount is Count + 1,
    assert(search_stats(generated, NewCount)).

%% get_search_stats(-Expanded, -Generated)
%  Obtient les statistiques de recherche
get_search_stats(Expanded, Generated) :-
    (   search_stats(expanded, Expanded) -> true ; Expanded = 0 ),
    (   search_stats(generated, Generated) -> true ; Generated = 0 ).