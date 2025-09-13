% =============================================================================
% TESTS.PL - Tests unitaires et validation du solveur de Taquin
% =============================================================================
% Ce module contient tous les tests pour valider le bon fonctionnement :
% - Tests unitaires par module (game, astar, display)
% - Validation des cas de test (professeur + personnalisé)
% - Tests de robustesse et gestion d'erreurs
% - Vérification des résultats attendus
% =============================================================================

:- use_module(game).
:- use_module(astar).
:- use_module(display).

% run_all_tests
% Lance tous les tests du système
run_all_tests :-
    write('=== TESTS UNITAIRES DU SOLVEUR DE TAQUIN ==='), nl,
    test_game_module,
    test_astar_module,
    test_integration,
    write('=== TOUS LES TESTS TERMINÉS ==='), nl.

% test_game_module
% Tests du module game.pl
test_game_module :-
    write('Testing game.pl...'), nl,
    test_valid_state,
    test_find_blank,
    test_generate_moves,
    test_apply_move,
    write('✓ game.pl tests passed'), nl, nl.

% test_astar_module  
% Tests du module astar.pl
test_astar_module :-
    write('Testing astar.pl...'), nl,
    test_misplaced_tiles,
    test_manhattan_distance,
    write('✓ astar.pl tests passed'), nl, nl.

% test_integration
% Tests d'intégration complets
test_integration :-
    write('Testing integration...'), nl,
    test_case1_results,
    test_case2_results,
    write('✓ Integration tests passed'), nl, nl.

% test_valid_state
% Test de validation d'états
test_valid_state :-
    valid_state([1,2,3,4,5,6,7,8,0]),
    \+ valid_state([1,2,3,4,5,6,7,8]),  % trop court
    \+ valid_state([1,2,3,4,5,6,7,8,9,0]),  % trop long
    \+ valid_state([1,1,3,4,5,6,7,8,0]).  % doublons

% test_find_blank
% Test de recherche de case vide
test_find_blank :-
    find_blank([1,2,3,5,0,6,4,7,8], 4),
    find_blank([0,2,3,5,1,6,4,7,8], 0),
    find_blank([1,2,3,5,1,6,4,7,0], 8).

% test_generate_moves
% Test de génération des mouvements
test_generate_moves :-
    generate_moves([1,2,3,5,0,6,4,7,8], Moves),
    length(Moves, 4),  % 4 mouvements possibles depuis le centre
    generate_moves([0,2,3,5,1,6,4,7,8], MovesCorner),
    length(MovesCorner, 2).  % 2 mouvements depuis un coin

% test_apply_move
% Test d'application des mouvements
test_apply_move :-
    apply_move([1,2,3,5,0,6,4,7,8], up, [1,0,3,5,2,6,4,7,8]),
    apply_move([1,2,3,5,0,6,4,7,8], left, [1,2,3,0,5,6,4,7,8]).

% test_misplaced_tiles
% Test de l'heuristique tuiles mal placées
test_misplaced_tiles :-
    goal_state(Goal),
    misplaced_tiles([1,2,3,5,0,6,4,7,8], Goal, 5),  % État initial du TP1
    misplaced_tiles(Goal, Goal, 0).  % État but = 0 tuiles mal placées

% test_manhattan_distance
% Test de l'heuristique distance de Manhattan
test_manhattan_distance :-
    goal_state(Goal),
    manhattan_distance([1,2,3,5,0,6,4,7,8], Goal, Distance),
    Distance > 0,  % Distance positive pour état non-but
    manhattan_distance(Goal, Goal, 0).  % État but = distance 0

% test_case1_results
% Validation exacte du cas de test 1 (professeur)
test_case1_results :-
    write('Validating Case 1 (Professor example)...'), nl,
    solve_puzzle(case1, result(Path, Cost, Expanded)),
    % Vérification des résultats exacts attendus
    Cost = 4,  % 4 mouvements selon l'exemple
    Expanded = 9,  % 9 nœuds explorés selon l'exemple
    length(Path, 5),  % 5 états dans le chemin (A→B→C→D→E)
    Path = [Initial|_],
    last(Path, Final),
    initial_state(Initial),
    goal_state(Final),
    write('✓ Case 1 results validated: Cost=4, Expanded=9, Path length=5'), nl.

% test_case2_results
% Validation du cas de test 2 (personnalisé)
test_case2_results :-
    write('Validating Case 2 (Custom example)...'), nl,
    solve_puzzle(case2, result(Path, Cost, Expanded)),
    % Vérifications générales
    Cost >= 6,  % Au moins 6 mouvements selon spécifications
    Expanded > 0,  % Au moins quelques nœuds explorés
    length(Path, PathLength),
    PathLength is Cost + 1,  % Path = Cost + 1 états
    Path = [Initial|_],
    last(Path, Final),
    custom_initial_state(Initial),
    custom_goal_state(Final),
    write('✓ Case 2 results validated: Cost>=6, consistent Path/Cost'), nl.

% last(+List, -LastElement)
% Utilitaire pour obtenir le dernier élément d'une liste
last([X], X) :- !.
last([_|T], X) :- last(T, X).

% benchmark_performance
% Test de performance (optionnel)
benchmark_performance :-
    write('Running performance benchmarks...'), nl,
    get_time(Start1),
    solve_puzzle(case1, _),
    get_time(End1),
    Time1 is End1 - Start1,
    get_time(Start2),
    solve_puzzle(case2, _),
    get_time(End2),
    Time2 is End2 - Start2,
    format('Case 1 solved in ~3f seconds~n', [Time1]),
    format('Case 2 solved in ~3f seconds~n', [Time2]),
    (   (Time1 < 1.0, Time2 < 5.0) ->
        write('✓ Performance benchmarks passed')
    ;   write('⚠ Performance could be improved')
    ),
    nl.