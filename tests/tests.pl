/**
 * tests.pl - Tests unitaires et cas de test pour le solveur de taquin
 * 
 * Ce module contient tous les tests pour valider le bon fonctionnement
 * des différents composants du solveur.
 */

:- module(tests, [
    run_all_tests/0,
    test_board_operations/0,
    test_move_generation/0,
    test_heuristics/0,
    test_search_algorithm/0,
    test_case_1/0,
    test_case_2/0
]).

:- use_module(board).
:- use_module(moves).
:- use_module(heuristics).
:- use_module(search).
:- use_module(utils).

%% run_all_tests/0
%  Exécute tous les tests
run_all_tests :-
    writeln('=== EXÉCUTION DE TOUS LES TESTS ==='),
    test_board_operations,
    test_move_generation,
    test_heuristics,
    test_search_algorithm,
    test_case_1,
    test_case_2,
    writeln('=== TOUS LES TESTS TERMINÉS ===').

%% test_board_operations/0
%  Teste les opérations de base sur le plateau
test_board_operations :-
    writeln('--- Test des opérations de plateau ---'),
    
    % Test de validation d'état
    TestState1 = [1,2,3,4,5,6,7,8,0],
    (   board:is_valid_state(TestState1) ->
        writeln('✓ État valide reconnu')
    ;   writeln('✗ Échec validation état valide')
    ),
    
    % Test position case vide
    (   board:get_empty_position(TestState1, 8) ->
        writeln('✓ Position case vide correcte')
    ;   writeln('✗ Échec position case vide')
    ),
    
    % Test échange de positions
    board:swap_positions(TestState1, 8, 5, NewState),
    ExpectedState = [1,2,3,4,5,0,7,8,6],
    (   NewState == ExpectedState ->
        writeln('✓ Échange de positions correct')
    ;   writeln('✗ Échec échange de positions')
    ),
    
    writeln('--- Tests plateau terminés ---').

%% test_move_generation/0
%  Teste la génération de mouvements
test_move_generation :-
    writeln('--- Test génération de mouvements ---'),
    
    % Test avec case vide au centre
    TestState = [1,2,3,4,0,6,7,8,9],
    moves:generate_moves(TestState, Moves),
    length(Moves, NumMoves),
    (   NumMoves =:= 4 ->
        writeln('✓ 4 mouvements générés pour case vide au centre')
    ;   format('✗ Nombre incorrect de mouvements: ~w~n', [NumMoves])
    ),
    
    % Test avec case vide dans un coin
    CornerState = [0,1,2,3,4,5,6,7,8],
    moves:generate_moves(CornerState, CornerMoves),
    length(CornerMoves, NumCornerMoves),
    (   NumCornerMoves =:= 2 ->
        writeln('✓ 2 mouvements générés pour case vide dans un coin')
    ;   format('✗ Nombre incorrect de mouvements coin: ~w~n', [NumCornerMoves])
    ),
    
    writeln('--- Tests mouvements terminés ---').

%% test_heuristics/0
%  Teste les fonctions heuristiques
test_heuristics :-
    writeln('--- Test des heuristiques ---'),
    
    InitialState = [1,2,3,5,0,6,4,7,8],
    GoalState = [1,2,3,4,5,6,7,8,0],
    
    % Test heuristique tuiles mal placées
    heuristics:misplaced_tiles(InitialState, GoalState, MisplacedCount),
    (   MisplacedCount > 0 ->
        format('✓ Tuiles mal placées: ~w~n', [MisplacedCount])
    ;   writeln('✗ Échec heuristique tuiles mal placées')
    ),
    
    % Test distance de Manhattan
    heuristics:manhattan_distance(InitialState, GoalState, ManhattanDist),
    (   ManhattanDist > 0 ->
        format('✓ Distance Manhattan: ~w~n', [ManhattanDist])
    ;   writeln('✗ Échec distance Manhattan')
    ),
    
    % Test état but (doit donner 0)
    heuristics:manhattan_distance(GoalState, GoalState, ZeroDist),
    (   ZeroDist =:= 0 ->
        writeln('✓ Distance nulle pour état but')
    ;   writeln('✗ Échec distance nulle état but')
    ),
    
    writeln('--- Tests heuristiques terminés ---').

%% test_search_algorithm/0
%  Teste l'algorithme de recherche A*
test_search_algorithm :-
    writeln('--- Test algorithme A* ---'),
    
    % Test cas simple: état initial = état but
    SimpleState = [1,2,3,4,5,6,7,8,0],
    search:astar_search(SimpleState, SimpleState, manhattan, Path, Cost, Expanded),
    (   Cost =:= 0 ->
        writeln('✓ Coût 0 pour état initial = état but')
    ;   format('✗ Coût incorrect: ~w~n', [Cost])
    ),
    
    writeln('--- Tests algorithme A* terminés ---').

%% test_case_1/0
%  Premier cas de test (exemple du professeur)
test_case_1 :-
    writeln('--- CAS DE TEST 1 (Professeur) ---'),
    
    InitialState = [1,2,3,5,0,6,4,7,8],
    GoalState = [1,2,3,4,5,6,7,8,0],
    
    utils:display_state('État initial', InitialState),
    utils:display_state('État but', GoalState),
    
    get_time(StartTime),
    search:astar_search(InitialState, GoalState, manhattan, Path, Cost, Expanded),
    get_time(EndTime),
    
    (   Path \= [] ->
        utils:display_solution(Path, Cost, Expanded),
        Time is EndTime - StartTime,
        format('Temps d\'exécution: ~3f secondes~n', [Time])
    ;   writeln('✗ Échec résolution cas de test 1')
    ),
    
    writeln('--- Fin cas de test 1 ---').

%% test_case_2/0
%  Deuxième cas de test (personnalisé)
test_case_2 :-
    writeln('--- CAS DE TEST 2 (Personnalisé) ---'),
    
    % État plus complexe nécessitant plus de mouvements
    InitialState = [2,8,3,1,6,4,7,0,5],
    GoalState = [1,2,3,4,5,6,7,8,0],
    
    utils:display_state('État initial', InitialState),
    utils:display_state('État but', GoalState),
    
    get_time(StartTime),
    search:astar_search(InitialState, GoalState, manhattan, Path, Cost, Expanded),
    get_time(EndTime),
    
    (   Path \= [] ->
        utils:display_solution(Path, Cost, Expanded),
        Time is EndTime - StartTime,
        format('Temps d\'exécution: ~3f secondes~n', [Time])
    ;   writeln('✗ Échec résolution cas de test 2')
    ),
    
    writeln('--- Fin cas de test 2 ---').

%% benchmark_heuristics/2
%  Compare les performances de différentes heuristiques
benchmark_heuristics(InitialState, GoalState) :-
    writeln('--- BENCHMARK DES HEURISTIQUES ---'),
    
    % Test avec tuiles mal placées
    get_time(Start1),
    search:astar_search(InitialState, GoalState, misplaced, Path1, Cost1, Expanded1),
    get_time(End1),
    Time1 is End1 - Start1,
    format('Tuiles mal placées - Coût: ~w, Explorés: ~w, Temps: ~3fs~n', 
           [Cost1, Expanded1, Time1]),
    
    % Test avec distance Manhattan
    get_time(Start2),
    search:astar_search(InitialState, GoalState, manhattan, Path2, Cost2, Expanded2),
    get_time(End2),
    Time2 is End2 - Start2,
    format('Distance Manhattan - Coût: ~w, Explorés: ~w, Temps: ~3fs~n', 
           [Cost2, Expanded2, Time2]).

%% assert_equal/3
%  Utilitaire pour les assertions dans les tests
assert_equal(Expected, Actual, TestName) :-
    (   Expected == Actual ->
        format('✓ ~w~n', [TestName])
    ;   format('✗ ~w - Attendu: ~w, Obtenu: ~w~n', [TestName, Expected, Actual])
    ).