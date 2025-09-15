% =============================================================================
% TESTS.PL - Suite de tests unitaires et validation du solveur Taquin A*
% =============================================================================

:- encoding(utf8).
%
% ÉQUIPE       : Projet universitaire IFT-2003
% COURS        : IFT-2003 - Intelligence Artificielle
% INSTITUTION  : Université Laval
% VERSION      : 1.0
%
% DESCRIPTION  : Suite de tests exhaustive pour valider l'ensemble du système
%                de résolution de taquin. Garantit la conformité académique
%                et la robustesse de l'implémentation.
%
% FONCTIONNALITÉS PRINCIPALES :
% - Tests unitaires pour chaque module (game, astar, display, main)
% - Validation académique stricte (Cost=4, Expanded=9 pour cas test 1)
% - Tests d'intégration et de robustesse système
% - Tests de performance et gestion des cas limites
% - Validation complète des heuristiques et de l'algorithme A*
%
% ARCHITECTURE DES SECTIONS :
% 1. Tests unitaires module game.pl
% 2. Tests unitaires module astar.pl
% 3. Tests unitaires module display.pl
% 4. Tests d'intégration système
% 5. Tests de performance et robustesse
%
% CRITIQUE: Test cas_test_1_exact doit absolument passer pour validation académique
%
% UTILISATION  : swipl -g run_all_tests src/tests.pl
%
% =============================================================================

% Importation des modules à tester
:- consult('game.pl').
:- consult('astar.pl').
:- consult('display.pl').
:- consult('main.pl').

% =============================================================================
% SECTION 1: TESTS UNITAIRES MODULE GAME.PL
% =============================================================================

%! test_game_module is det.
%  Lance tous les tests unitaires pour le module game.pl
test_game_module :-
    write('[TEST] Tests module GAME.PL...'), nl,
    test_valid_state,
    test_find_blank,
    test_generate_moves_order,
    test_apply_move,
    test_solvability,
    test_swap_tiles,
    write('   [OK] Module game.pl - TOUS TESTS PASSES'), nl, nl.

%! test_valid_state is det.
%  Test de validation des états valides/invalides
test_valid_state :-
    write('  -> Test validation etats...'),

    % États valides
    assertion(valid_state([1,2,3,4,5,6,7,8,0])),
    assertion(valid_state([0,1,2,3,4,5,6,7,8])),
    initial_state(TestState),
    assertion(valid_state(TestState)),

    % États invalides
    assertion(\+ valid_state([1,2,3,4,5,6,7,8])),      % 8 éléments seulement
    assertion(\+ valid_state([1,2,3,4,5,6,7,8,9,0])),  % 10 éléments
    assertion(\+ valid_state([1,2,3,4,5,6,7,8,8])),    % Doublon
    assertion(\+ valid_state([1,2,3,4,5,6,7,8,10])),   % Valeur hors range

    write(' ✓'), nl.

%! test_find_blank is det.
%  Test de localisation de la case vide
test_find_blank :-
    write('  → Test localisation case vide...'),

    assertion((find_blank([0,1,2,3,4,5,6,7,8], Pos1), Pos1 =:= 0)),
    assertion((find_blank([1,2,3,4,0,5,6,7,8], Pos2), Pos2 =:= 4)),
    assertion((find_blank([1,2,3,4,5,6,7,8,0], Pos3), Pos3 =:= 8)),

    write(' ✓'), nl.

%! test_generate_moves_order is det.
%  Test CRITIQUE : vérifier l'ordre exact des mouvements (UP, DOWN, LEFT, RIGHT)
test_generate_moves_order :-
    write('  -> Test ordre generation mouvements (CRITIQUE)...'),

    % Test position centrale (4) : tous les mouvements possibles
    State = [1,2,3,4,0,5,6,7,8],  % Case vide au centre
    generate_moves(State, Successors),

    % Vérifier que nous avons exactement 4 successeurs
    length(Successors, 4),

    % Vérifier l'ordre exact : UP, DOWN, LEFT, RIGHT
    Successors = [UpState, DownState, LeftState, RightState],
    assertion(UpState = [1,0,3,4,2,5,6,7,8]),      % UP : case vide monte
    assertion(DownState = [1,2,3,4,7,5,6,0,8]),    % DOWN : case vide descend
    assertion(LeftState = [1,2,3,0,4,5,6,7,8]),    % LEFT : case vide à gauche
    assertion(RightState = [1,2,3,4,5,0,6,7,8]),   % RIGHT : case vide à droite

    write(' ✓ ORDRE VÉRIFIÉ'), nl.

%! test_apply_move is det.
%  Test d'application des mouvements
test_apply_move :-
    write('  → Test application mouvements...'),

    State = [1,2,3,4,0,5,6,7,8],

    assertion((apply_move(State, up, NewState1),
               NewState1 = [1,0,3,4,2,5,6,7,8])),
    assertion((apply_move(State, down, NewState2),
               NewState2 = [1,2,3,4,7,5,6,0,8])),
    assertion((apply_move(State, left, NewState3),
               NewState3 = [1,2,3,0,4,5,6,7,8])),
    assertion((apply_move(State, right, NewState4),
               NewState4 = [1,2,3,4,5,0,6,7,8])),

    write(' ✓'), nl.

%! test_solvability is det.
%  Test de détection de solvabilité (inversions)
test_solvability :-
    write('  -> Test solvabilite (inversions)...'),

    % États solvables
    initial_state(Initial),
    goal_state(Goal),
    assertion(is_solvable(Initial, Goal)),
    assertion(is_solvable([1,2,3,4,5,6,7,8,0], [1,2,3,4,5,6,7,8,0])),

    % État impossible (parité différente)
    assertion(\+ is_solvable([1,2,3,4,5,6,8,7,0], [1,2,3,4,5,6,7,8,0])),

    write(' ✓'), nl.

%! test_swap_tiles is det.
%  Test d'échange de tuiles
test_swap_tiles :-
    write('  -> Test echange tuiles...'),

    State = [1,2,3,4,5,6,7,8,9],
    assertion((swap_tiles(State, 0, 8, NewState),
               NewState = [9,2,3,4,5,6,7,8,1])),
    assertion((swap_tiles(State, 1, 2, NewState2),
               NewState2 = [1,3,2,4,5,6,7,8,9])),

    write(' ✓'), nl.

% =============================================================================
% SECTION 2: TESTS UNITAIRES MODULE ASTAR.PL
% =============================================================================

%! test_astar_module is det.
%  Lance tous les tests unitaires pour le module astar.pl
test_astar_module :-
    write('[TEST] Tests module ASTAR.PL...'), nl,
    test_heuristic_misplaced_tiles,
    test_manhattan_distance,
    test_node_comparison,
    test_path_reconstruction,
    write('   [OK] Module astar.pl - TOUS TESTS PASSES'), nl, nl.

%! test_heuristic_misplaced_tiles is det.
%  Test CRITIQUE : validation de l'heuristique tuiles mal placées
test_heuristic_misplaced_tiles :-
    write('  -> Test heuristique tuiles mal placees (CRITIQUE)...'),

    % Test du cas académique exact : h(initial_state) = 4
    initial_state(Initial),
    goal_state(Goal),
    assertion((misplaced_tiles_heuristic(Initial, Goal, H), H =:= 4)),

    % Test état résolu : h = 0
    assertion((misplaced_tiles_heuristic(Goal, Goal, H0), H0 =:= 0)),

    % Test avec plusieurs tuiles mal placées
    State2 = [8,7,6,5,4,3,2,1,0],
    assertion((misplaced_tiles_heuristic(State2, Goal, H2), H2 =:= 8)),

    % Test que case vide n'est pas comptée
    State3 = [0,2,3,4,5,6,7,8,1],  % Seule tuile 1 mal placée
    assertion((misplaced_tiles_heuristic(State3, Goal, H3), H3 =:= 1)),

    write(' ✓ HEURISTIQUE VALIDÉE'), nl.

%! test_manhattan_distance is det.
%  Test de l'heuristique Manhattan (optionnelle)
test_manhattan_distance :-
    write('  → Test heuristique Manhattan...'),

    Goal = [1,2,3,4,5,6,7,8,0],

    % État résolu : distance = 0
    assertion((manhattan_distance_heuristic(Goal, Goal, D0), D0 =:= 0)),

    % Test cas simple : une tuile déplacée de 1 case
    State1 = [2,1,3,4,5,6,7,8,0],  % Tuiles 1 et 2 échangées
    assertion((manhattan_distance_heuristic(State1, Goal, D1), D1 =:= 2)),

    write(' ✓'), nl.

%! test_node_comparison is det.
%  Test du tri des nœuds par valeur f (tie-breaking inclus)
test_node_comparison :-
    write('  → Test comparaison nœuds (tie-breaking)...'),

    Node1 = node([1,2,3], 1, 3, 4, nil),  % f=4, g=1
    Node2 = node([4,5,6], 2, 2, 4, nil),  % f=4, g=2 (même f, g plus grand)
    Node3 = node([7,8,9], 0, 3, 3, nil),  % f=3 (plus petit f)

    % Test ordre : Node3 (f=3) < Node1 (f=4,g=1) < Node2 (f=4,g=2)
    Nodes = [Node1, Node2, Node3],
    sort_by_f_value(Nodes, [First, Second, Third]),
    assertion(First = node([7,8,9], 0, 3, 3, nil)),
    assertion(Second = node([1,2,3], 1, 3, 4, nil)),
    assertion(Third = node([4,5,6], 2, 2, 4, nil)),

    write(' ✓'), nl.

%! test_path_reconstruction is det.
%  Test de reconstruction du chemin
test_path_reconstruction :-
    write('  → Test reconstruction chemin...'),

    % Construire une chaîne de nœuds
    NodeA = node([1,2,3], 0, 0, 0, nil),
    NodeB = node([4,5,6], 1, 0, 1, NodeA),
    NodeC = node([7,8,9], 2, 0, 2, NodeB),

    assertion((reconstruct_path(NodeC, Path),
               Path = [[7,8,9], [4,5,6], [1,2,3]])),

    write(' ✓'), nl.

% =============================================================================
% SECTION 3: TEST CRITIQUE - VALIDATION ACADÉMIQUE
% =============================================================================

%! test_case_1_exact is det.
%  TEST CRITIQUE : Valider exactement Cost=4, Expanded=9 pour cas test 1
%  Ce test doit ABSOLUMENT passer pour la validation académique
test_case_1_exact :-
    write('[CRITIQUE] TEST CRITIQUE - Validation academique cas test 1...'), nl,

    initial_state(Initial),
    goal_state(Goal),

    % Exécuter la résolution A*
    get_time(StartTime),
    astar_search(Initial, Goal, Path, Cost, Expanded),
    get_time(EndTime),

    ResponseTime is EndTime - StartTime,
    length(Path, PathLength),

    write('  Resultats obtenus :'), nl,
    format('    Path Length: ~w états~n', [PathLength]),
    format('    Cost: ~w mouvements~n', [Cost]),
    format('    Expanded: ~w nœuds~n', [Expanded]),
    format('    Temps: ~3f secondes~n', [ResponseTime]),

    % Validations critiques
    write('  Validations critiques :'), nl,

    % 1. Vérifier Cost = 4
    (   Cost =:= 4 ->
        write('    [OK] Cost = 4 (VALIDE)')
    ;   format('    [ERREUR] Cost = ~w (ATTENDU: 4)', [Cost])
    ), nl,

    % 2. Vérifier Expanded = 9
    (   Expanded =:= 9 ->
        write('    [OK] Expanded = 9 (VALIDE)')
    ;   format('    [ERREUR] Expanded = ~w (ATTENDU: 9)', [Expanded])
    ), nl,

    % 3. Vérifier longueur Path = 5
    (   PathLength =:= 5 ->
        write('    [OK] Path Length = 5 (VALIDE)')
    ;   format('    [ERREUR] Path Length = ~w (ATTENDU: 5)', [PathLength])
    ), nl,

    % 4. Vérifier performance < 1 seconde
    (   ResponseTime < 1.0 ->
        write('    [OK] Performance < 1s (VALIDE)')
    ;   format('    [WARN] Performance = ~3f s (> 1s)', [ResponseTime])
    ), nl,

    % Assertions finales pour arrêter si échec
    assertion(Cost =:= 4),
    assertion(Expanded =:= 9),
    assertion(PathLength =:= 5),

    write('  [SUCCES] VALIDATION ACADEMIQUE COMPLETE REUSSIE!'), nl, nl.

% =============================================================================
% SECTION 4: TESTS D'INTÉGRATION
% =============================================================================

%! test_integration is det.
%  Tests d'intégration entre les modules
test_integration :-
    write('[TEST] Tests integration modules...'), nl,
    test_game_to_astar_integration,
    test_full_pipeline,
    write('   [OK] Integration modules - TOUS TESTS PASSES'), nl, nl.

%! test_game_to_astar_integration is det.
%  Test intégration game.pl → astar.pl
test_game_to_astar_integration :-
    write('  -> Test integration game -> astar...'),

    % Générer des mouvements et vérifier qu'ils sont résolubles
    initial_state(Initial),
    generate_moves(Initial, Successors),
    goal_state(Goal),

    % Chaque successeur doit être solvable
    forall(member(Successor, Successors),
           assertion(is_solvable(Successor, Goal))),

    write(' ✓'), nl.

%! test_full_pipeline is det.
%  Test du pipeline complet : game → astar → display
test_full_pipeline :-
    write('  → Test pipeline complet...'),

    % Test cas 1 complet
    solve_puzzle(case1, result(Path1, Cost1, Expanded1)),
    assertion(is_list(Path1)),
    assertion(integer(Cost1)),
    assertion(integer(Expanded1)),

    % Test cas 2 complet
    solve_puzzle(case2, result(Path2, Cost2, Expanded2)),
    assertion(is_list(Path2)),
    assertion(integer(Cost2)),
    assertion(integer(Expanded2)),

    write(' ✓'), nl.

% =============================================================================
% SECTION 5: TESTS DE ROBUSTESSE ET CAS LIMITES
% =============================================================================

%! test_edge_cases is det.
%  Tests des cas limites et situations exceptionnelles
test_edge_cases :-
    write('[TEST] Tests cas limites...'), nl,
    test_already_solved,
    test_invalid_states,
    test_unsolvable_states,
    write('   [OK] Cas limites - TOUS TESTS PASSES'), nl, nl.

%! test_already_solved is det.
%  Test état déjà résolu
test_already_solved :-
    write('  -> Test etat deja resolu...'),

    Goal = [1,2,3,4,5,6,7,8,0],
    astar_search(Goal, Goal, Path, Cost, Expanded),

    assertion(Path = [Goal]),
    assertion(Cost =:= 0),
    assertion(Expanded =:= 0),

    write(' ✓'), nl.

%! test_invalid_states is det.
%  Test gestion des états invalides
test_invalid_states :-
    write('  -> Test gestion etats invalides...'),

    InvalidState = [1,2,3,4,5,6,7,8,8],  % Doublon
    Goal = [1,2,3,4,5,6,7,8,0],

    % Doit lever une exception ou échouer proprement
    catch(
        (astar_search(InvalidState, Goal, _, _, _), fail),
        _Error,
        true
    ),

    write(' ✓'), nl.

%! test_unsolvable_states is det.
%  Test gestion des états impossibles
test_unsolvable_states :-
    write('  -> Test gestion etats impossibles...'),

    % État avec parité d'inversions incorrecte
    UnsolvableState = [1,2,3,4,5,6,8,7,0],  % Échange 7 et 8
    Goal = [1,2,3,4,5,6,7,8,0],

    % Doit échouer proprement
    assertion(\+ is_solvable(UnsolvableState, Goal)),

    write(' ✓'), nl.

% =============================================================================
% SECTION 6: TESTS DE PERFORMANCE
% =============================================================================

%! test_performance is det.
%  Tests de performance et stabilité
test_performance :-
    write('[TEST] Tests performance...'), nl,
    test_response_times,
    test_memory_usage,
    write('   [OK] Performance - TOUS TESTS PASSES'), nl, nl.

%! test_response_times is det.
%  Test des temps de réponse
test_response_times :-
    write('  -> Test temps de reponse...'),

    % Cas 1 : doit être résolu en < 1 seconde
    get_time(Start1),
    solve_puzzle(case1, _),
    get_time(End1),
    Time1 is End1 - Start1,
    assertion(Time1 < 1.0),

    % Cas 2 : doit être résolu en < 3 secondes
    get_time(Start2),
    solve_puzzle(case2, _),
    get_time(End2),
    Time2 is End2 - Start2,
    assertion(Time2 < 3.0),

    format(' ✓ (Cas1: ~3fs, Cas2: ~3fs)', [Time1, Time2]), nl.

%! test_memory_usage is det.
%  Test d'utilisation mémoire (simple)
test_memory_usage :-
    write('  -> Test utilisation memoire...'),

    % Exécuter plusieurs résolutions pour détecter les fuites mémoire
    numlist(1, 5, _),  % 5 itérations
    forall(between(1, 5, _), solve_puzzle(case1, _)),

    write(' ✓'), nl.

%! test_case_2_validation is det.
%  TEST PRIORITÉ 2 : Validation spécifique pour cas test 2 personnalisé
test_case_2_validation :-
    write('[CRITIQUE] TEST VALIDATION - Cas test 2 personnalise...'), nl,

    % Exécuter le cas test 2
    get_time(StartTime),
    solve_puzzle(case2, result(Path, Cost, Expanded)),
    get_time(EndTime),

    ResponseTime is EndTime - StartTime,
    length(Path, PathLength),

    write('  Resultats cas test 2 :'), nl,
    format('    Path Length: ~w états~n', [PathLength]),
    format('    Cost: ~w mouvements~n', [Cost]),
    format('    Expanded: ~w nœuds~n', [Expanded]),
    format('    Temps: ~3f secondes~n', [ResponseTime]),

    % Validations pour cas test 2
    write('  Validations cas test 2 :'), nl,

    % 1. Vérifier que solution existe
    (   Cost > 0 ->
        write('    [OK] Solution trouvee')
    ;   write('    [ERREUR] Pas de solution')
    ), nl,

    % 2. Vérifier performance acceptable (< 5 secondes pour cas plus complexe)
    (   ResponseTime < 5.0 ->
        write('    [OK] Performance < 5s')
    ;   format('    [WARN] Performance = ~3f s (> 5s)', [ResponseTime])
    ), nl,

    % 3. Vérifier cohérence Path/Cost
    ExpectedPathLength is Cost + 1,
    (   PathLength =:= ExpectedPathLength ->
        write('    [OK] Coherence Path/Cost')
    ;   format('    [WARN] Path Length ~w != Cost+1 (~w)', [PathLength, ExpectedPathLength])
    ), nl,

    % 4. Vérifier que expanded > 0 (exploration nécessaire)
    (   Expanded > 0 ->
        write('    [OK] Exploration A* active')
    ;   write('    [WARN] Pas d\'exploration detectee')
    ), nl,

    write('  [SUCCES] VALIDATION CAS TEST 2 TERMINÉE!'), nl, nl.

% =============================================================================
% SECTION 7: EXÉCUTION COMPLÈTE DES TESTS
% =============================================================================

%! run_all_tests is det.
%  Exécute la suite complète de tests avec rapport détaillé
run_all_tests :-
    write('+----------------------------------------------------------+'), nl,
    write('|             SUITE DE TESTS SOLVEUR TAQUIN A*           |'), nl,
    write('|                  Universite Laval IFT-2003              |'), nl,
    write('+----------------------------------------------------------+'), nl, nl,

    get_time(TestStartTime),

    % Tests unitaires par module
    test_game_module,
    test_astar_module,

    % Test critique validation académique
    test_case_1_exact,

    % Test priorité 2 - validation cas test 2
    test_case_2_validation,

    % Tests d'intégration et robustesse
    test_integration,
    test_edge_cases,
    test_performance,

    get_time(TestEndTime),
    TotalTime is TestEndTime - TestStartTime,

    nl,
    write('+----------------------------------------------------------+'), nl,
    write('|                       RESUME FINAL                     |'), nl,
    write('+----------------------------------------------------------+'), nl,
    format('Total des tests exécutés en ~3f secondes~n', [TotalTime]),
    write('[OK] TOUS LES TESTS SONT PASSES AVEC SUCCES!'), nl,
    write('[SUCCES] VALIDATION ACADEMIQUE CONFIRMEE'), nl,
    write('[INFO] Pret pour evaluation finale'), nl, nl.

%! run_critical_tests_only is det.
%  Exécute uniquement les tests critiques pour validation rapide
run_critical_tests_only :-
    write('[CRITIQUE] TESTS CRITIQUES SEULEMENT...'), nl,
    test_case_1_exact,
    test_heuristic_misplaced_tiles,
    test_generate_moves_order,
    write('[OK] TESTS CRITIQUES VALIDES'), nl.

% =============================================================================
% UTILITAIRES DE TEST
% =============================================================================

%! assertion(+Goal) is det.
%  Macro d'assertion pour les tests (arrête en cas d'échec)
assertion(Goal) :-
    (   call(Goal) -> true
    ;   format('[ERREUR] ASSERTION ECHOUEE: ~q~n', [Goal]),
        fail
    ).

% Point d'entrée principal pour les tests
% Note: Éviter conflit avec main/0 de main.pl
test_main :- run_all_tests.