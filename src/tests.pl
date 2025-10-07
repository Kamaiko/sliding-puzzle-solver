% =============================================================================
% TESTS.PL - Suite de tests pour validation du solveur Taquin A*
% =============================================================================

:- encoding(utf8).

% Importation des modules à tester
% Note: main.pl consulte automatiquement game.pl, astar.pl et display.pl
:- consult('main.pl').

% =============================================================================
% SECTION 1: TESTS MODULE GAME.PL
% =============================================================================

%! test_game_module is det.
%  Tests unitaires pour le module game.pl (logique métier)
test_game_module :-
    write('[TEST] Module GAME.PL...'), nl,
    test_find_blank,
    test_generate_moves,
    test_valid_state,
    test_solvability,
    write('   [OK] game.pl valide'), nl, nl.

%! test_find_blank is det.
%  Vérifie la localisation de la case vide
test_find_blank :-
    write('  -> Localisation case vide...'),
    assertion((find_blank([0,1,2,3,4,5,6,7,8], Pos1), Pos1 =:= 0)),
    assertion((find_blank([1,2,3,4,0,5,6,7,8], Pos2), Pos2 =:= 4)),
    assertion((find_blank([1,2,3,4,5,6,7,8,0], Pos3), Pos3 =:= 8)),
    write(' OK'), nl.

%! test_generate_moves is det.
%  Vérifie la génération des mouvements valides
test_generate_moves :-
    write('  -> Generation mouvements valides...'),
    State = [1,2,3,4,0,5,6,7,8],  % Blanc au centre
    generate_moves(State, Successors),
    length(Successors, Len),
    assertion(Len =:= 4),  % 4 mouvements possibles au centre
    write(' OK'), nl.

%! test_valid_state is det.
%  Vérifie la validation des états
test_valid_state :-
    write('  -> Validation etats...'),
    assertion(valid_state([1,2,3,4,5,6,7,8,0])),
    assertion(\+ valid_state([1,2,3,4,5,6,7,8])),      % 8 éléments
    assertion(\+ valid_state([1,2,3,4,5,6,7,8,8])),    % Doublon
    write(' OK'), nl.

%! test_solvability is det.
%  Vérifie la détection de solvabilité (inversions)
test_solvability :-
    write('  -> Detection solvabilite...'),
    Goal = [1,2,3,4,5,6,7,8,0],
    assertion(is_solvable([1,2,3,5,0,6,4,7,8], Goal)),  % Solvable
    assertion(\+ is_solvable([1,2,3,4,5,6,8,7,0], Goal)),  % Impossible
    write(' OK'), nl.

% =============================================================================
% SECTION 2: TESTS MODULE ASTAR.PL
% =============================================================================

%! test_astar_module is det.
%  Tests unitaires pour le module astar.pl (algorithme A*)
test_astar_module :-
    write('[TEST] Module ASTAR.PL...'), nl,
    test_manhattan_heuristic,
    test_node_creation,
    test_node_sorting,
    test_path_reconstruction,
    write('   [OK] astar.pl valide'), nl, nl.

%! test_manhattan_heuristic is det.
%  Vérifie l'heuristique distance Manhattan
test_manhattan_heuristic :-
    write('  -> Heuristique Manhattan...'),
    Goal = [1,2,3,4,5,6,7,8,0],

    % État résolu : distance = 0
    assertion((manhattan_distance_heuristic(Goal, Goal, H0), H0 =:= 0)),

    % État initial case 1 : distance connue
    assertion((manhattan_distance_heuristic([1,2,3,5,0,6,4,7,8], Goal, H1), H1 =:= 4)),

    % Autre configuration
    assertion((manhattan_distance_heuristic([1,3,6,5,2,8,4,0,7], Goal, H2), H2 > 0)),

    write(' OK'), nl.

%! test_node_creation is det.
%  Vérifie la création de nœuds A*
test_node_creation :-
    write('  -> Creation noeuds A*...'),
    State = [1,2,3,4,5,6,7,8,0],
    create_node(State, 5, 3, nil, Node),
    node_state(Node, State),
    node_g_cost(Node, G),
    node_h_cost(Node, H),
    node_f_cost(Node, F),
    assertion(G =:= 5),
    assertion(H =:= 3),
    assertion(F =:= 8),  % f = g + h
    write(' OK'), nl.

%! test_node_sorting is det.
%  Vérifie le tri des nœuds par f-value (tie-breaking sur g)
test_node_sorting :-
    write('  -> Tri noeuds (f-value + tie-breaking)...'),
    Node1 = node([1], 2, 2, 4, nil),  % f=4, g=2
    Node2 = node([2], 1, 3, 4, nil),  % f=4, g=1 (meilleur g)
    Node3 = node([3], 0, 2, 2, nil),  % f=2 (meilleur f)

    sort_open_list_by_f_value([Node1, Node2, Node3], Sorted),
    Sorted = [First, Second, _Third],
    node_f_cost(First, F1),
    node_f_cost(Second, F2),
    node_g_cost(Second, G2),

    assertion(F1 =:= 2),  % Node3 en premier (f=2)
    assertion(F2 =:= 4),  % Node2 en second (f=4, g=1)
    assertion(G2 =:= 1),  % Vérifie tie-breaking
    write(' OK'), nl.

%! test_path_reconstruction is det.
%  Vérifie la reconstruction du chemin solution
test_path_reconstruction :-
    write('  -> Reconstruction chemin...'),
    NodeA = node([1,2,3], 0, 0, 0, nil),
    NodeB = node([4,5,6], 1, 0, 1, NodeA),
    NodeC = node([7,8,9], 2, 0, 2, NodeB),

    reconstruct_solution_path(NodeC, Path),
    assertion(Path = [[1,2,3], [4,5,6], [7,8,9]]),
    write(' OK'), nl.

% =============================================================================
% SECTION 3: TESTS MODULE DISPLAY.PL
% =============================================================================

%! test_display_module is det.
%  Tests unitaires pour le module display.pl (affichage)
test_display_module :-
    write('[TEST] Module DISPLAY.PL...'), nl,
    test_display_state,
    test_format_tile,
    write('   [OK] display.pl valide'), nl, nl.

%! test_display_state is det.
%  Vérifie l'affichage des états (ne doit pas crasher)
test_display_state :-
    write('  -> Affichage etats...'),
    State = [1,2,3,4,5,6,7,8,0],
    % Test que display_state s'exécute sans erreur
    catch(
        (with_output_to(atom(_), display_state('Test', State)), true),
        _,
        fail
    ),
    write(' OK'), nl.

%! test_format_tile is det.
%  Vérifie le formatage des tuiles
test_format_tile :-
    write('  -> Formatage tuiles...'),
    assertion((format_tile(0, Blank), Blank = '#')),
    assertion((format_tile(1, T1), T1 =:= 1)),
    assertion((format_tile(8, T8), T8 =:= 8)),
    write(' OK'), nl.

% =============================================================================
% SECTION 4: TESTS MODULE MAIN.PL
% =============================================================================

%! test_main_module is det.
%  Tests unitaires pour le module main.pl (orchestration)
test_main_module :-
    write('[TEST] Module MAIN.PL...'), nl,
    test_solve_puzzle_case1,
    test_solve_puzzle_case2,
    write('   [OK] main.pl valide'), nl, nl.

%! test_solve_puzzle_case1 is det.
%  Vérifie la résolution du cas test 1
test_solve_puzzle_case1 :-
    write('  -> Resolution cas test 1...'),
    solve_puzzle(case1, result(Path, Cost, Expanded)),
    assertion(is_list(Path)),
    assertion(Cost =:= 4),      % Solution optimale connue
    assertion(Expanded =:= 12), % Nœuds explorés attendus
    write(' OK'), nl.

%! test_solve_puzzle_case2 is det.
%  Vérifie la résolution du cas test 2
test_solve_puzzle_case2 :-
    write('  -> Resolution cas test 2...'),
    solve_puzzle(case2, result(Path, Cost, Expanded)),
    assertion(is_list(Path)),
    assertion(Cost =:= 9),      % Solution optimale
    assertion(Expanded =:= 25), % Nœuds explorés attendus
    write(' OK'), nl.

% =============================================================================
% SECTION 5: TESTS D'INTEGRATION
% =============================================================================

%! test_integration is det.
%  Tests d'intégration entre modules
test_integration :-
    write('[TEST] Integration modules...'), nl,
    test_end_to_end_case1,
    test_end_to_end_case2,
    write('   [OK] Integration valide'), nl, nl.

%! test_end_to_end_case1 is det.
%  Test end-to-end complet pour cas 1
test_end_to_end_case1 :-
    write('  -> Pipeline complet cas 1...'),
    initial_state(Initial),
    goal_state(Goal),

    get_time(Start),
    astar_search(Initial, Goal, Path, Cost, Expanded),
    get_time(End),

    Time is End - Start,

    % Validations
    assertion(Cost =:= 4),
    assertion(Expanded =:= 12),
    assertion(Time < 1.0),  % Performance < 1s

    % Vérifier cohérence path/cost
    length(Path, PathLen),
    assertion(PathLen =:= Cost + 1),

    write(' OK'), nl.

%! test_end_to_end_case2 is det.
%  Test end-to-end complet pour cas 2
test_end_to_end_case2 :-
    write('  -> Pipeline complet cas 2...'),
    custom_initial_state(Initial),
    custom_goal_state(Goal),

    get_time(Start),
    astar_search(Initial, Goal, Path, Cost, Expanded),
    get_time(End),

    Time is End - Start,

    % Validations
    assertion(Cost =:= 9),
    assertion(Expanded =:= 25),
    assertion(Time < 3.0),  % Performance < 3s

    % Vérifier cohérence path/cost
    length(Path, PathLen),
    assertion(PathLen =:= Cost + 1),

    write(' OK'), nl.

% =============================================================================
% SECTION 6: EXECUTION SUITE DE TESTS
% =============================================================================

%! run_all_tests is det.
%  Exécute la suite complète de tests
run_all_tests :-
    write('================================================================'), nl,
    write('         SUITE DE TESTS - SOLVEUR TAQUIN A*                    '), nl,
    write('================================================================'), nl, nl,

    get_time(StartTime),

    % Tests unitaires par module
    test_game_module,
    test_astar_module,
    test_display_module,
    test_main_module,

    % Tests d'intégration
    test_integration,

    get_time(EndTime),
    TotalTime is EndTime - StartTime,

    nl,
    write('================================================================'), nl,
    write('                        RESUME                                  '), nl,
    write('================================================================'), nl,
    format('Temps total: ~3f secondes~n', [TotalTime]),
    write('[OK] TOUS LES TESTS SONT PASSES'), nl,
    nl,
    halt(0).

% =============================================================================
% UTILITAIRES
% =============================================================================

%! assertion(+Goal) is det.
%  Macro d'assertion simple (fail si Goal échoue)
assertion(Goal) :-
    (   call(Goal) -> true
    ;   format('[ERREUR] Assertion echouee: ~q~n', [Goal]),
        fail
    ).

% Point d'entrée
% Note: Utiliser 'swipl -g run_all_tests src/tests.pl' pour exécuter les tests
