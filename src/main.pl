% =============================================================================
% MAIN.PL - Point d'entrée principal et orchestration du solveur de Taquin
% =============================================================================

:- encoding(utf8).
%
% ÉQUIPE       : Projet universitaire IFT-2003
% COURS        : IFT-2003 - Intelligence Artificielle
% INSTITUTION  : Université Laval
% VERSION      : 1.0
%
% DESCRIPTION  : Point d'entrée principal du système de résolution de taquin
%                utilisant l'algorithme A*. Ce module orchestre l'ensemble
%                du programme et fournit une interface utilisateur professionnelle.
%
% FONCTIONNALITÉS PRINCIPALES :
% - Point d'entrée principal et initialisation système
% - Menu interactif CLI avec validation robuste des entrées
% - Gestion de cas de test académiques et personnalisés
% - Mesure précise des temps de réponse de l'IA
% - Gestion d'erreurs exhaustive avec messages informatifs
% - Interface utilisateur fluide et professionnelle
%
% ARCHITECTURE DES SECTIONS :
% 1. Points d'entrée et initialisation
% 2. Menu principal et navigation
% 3. Gestion des choix utilisateur
% 4. Exécution des cas de test
% 5. Utilitaires de développement et débogage
% 6. Modes d'utilisation avancés
%
% UTILISATION  : swipl -g main src/main.pl
%               ou depuis l'interpréteur : ?- main.
%
% =============================================================================

:- consult(game).
:- consult(astar).
:- consult(display).

% =============================================================================
% SECTION 1: POINTS D'ENTRÉE ET INITIALISATION
% =============================================================================

%! Point d'entrée principal pour SWI-Prolog
%  Initialization désactivée pour éviter conflit avec tests
%  Utiliser main/0 ou main([]) pour lancer manuellement

%! main(+Args:list) is det.
%  Point d'entrée principal avec gestion des arguments
%  Lance la bannière d'accueil et démarre le menu principal
%  @param Args Arguments de ligne de commande (ignorés pour ce programme)
main(_) :-
    setup_environment,
    display_banner,
    main_menu.

%! main is det.
%  Point d'entrée alternatif sans arguments
%  Utilisé pour lancement manuel depuis l'interpréteur Prolog
main :-
    main([]).

%! setup_environment is det.
%  Configure l'environnement d'exécution
%  Initialise les paramètres et vérifications préliminaires
setup_environment :-
    % Vérifier que les modules sont bien chargés
    (   current_predicate(valid_state/1) -> true
    ;   throw(error('Module game.pl non chargé', setup))
    ),
    (   current_predicate(astar_search/5) -> true
    ;   throw(error('Module astar.pl non chargé', setup))
    ),
    (   current_predicate(display_banner/0) -> true
    ;   throw(error('Module display.pl non chargé', setup))
    ).

% =============================================================================
% SECTION 2: MENU PRINCIPAL ET NAVIGATION
% =============================================================================

%! main_menu is det.
%  Boucle principale du menu interactif
%  Affiche le menu, lit le choix utilisateur et traite la demande
main_menu :-
    display_menu,
    read_choice(Choice),
    handle_choice(Choice).

%! read_choice(-Choice:integer) is det.
%  Lit et valide le choix utilisateur depuis l'entrée standard
%  Gère les erreurs de saisie avec redemande automatique et EOF
%  @param Choice Choix valide de l'utilisateur (1-3)
read_choice(Choice) :-
    flush_output,
    catch(
        (read(Input),
         (   Input == end_of_file ->
             % Gestion EOF : quitter proprement
             (write('Programme interrompu.'), nl, halt)
         ;   integer(Input),
             Input >= 1,
             Input =< 3 ->
             Choice = Input
         ;   fail  % Input invalide, aller au cas d'erreur
         )),
        _Error,
        fail
    ),
    !.  % Couper si succès

read_choice(Choice) :-
    % Gestion des erreurs de saisie avec limite pour éviter boucle infinie
    write('[ERREUR] Entree invalide. Veuillez entrer un nombre entre 1 et 3.'), nl,
    % Nettoyer le buffer d'entrée si nécessaire
    catch(
        (repeat,
         read(Next),
         (Next == end_of_file -> halt ; true),
         !),
        _,
        true
    ),
    read_choice(Choice).

%! skip_to_newline is det.
%  Nettoie le buffer d'entrée jusqu'à la prochaine ligne
%  Utilisé pour récupérer des erreurs de saisie
skip_to_newline :-
    get_char(C),
    (   C = '\n' -> true
    ;   C = end_of_file -> true
    ;   skip_to_newline
    ).

% =============================================================================
% SECTION 3: GESTION DES CHOIX UTILISATEUR
% =============================================================================

%! handle_choice(+Choice:integer) is det.
%  Traite le choix utilisateur du menu principal
%  Exécute l'action correspondante avec gestion d'erreurs
%  @param Choice Choix de l'utilisateur (1=cas test 1, 2=cas test 2, 3=quitter)

% Cas test 1 : Exemple professeur (validation académique)
handle_choice(1) :-
    nl,
    write('+------------------------------------------------+'), nl,
    write('|       CAS TEST 1 : EXEMPLE PROFESSEUR         |'), nl,
    write('|   Configuration: [1,2,3,5,0,6,4,7,8]         |'), nl,
    write('|   Objectif: [1,2,3,4,5,6,7,8,0]               |'), nl,
    write('|   Validation: Cost=4, Expanded=9              |'), nl,
    write('+------------------------------------------------+'), nl,

    execute_test_case(case1),
    wait_for_continue,
    main_menu.

% Cas test 2 : Exemple personnalisé (configuration étendue)
handle_choice(2) :-
    nl,
    write('+------------------------------------------------+'), nl,
    write('|      CAS TEST 2 : EXEMPLE PERSONNALISE        |'), nl,
    write('|   Configuration plus complexe (6+ mouvements) |'), nl,
    write('|   Demonstration etendue des capacites A*      |'), nl,
    write('+------------------------------------------------+'), nl,

    execute_test_case(case2),
    wait_for_continue,
    main_menu.

% Quitter le programme
handle_choice(3) :-
    nl,
    write('+----------------------------------------------+'), nl,
    write('|            MERCI ET AU REVOIR!               |'), nl,
    write('|        Solveur Taquin A* - Mission accomplie |'), nl,
    write('|        Universite Laval - IFT-2003          |'), nl,
    write('+----------------------------------------------+'), nl,
    write('[SYSTEME] Fermeture du programme...'), nl,
    flush_output,
    halt(0).

% Gestion des choix invalides (normalement interceptés par read_choice)
handle_choice(InvalidChoice) :-
    nl,
    format('[ERREUR] Choix invalide: ~w~n', [InvalidChoice]),
    write('Veuillez choisir entre 1, 2 ou 3.'), nl,
    main_menu.

% =============================================================================
% SECTION 4: EXÉCUTION DES CAS DE TEST
% =============================================================================

%! execute_test_case(+TestCase:atom) is det.
%  Exécute un cas de test avec mesure de performance et gestion d'erreurs
%  @param TestCase Identifiant du cas (case1 | case2)
execute_test_case(TestCase) :-
    % Afficher message de réflexion IA
    display_thinking_message,

    % Mesurer le temps de calcul avec précision
    get_time(StartTime),

    catch(
        % Tentative de résolution avec A*
        (solve_puzzle(TestCase, result(Path, Cost, Expanded)),
         get_time(EndTime),
         ResponseTime is EndTime - StartTime,

         % Afficher la solution trouvée
         display_success_message,
         display_solution(Path, Cost, Expanded, ResponseTime)),

        % Gestion des erreurs spécifiques
        Error,
        handle_execution_error(Error)
    ).

%! handle_execution_error(+Error:compound) is det.
%  Gère les erreurs durant l'exécution des cas de test
%  Affiche des messages d'erreur informatifs selon le type d'erreur
%  @param Error Structure d'erreur Prolog
handle_execution_error(error(timeout, _)) :-
    !,
    display_error(timeout, 'Temps de calcul dépassé').

handle_execution_error(error(invalid_state, Details)) :-
    !,
    display_error(invalid_state, Details).

handle_execution_error(error(unsolvable, Details)) :-
    !,
    display_error(unsolvable, Details).

handle_execution_error(Error) :-
    % Erreur générique non prévue
    nl,
    write('+--------------------------------------+'), nl,
    write('|             ERREUR INATTENDUE        |'), nl,
    write('+--------------------------------------+'), nl,
    format('Erreur système: ~w~n', [Error]),
    write('Veuillez signaler ce probleme aux developpeurs.'), nl, nl.

%! wait_for_continue is det.
%  Attend une interaction utilisateur pour continuer
%  Permet à l'utilisateur de lire les résultats avant de revenir au menu
wait_for_continue :-
    nl,
    write('[INFO] Appuyez sur Entree pour revenir au menu...'),
    flush_output,
    get_char(_),  % Attendre n'importe quel caractère
    nl.

% =============================================================================
% SECTION 5: UTILITAIRES DE DÉVELOPPEMENT ET DÉBOGAGE
% =============================================================================

%! run_performance_tests is det.
%  Exécute une série de tests de performance sur les deux cas
%  Utilitaire pour valider les performances et la stabilité
run_performance_tests :-
    write('🔬 TESTS DE PERFORMANCE'), nl,
    write('========================'), nl, nl,

    % Test du cas 1 (5 itérations)
    write('Test cas 1 (5 iterations):'), nl,
    run_multiple_tests(case1, 5, Times1),
    calculate_statistics(Times1, Mean1, Min1, Max1),
    format('  Moyenne: ~3f s | Min: ~3f s | Max: ~3f s~n', [Mean1, Min1, Max1]),

    nl,

    % Test du cas 2 (3 itérations)
    write('Test cas 2 (3 iterations):'), nl,
    run_multiple_tests(case2, 3, Times2),
    calculate_statistics(Times2, Mean2, Min2, Max2),
    format('  Moyenne: ~3f s | Min: ~3f s | Max: ~3f s~n', [Mean2, Min2, Max2]),

    nl.

%! run_multiple_tests(+TestCase:atom, +Count:integer, -Times:list) is det.
%  Exécute un cas de test plusieurs fois et collecte les temps
%  @param TestCase Cas à tester
%  @param Count Nombre d'itérations
%  @param Times Liste des temps mesurés
run_multiple_tests(_, 0, []) :- !.
run_multiple_tests(TestCase, Count, [Time|RestTimes]) :-
    get_time(Start),
    solve_puzzle(TestCase, _),
    get_time(End),
    Time is End - Start,
    NextCount is Count - 1,
    run_multiple_tests(TestCase, NextCount, RestTimes).

%! calculate_statistics(+Times:list, -Mean:float, -Min:float, -Max:float) is det.
%  Calcule les statistiques de base sur une liste de temps
%  @param Times Liste des temps mesurés
%  @param Mean Temps moyen
%  @param Min Temps minimum
%  @param Max Temps maximum
calculate_statistics(Times, Mean, Min, Max) :-
    length(Times, Count),
    sum_list(Times, Sum),
    Mean is Sum / Count,
    min_list(Times, Min),
    max_list(Times, Max).

% =============================================================================
% SECTION 6: MODES D'UTILISATION AVANCÉS
% =============================================================================

%! solve_custom(+Initial:list, +Goal:list) is det.
%  Interface pour résoudre des configurations personnalisées
%  Permet de tester des configurations non prédéfinies
%  @param Initial État de départ personnalisé
%  @param Goal État but personnalisé
solve_custom(Initial, Goal) :-
    write('🎯 RÉSOLUTION PERSONNALISÉE'), nl,
    format('De: ~w~n', [Initial]),
    format('Vers: ~w~n', [Goal]),
    nl,

    get_time(StartTime),
    catch(
        (solve_custom_puzzle(Initial, Goal, result(Path, Cost, Expanded)),
         get_time(EndTime),
         ResponseTime is EndTime - StartTime,
         display_solution(Path, Cost, Expanded, ResponseTime)),
        Error,
        handle_execution_error(Error)
    ).

%! demo_mode is det.
%  Mode démonstration automatique des deux cas de test
%  Exécute séquentiellement les cas sans interaction utilisateur
demo_mode :-
    write('[DEMO] MODE DEMONSTRATION'), nl,
    write('====================='), nl, nl,

    write('→ Cas test 1...'), nl,
    execute_test_case(case1),

    nl, nl,
    write('→ Cas test 2...'), nl,
    execute_test_case(case2),

    nl,
    write('[OK] Demonstration terminee.'), nl.