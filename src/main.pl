/** <module> Orchestrateur CLI du solveur de taquin

Point d'entrée avec warm-up JIT et mesure de performance.
Coordonne les modules game.pl, astar.pl et display.pl.

@benchmark Cas 1: 4 mouvements, 12 nœuds, <3ms attendu
@sections
  1. Points d'entrée et initialisation
  2. Menu principal et navigation
  3. Gestion des choix utilisateur
  4. Exécution des cas de test
  5. Utilitaires essentiels
*/

:- encoding(utf8).

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
    (   current_predicate(display_menu/0) -> true
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
%  Lit et valide le choix utilisateur immédiatement (sans Enter)
%  Utilise get_single_char pour saisie instantanée (1, 2, 3, ou 4)
%  @param Choice Choix valide de l'utilisateur (1-4)
read_choice(Choice) :-
    flush_output,
    get_single_char(Code),
    char_code(Char, Code),
    (   Code = -1 ->
        % Gestion EOF ou Ctrl+C : quitter proprement
        halt(0)
    ;   member(Char, ['1', '2', '3', '4']) ->
        % Convertir le caractère en nombre
        atom_number(Char, Choice)
    ;   % Choix invalide
        nl,
        write('CHOIX INVALIDE. Veuillez choisir entre 1-4'), nl,
        read_choice(Choice)  % Réessayer
    ).


% =============================================================================
% SECTION 3: GESTION DES CHOIX UTILISATEUR
% =============================================================================

%! handle_choice(+Choice:integer) is det.
%  Traite le choix utilisateur du menu principal
%  Exécute l'action correspondante avec gestion d'erreurs
%  @param Choice Choix de l'utilisateur (1=cas test 1, 2=cas test 2, 3=a propos, 4=quitter)

% Cas test 1 : Exemple professeur (validation académique)
handle_choice(1) :-
    initial_state(InitState),
    goal_state(GoalState),
    display_case1_banner(InitState, GoalState),
    execute_test_case(case1),
    wait_for_continue,
    main_menu.

% Cas test 2 : Exemple personnalisé (configuration étendue)
handle_choice(2) :-
    display_case2_banner,
    execute_test_case(case2),
    wait_for_continue,
    main_menu.

% A propos du programme
handle_choice(3) :-
    display_about_banner,
    wait_for_continue,
    main_menu.

% Quitter le programme
handle_choice(4) :-
    nl,
    write('MERCI ET AU REVOIR!'), nl,
    flush_output,
    halt(0).

% Gestion des choix invalides (normalement interceptés par read_choice)
handle_choice(InvalidChoice) :-
    nl,
    format('[ERREUR-001] Choix invalide: ~w~n', [InvalidChoice]),
    write('Veuillez choisir entre 1, 2, 3 ou 4.'), nl,
    main_menu.

% =============================================================================
% SECTION 4: EXÉCUTION DES CAS DE TEST
% =============================================================================

%! execute_test_case(+TestCase:atom) is det.
%  Exécute un cas de test avec mesure de performance et gestion d'erreurs
%  @param TestCase Identifiant du cas (case1 | case2)
execute_test_case(TestCase) :-
    % Warm-up pour eliminer la compilation JIT
    catch(solve_puzzle(TestCase, _), _, true),
    display_thinking_message,
    get_time(StartTime),
    catch(
        (solve_puzzle(TestCase, result(Path, Cost, Expanded)),
         get_time(EndTime),
         ResponseTime is EndTime - StartTime,
         display_success_message,
         display_solution(Path, Cost, Expanded, ResponseTime)),
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
    write('Appuyez sur une touche pour continuer...'),
    flush_output,
    get_single_char(_),  % Attendre n'importe quel caractère (immédiat)
    nl.

% =============================================================================
% SECTION 5: UTILITAIRES ESSENTIELS
% =============================================================================

%! solve_custom(+Initial:list, +Goal:list) is det.
%  Interface pour configurations personnalisées
solve_custom(Initial, Goal) :-
    get_time(StartTime),
    solve_custom_puzzle(Initial, Goal, result(Path, Cost, Expanded)),
    get_time(EndTime),
    ResponseTime is EndTime - StartTime,
    display_solution(Path, Cost, Expanded, ResponseTime).