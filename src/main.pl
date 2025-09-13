% =============================================================================
% MAIN.PL - Point d'entrée principal du solveur de Taquin
% =============================================================================
% Ce module orchestre l'ensemble du programme :
% - Menu interactif CLI
% - Gestion des cas de test (professeur + personnalisé)
% - Mesure des temps de réponse IA
% - Interface utilisateur et gestion des erreurs
% =============================================================================

:- use_module(game).
:- use_module(astar).
:- use_module(display).

% Point d'entrée principal
:- initialization(main, main).

% main(+Args)
% Point d'entrée principal avec gestion des arguments
main(_) :-
    display_banner,
    main_menu.

% main_menu
% Boucle principale du menu interactif
main_menu :-
    display_menu,
    read_choice(Choice),
    handle_choice(Choice).

% handle_choice(+Choice)
% Traite le choix utilisateur du menu
handle_choice(1) :-
    nl,
    write('=== CAS DE TEST 1 : EXEMPLE PROFESSEUR ==='), nl,
    get_time(StartTime),
    solve_puzzle(case1, result(Path, Cost, Expanded)),
    get_time(EndTime),
    ResponseTime is EndTime - StartTime,
    display_solution(Path, Cost, Expanded, ResponseTime),
    nl,
    main_menu.

handle_choice(2) :-
    nl,
    write('=== CAS DE TEST 2 : EXEMPLE PERSONNALISÉ ==='), nl,
    get_time(StartTime),
    solve_puzzle(case2, result(Path, Cost, Expanded)),
    get_time(EndTime),
    ResponseTime is EndTime - StartTime,
    display_solution(Path, Cost, Expanded, ResponseTime),
    nl,
    main_menu.

handle_choice(3) :-
    nl,
    write('Au revoir !'), nl,
    halt.

handle_choice(_) :-
    nl,
    write('Choix invalide. Veuillez réessayer.'), nl,
    main_menu.

% read_choice(-Choice)
% Lit et valide le choix utilisateur
read_choice(Choice) :-
    write('Votre choix: '),
    read(Choice),
    integer(Choice), !.
read_choice(Choice) :-
    write('Veuillez entrer un nombre valide.'), nl,
    read_choice(Choice).