% =============================================================================
% DISPLAY.PL - Affichage et formatage pour le solveur de Taquin
% =============================================================================
% Ce module gère toute la présentation utilisateur :
% - Affichage du plateau 3x3 formaté
% - Messages du menu et bannière
% - Formatage des résultats Path/Cost/Expanded
% - Messages d'erreur et feedback utilisateur
% =============================================================================

% display_banner
% Affiche la bannière du programme
display_banner :-
    nl,
    write('╔════════════════════════════════════════════════╗'), nl,
    write('║        SOLVEUR DE TAQUIN 3x3 avec A*          ║'), nl,
    write('║         Intelligence Artificielle IFT-2003    ║'), nl,
    write('╚════════════════════════════════════════════════╝'), nl, nl.

% display_menu
% Affiche le menu principal
display_menu :-
    write('=== MENU PRINCIPAL ==='), nl,
    write('1. Résoudre cas de test 1 (Exemple professeur)'), nl,
    write('2. Résoudre cas de test 2 (Exemple personnalisé)'), nl,
    write('3. Quitter'), nl, nl.

% display_state(+Title, +State)
% Affiche un état du taquin au format 3x3
display_state(Title, State) :-
    nl,
    write(Title), write(' :'), nl,
    State = [S1,S2,S3,S4,S5,S6,S7,S8,S9],
    write('┌─────┬─────┬─────┐'), nl,
    format('│  ~w  │  ~w  │  ~w  │~n', [S1,S2,S3]),
    write('├─────┼─────┼─────┤'), nl,
    format('│  ~w  │  ~w  │  ~w  │~n', [S4,S5,S6]),
    write('├─────┼─────┼─────┤'), nl,
    format('│  ~w  │  ~w  │  ~w  │~n', [S7,S8,S9]),
    write('└─────┴─────┴─────┘'), nl.

% display_solution(+Path, +Cost, +Expanded, +ResponseTime)
% Affiche la solution complète avec statistiques
display_solution(Path, Cost, Expanded, ResponseTime) :-
    nl,
    write('╔══════════════════════════════════════╗'), nl,
    write('║              SOLUTION                ║'), nl,
    write('╚══════════════════════════════════════╝'), nl,
    display_path(Path),
    nl,
    write('╔══════════════════════════════════════╗'), nl,
    write('║             STATISTIQUES             ║'), nl,
    write('╚══════════════════════════════════════╝'), nl,
    format('Path   : ~w états (A → B → C → ... → But)~n', [Path]),
    format('Cost   : ~w mouvements~n', [Cost]),
    format('Expanded: ~w nœuds explorés~n', [Expanded]),
    format('Temps IA: ~3f secondes~n', [ResponseTime]),
    nl.

% display_path(+Path)
% Affiche séquentiellement tous les états du chemin
display_path([]).
display_path([State|RestPath]) :-
    display_state_compact(State),
    (   RestPath = [] ->
        write('    ↓ BUT ATTEINT!')
    ;   write('    ↓')
    ),
    nl,
    display_path(RestPath).

% display_state_compact(+State)
% Affiche un état en format compact pour le chemin
display_state_compact([S1,S2,S3,S4,S5,S6,S7,S8,S9]) :-
    format('│ ~w ~w ~w │~n', [S1,S2,S3]),
    format('│ ~w ~w ~w │~n', [S4,S5,S6]),
    format('│ ~w ~w ~w │~n', [S7,S8,S9]).

% format_tile(+Tile, -Formatted)
% Formate une tuile (remplace 0 par espace)
format_tile(0, ' ') :- !.
format_tile(Tile, Tile).