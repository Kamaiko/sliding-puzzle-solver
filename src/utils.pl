/**
 * utils.pl - Utilitaires pour l'affichage et fonctions auxiliaires
 * 
 * Ce module contient les prédicats utilitaires pour afficher
 * les états du taquin et les résultats de la recherche.
 */

:- module(utils, [
    display_state/2,
    display_board/1,
    display_solution/3,
    display_path/1,
    format_tile/2
]).

%% display_state(+Title, +State)
%  Affiche un état avec un titre
display_state(Title, State) :-
    format('~n~w:~n', [Title]),
    display_board(State).

%% display_board(+State)
%  Affiche le plateau de jeu dans un format 3x3 lisible
display_board(State) :-
    format_row(State, 0, 3),
    format_row(State, 3, 6),
    format_row(State, 6, 9),
    nl.

%% format_row(+State, +Start, +End)
%  Formate et affiche une ligne du plateau
format_row(State, Start, End) :-
    format_row_helper(State, Start, End),
    nl.

format_row_helper(_, End, End) :- !.
format_row_helper(State, Current, End) :-
    nth0(Current, State, Value),
    format_tile(Value, Formatted),
    format('~w ', [Formatted]),
    Next is Current + 1,
    format_row_helper(State, Next, End).

%% format_tile(+Value, -Formatted)
%  Formate une tuile pour l'affichage (0 devient '_')
format_tile(0, '_') :- !.
format_tile(Value, Value).

%% display_solution(+Path, +Cost, +Expanded)
%  Affiche les résultats de la recherche
display_solution(Path, Cost, Expanded) :-
    nl,
    writeln('=== RÉSULTATS DE LA RECHERCHE ==='),
    format('Path: ~w étapes~n', [Cost]),
    format('Cost: ~w~n', [Cost]),
    format('Expanded: ~w nœuds~n', [Expanded]),
    nl,
    writeln('Chemin solution:'),
    display_path(Path).

%% display_path(+Path)
%  Affiche le chemin complet de la solution
display_path([]).
display_path([State|Rest]) :-
    display_board(State),
    (Rest \= [] -> 
        writeln('    ↓') ; 
        true
    ),
    display_path(Rest).

%% display_statistics(+StartTime, +EndTime, +Expanded, +Generated)
%  Affiche les statistiques de performance
display_statistics(StartTime, EndTime, Expanded, Generated) :-
    Time is EndTime - StartTime,
    nl,
    writeln('=== STATISTIQUES DE PERFORMANCE ==='),
    format('Temps d\'exécution: ~3f secondes~n', [Time]),
    format('Nœuds explorés: ~w~n', [Expanded]),
    format('Nœuds générés: ~w~n', [Generated]),
    (Generated > 0 ->
        Ratio is Expanded / Generated,
        format('Ratio exploration: ~3f~n', [Ratio])
    ;   true
    ).

%% clear_screen/0
%  Efface l'écran (compatible Unix/Windows)
clear_screen :-
    (current_prolog_flag(windows, true) ->
        shell(cls)
    ;   shell(clear)
    ).