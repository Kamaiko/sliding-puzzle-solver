/**
 * main.pl - Point d'entrée principal du solveur de taquin
 * 
 * Ce fichier contient l'interface utilisateur et orchestre 
 * l'utilisation des autres modules pour résoudre le puzzle.
 * 
 * Auteur: [Votre nom]
 * Date: [Date]
 * Cours: IFT-2003 Intelligence Artificielle
 */

:- use_module(board).
:- use_module(moves).
:- use_module(heuristics).
:- use_module(search).
:- use_module(utils).

%% solve_taquin(+InitialState, +GoalState, -Path, -Cost, -Expanded)
%  Point d'entrée principal pour résoudre le taquin
%  
%  @param InitialState État initial du puzzle
%  @param GoalState État final désiré
%  @param Path Chemin solution (liste d'états)
%  @param Cost Nombre de mouvements dans la solution
%  @param Expanded Nombre de nœuds explorés
solve_taquin(InitialState, GoalState, Path, Cost, Expanded) :-
    % À implémenter
    fail.

%% demo1/0
%  Exécute le premier cas de test (exemple du professeur)
demo1 :-
    InitialState = [1,2,3,5,0,6,4,7,8],
    GoalState = [1,2,3,4,5,6,7,8,0],
    utils:display_state('État initial', InitialState),
    utils:display_state('État final', GoalState),
    solve_taquin(InitialState, GoalState, Path, Cost, Expanded),
    utils:display_solution(Path, Cost, Expanded).

%% demo2/0
%  Exécute le deuxième cas de test (à définir)
demo2 :-
    % À définir selon vos choix
    fail.

%% main/0
%  Point d'entrée principal du programme
main :-
    writeln('=== Solveur de Taquin 3x3 avec A* ==='),
    writeln('1. demo1 - Cas de test du professeur'),
    writeln('2. demo2 - Cas de test personnalisé'),
    writeln('Tapez demo1. ou demo2. pour exécuter les tests.').