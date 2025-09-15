% =============================================================================
% DISPLAY.PL - Affichage et formatage pour le solveur de Taquin
% =============================================================================

:- encoding(utf8).
%
% ÉQUIPE       : Projet universitaire IFT-2003
% COURS        : IFT-2003 - Intelligence Artificielle
% INSTITUTION  : Université Laval
% VERSION      : 1.0
%
% DESCRIPTION  : Module de présentation et interface utilisateur pour le
%                solveur de taquin. Gère l'affichage professionnel des
%                résultats et l'interaction avec l'utilisateur.
%
% FONCTIONNALITÉS PRINCIPALES :
% - Affichage du plateau 3×3 avec représentation visuelle claire
% - Messages du menu et bannière professionnelle
% - Formatage des résultats Path A->B->C->D->E avec flèches
% - Statistiques détaillées (Path/Cost/Expanded/Temps IA)
% - Messages d'erreur et feedback utilisateur en français
%
% ARCHITECTURE DES SECTIONS :
% 1. Bannières et menus principaux
% 2. Affichage des plateaux et états
% 3. Formatage des solutions
% 4. Messages et feedback utilisateur
% 5. Utilitaires d'affichage
%
% =============================================================================

% =============================================================================
% SECTION 1: BANNIÈRES ET MENUS PRINCIPAUX
% =============================================================================


%! display_menu is det.
%  Affiche le menu unifié avec titre ASCII et options
%  Fusion de l'écran d'accueil et du menu principal
display_menu :-
    write('╔═══════════════════════════════════════════════════════════════════════════════╗'), nl,
    write('║                                                                               ║'), nl,
    write('║     ████████╗ █████╗  ██████╗ ██╗   ██╗██╗███╗   ██╗                          ║'), nl,
    write('║     ╚══██╔══╝██╔══██╗██╔═══██╗██║   ██║██║████╗  ██║                          ║'), nl,
    write('║        ██║   ███████║██║   ██║██║   ██║██║██╔██╗ ██║                          ║'), nl,
    write('║        ██║   ██╔══██║██║▄▄ ██║██║   ██║██║██║╚██╗██║                          ║'), nl,
    write('║        ██║   ██║  ██║╚██████╔╝╚██████╔╝██║██║ ╚████║                          ║'), nl,
    write('║        ╚═╝   ╚═╝  ╚═╝ ╚═════╝  ╚═════╝ ╚═╝╚═╝  ╚═══╝                          ║'), nl,
    write('║        ███████╗ ██████╗ ██╗    ██╗   ██╗███████╗██╗   ██╗██████╗              ║'), nl,
    write('║        ██╔════╝██╔═══██╗██║    ██║   ██║██╔════╝██║   ██║██╔══██╗             ║'), nl,
    write('║        ███████╗██║   ██║██║    ██║   ██║█████╗  ██║   ██║██████╔╝             ║'), nl,
    write('║        ╚════██║██║   ██║██║    ╚██╗ ██╔╝██╔══╝  ██║   ██║██╔══██╗             ║'), nl,
    write('║        ███████║╚██████╔╝███████╗╚████╔╝ ███████╗╚██████╔╝██║  ██║             ║'), nl,
    write('║        ╚══════╝ ╚═════╝ ╚══════╝ ╚═══╝  ╚══════╝ ╚═════╝ ╚═╝  ╚═╝             ║'), nl,
    write('║                         ╔═════════════════════════╗                           ║'), nl,
    write('║                         ║ [1] CAS TEST CLASSIQUE  ║                           ║'), nl,
    write('║                         ╠═════════════════════════╣                           ║'), nl,
    write('║                         ║ [2] CAS TEST AVANCE     ║                           ║'), nl,
    write('║                         ╠═════════════════════════╣                           ║'), nl,
    write('║                         ║ [3] A PROPOS            ║                           ║'), nl,
    write('║                         ╠═════════════════════════╣                           ║'), nl,
    write('║                         ║ [4] QUITTER             ║                           ║'), nl,
    write('║                         ╚═════════════════════════╝                           ║'), nl,
    write('║                           SOLVEUR INTELLIGENT A*                              ║'), nl,
    write('║                               IFT-2003 - IA                                   ║'), nl,
    write('║                             Universite Laval                                  ║'), nl,
    write('╚═══════════════════════════════════════════════════════════════════════════════╝'), nl, nl,
    write('Votre choix: ').

% =============================================================================
% SECTION 2: AFFICHAGE D'ÉTATS DU TAQUIN
% =============================================================================

%! display_state(+Title:string, +State:list) is det.
%  Affiche un état du taquin au format 3×3 avec titre
%  Case vide (0) affichée comme "*" selon conventions
%  @param Title Titre à afficher au-dessus du plateau
%  @param State État du taquin [1,2,3,5,0,6,4,7,8]
display_state(Title, State) :-
    nl,
    format('~w :', [Title]), nl,
    State = [S1,S2,S3,S4,S5,S6,S7,S8,S9],

    % Formatter les tuiles (0 -> *)
    format_tile(S1, F1), format_tile(S2, F2), format_tile(S3, F3),
    format_tile(S4, F4), format_tile(S5, F5), format_tile(S6, F6),
    format_tile(S7, F7), format_tile(S8, F8), format_tile(S9, F9),

    % Affichage avec bordures simples UTF-8
    write('┌─────┬─────┬─────┐'), nl,
    format('│  ~w  │  ~w  │  ~w  │~n', [F1,F2,F3]),
    write('├─────┼─────┼─────┤'), nl,
    format('│  ~w  │  ~w  │  ~w  │~n', [F4,F5,F6]),
    write('├─────┼─────┼─────┤'), nl,
    format('│  ~w  │  ~w  │  ~w  │~n', [F7,F8,F9]),
    write('└─────┴─────┴─────┘'), nl.

%! display_state_compact(+State:list) is det.
%  Affiche un état en format compact pour le chemin solution
%  Version condensée pour l'affichage séquentiel du path
%  @param State État du taquin à afficher
display_state_compact(State) :-
    State = [S1,S2,S3,S4,S5,S6,S7,S8,S9],

    % Formatter les tuiles (0 -> *)
    format_tile(S1, F1), format_tile(S2, F2), format_tile(S3, F3),
    format_tile(S4, F4), format_tile(S5, F5), format_tile(S6, F6),
    format_tile(S7, F7), format_tile(S8, F8), format_tile(S9, F9),

    % Format compact avec bordures simples
    format('   │ ~w ~w ~w │~n', [F1,F2,F3]),
    format('   │ ~w ~w ~w │~n', [F4,F5,F6]),
    format('   │ ~w ~w ~w │~n', [F7,F8,F9]).

%! format_tile(+Tile:integer, -Formatted) is det.
%  Formate une tuile pour l'affichage (0 devient #)
%  Convention: case vide représentée par "#" selon mockups
%  @param Tile Valeur de la tuile (0-8)
%  @param Formatted Représentation formatée pour affichage
format_tile(0, '#') :- !.  % Case vide = # selon mockups
format_tile(Tile, Tile).   % Autres tuiles inchangées

% =============================================================================
% SECTION 3: AFFICHAGE DES RÉSULTATS ET SOLUTIONS
% =============================================================================

%! display_solution(+Path:list, +Cost:integer, +Expanded:integer, +ResponseTime:float) is det.
%  Affiche la solution complète avec statistiques détaillées
%  Format professionnel avec chemin complet et métriques académiques
%  @param Path Chemin solution (liste des états)
%  @param Cost Nombre de mouvements (coût de la solution)
%  @param Expanded Nombre de nœuds explorés par A*
%  @param ResponseTime Temps de calcul IA en secondes
display_solution(Path, Cost, Expanded, ResponseTime) :-
    nl,
    write('╔═══════════════════════════════════════════════════════════════════════════════╗'), nl,
    write('║                                CHEMIN SOLUTION                                ║'), nl,
    write('╚═══════════════════════════════════════════════════════════════════════════════╝'), nl,

    % Afficher le chemin complet étape par étape avec labels
    display_path_sequence_with_labels(Path, 1),

    nl,
    write('╔═══════════════════════════════════════════════════════════════════════════════╗'), nl,
    write('║                             PARAMETRES DE RESOLUTION                          ║'), nl,
    write('╚═══════════════════════════════════════════════════════════════════════════════╝'), nl,

    % Afficher les paramètres essentiels sans préfixes
    length(Path, PathLength),
    format('Path length : ~w etats~n', [PathLength]),
    format('Cost        : ~w mouvements~n', [Cost]),
    format('Expanded    : ~w noeuds~n', [Expanded]),
    format('Temps       : ~3f secondes~n', [ResponseTime]),
    nl.

%! display_path_sequence_with_labels(+Path:list, +Index:integer) is det.
%  Affiche séquentiellement tous les états du chemin avec labels A, B, C, D, E
%  @param Path Liste des états depuis initial vers but
%  @param Index Index actuel pour générer le label (1=A, 2=B, etc.)
display_path_sequence_with_labels([], _).
display_path_sequence_with_labels([State], Index) :-
    % Dernier état (but atteint)
    StateLabel is Index + 64,  % 65='A', 66='B', etc.
    char_code(Label, StateLabel),
    format('ETAT ~w :~n', [Label]),
    display_state_compact(State),
    write('   [BUT ATTEINT!]'), nl, !.
display_path_sequence_with_labels([State|RestPath], Index) :-
    % États intermédiaires
    StateLabel is Index + 64,  % 65='A', 66='B', etc.
    char_code(Label, StateLabel),
    format('ETAT ~w :~n', [Label]),
    display_state_compact(State),
    write('        ↓'), nl,
    NextIndex is Index + 1,
    display_path_sequence_with_labels(RestPath, NextIndex).


% =============================================================================
% SECTION 4: MESSAGES D'ERREUR ET FEEDBACK
% =============================================================================

%! display_error(+ErrorType:atom, +Details:string) is det.
%  Affiche un message d'erreur formaté avec contexte
%  @param ErrorType Type d'erreur (timeout, invalid_state, unsolvable)
%  @param Details Détails supplémentaires sur l'erreur
display_error(timeout, Details) :-
    nl,
    write('╔══════════════════════════════════════╗'), nl,
    write('║            TIMEOUT                   ║'), nl,
    write('╚══════════════════════════════════════╝'), nl,
    format('Erreur: ~w~n', [Details]),
    write('Le calcul a depasse la limite de 10 secondes.'), nl,
    write('Essayez avec une configuration plus simple.'), nl, nl.

display_error(invalid_state, Details) :-
    nl,
    write('╔══════════════════════════════════════╗'), nl,
    write('║            ETAT INVALIDE             ║'), nl,
    write('╚══════════════════════════════════════╝'), nl,
    format('Erreur: ~w~n', [Details]),
    write('L\'etat fourni ne respecte pas le format taquin 3x3.'), nl,
    write('Verifiez: 9 elements, chiffres 0-8 uniques.'), nl, nl.

display_error(unsolvable, Details) :-
    nl,
    write('╔══════════════════════════════════════╗'), nl,
    write('║       IMPOSSIBLE A RESOUDRE         ║'), nl,
    write('╚══════════════════════════════════════╝'), nl,
    format('Erreur: ~w~n', [Details]),
    write('Cette configuration ne peut pas etre resolue.'), nl,
    write('Probleme de parite des inversions.'), nl, nl.

%! display_thinking_message is det.
%  Affiche un message pendant le calcul A*
display_thinking_message :-
    write('[IA] En cours de reflexion...'), nl,
    write('   Exploration de l\'espace d\'etats avec A*'), nl,
    flush_output.

%! display_success_message is det.
%  Affiche un message de succès après résolution
display_success_message :-
    write('[OK] Solution optimale trouvee!'), nl, nl.