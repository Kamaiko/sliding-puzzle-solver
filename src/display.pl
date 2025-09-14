% =============================================================================
% DISPLAY.PL - Affichage et formatage pour le solveur de Taquin
% =============================================================================
% Ce module gère toute la présentation utilisateur :
% - Affichage du plateau 3×3 avec case vide représentée par "*"
% - Messages du menu et bannière professionnelle
% - Formatage des résultats Path A→B→C→D→E avec flèches
% - Statistiques détaillées (Path/Cost/Expanded/Temps IA)
% - Messages d'erreur et feedback utilisateur en français
% =============================================================================

% =============================================================================
% SECTION 1: BANNIÈRES ET MENUS PRINCIPAUX
% =============================================================================

%! display_banner is det.
%  Affiche la bannière d'accueil du programme
%  Design professionnel avec ASCII art et informations contextuelles
display_banner :-
    nl,
    write('╔════════════════════════════════════════════════════════════════╗'), nl,
    write('║                 🧩 SOLVEUR DE TAQUIN A*                       ║'), nl,
    write('║                Intelligence Artificielle IFT-2003             ║'), nl,
    write('║                      Université Laval                         ║'), nl,
    write('╟────────────────────────────────────────────────────────────────╢'), nl,
    write('║  Algorithme: A* avec heuristique tuiles mal placées          ║'), nl,
    write('║  Performance: Solutions optimales en <1 seconde              ║'), nl,
    write('║  Validation: Cost=4, Expanded=9 (cas test académique)        ║'), nl,
    write('╚════════════════════════════════════════════════════════════════╝'), nl, nl.

%! display_menu is det.
%  Affiche le menu principal avec options de navigation
%  Interface claire pour sélection des cas de test
display_menu :-
    write('╔═══════════════════════════════════════╗'), nl,
    write('║            MENU PRINCIPAL             ║'), nl,
    write('╠═══════════════════════════════════════╣'), nl,
    write('║  1. Résoudre cas test 1 (Professeur) ║'), nl,
    write('║     État: [1,2,3,5,0,6,4,7,8]        ║'), nl,
    write('║     → [1,2,3,4,5,6,7,8,0]            ║'), nl,
    write('║                                       ║'), nl,
    write('║  2. Résoudre cas test 2 (Personnalisé)║'), nl,
    write('║     Configuration étendue (6+ mvts)   ║'), nl,
    write('║                                       ║'), nl,
    write('║  3. Quitter le programme              ║'), nl,
    write('╚═══════════════════════════════════════╝'), nl, nl.

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

    % Formatter les tuiles (0 → *)
    format_tile(S1, F1), format_tile(S2, F2), format_tile(S3, F3),
    format_tile(S4, F4), format_tile(S5, F5), format_tile(S6, F6),
    format_tile(S7, F7), format_tile(S8, F8), format_tile(S9, F9),

    % Affichage avec bordures élégantes
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

    % Formatter les tuiles (0 → *)
    format_tile(S1, F1), format_tile(S2, F2), format_tile(S3, F3),
    format_tile(S4, F4), format_tile(S5, F5), format_tile(S6, F6),
    format_tile(S7, F7), format_tile(S8, F8), format_tile(S9, F9),

    % Format compact avec séparateurs
    format('   │ ~w ~w ~w │~n', [F1,F2,F3]),
    format('   │ ~w ~w ~w │~n', [F4,F5,F6]),
    format('   │ ~w ~w ~w │~n', [F7,F8,F9]).

%! format_tile(+Tile:integer, -Formatted) is det.
%  Formate une tuile pour l'affichage (0 devient *)
%  Convention: case vide représentée par "*" au lieu de "0"
%  @param Tile Valeur de la tuile (0-8)
%  @param Formatted Représentation formatée pour affichage
format_tile(0, '*') :- !.  % Case vide = astérisque
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
    write('╔══════════════════════════════════════════════════════════╗'), nl,
    write('║                     SOLUTION TROUVÉE                    ║'), nl,
    write('╚══════════════════════════════════════════════════════════╝'), nl,

    % Afficher le chemin complet étape par étape
    display_path_sequence(Path),

    nl,
    write('╔══════════════════════════════════════════════════════════╗'), nl,
    write('║                  MÉTRIQUES ACADÉMIQUES                  ║'), nl,
    write('╚══════════════════════════════════════════════════════════╝'), nl,

    % Afficher le chemin au format A→B→C→D→E
    display_path_summary(Path),

    % Afficher les statistiques détaillées
    length(Path, PathLength),
    format('📊 Longueur Path  : ~w états (Initial → But)~n', [PathLength]),
    format('🎯 Cost          : ~w mouvements~n', [Cost]),
    format('🔍 Expanded      : ~w nœuds explorés~n', [Expanded]),
    format('⚡ Temps IA      : ~3f secondes~n', [ResponseTime]),

    % Validation académique
    (   (Cost =:= 4, Expanded =:= 9) ->
        write('✅ VALIDATION ACADÉMIQUE CONFIRMÉE (Cost=4, Expanded=9)'), nl
    ;   write('⚠️  Métriques différentes du cas test standard'), nl
    ),
    nl.

%! display_path_sequence(+Path:list) is det.
%  Affiche séquentiellement tous les états du chemin avec flèches
%  Animation visuelle du processus de résolution
%  @param Path Liste des états depuis initial vers but
display_path_sequence([]).
display_path_sequence([State]) :-
    % Dernier état (but atteint)
    display_state_compact(State),
    write('   🎯 BUT ATTEINT!'), nl, !.
display_path_sequence([State|RestPath]) :-
    % États intermédiaires
    display_state_compact(State),
    write('        ↓'), nl,
    display_path_sequence(RestPath).

%! display_path_summary(+Path:list) is det.
%  Affiche le résumé du chemin au format A→B→C→D→E
%  Représentation compacte pour validation académique
%  @param Path Chemin solution
display_path_summary(Path) :-
    length(Path, Length),
    format('🗺️  Path Summary  : '),
    display_path_labels(Path, 1, Length),
    nl.

% Helper pour afficher les labels A→B→C→D→E
display_path_labels([], _, _).
display_path_labels([_|Rest], Current, Total) :-
    StateLabel is Current + 64,  % 65='A', 66='B', etc.
    char_code(Label, StateLabel),
    write(Label),

    (   Current < Total ->
        write(' → '),
        Next is Current + 1,
        display_path_labels(Rest, Next, Total)
    ;   true
    ).

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
    write('║            ⏰ TIMEOUT               ║'), nl,
    write('╚══════════════════════════════════════╝'), nl,
    format('Erreur: ~w~n', [Details]),
    write('Le calcul a dépassé la limite de 10 secondes.'), nl,
    write('Essayez avec une configuration plus simple.'), nl, nl.

display_error(invalid_state, Details) :-
    nl,
    write('╔══════════════════════════════════════╗'), nl,
    write('║         ❌ ÉTAT INVALIDE           ║'), nl,
    write('╚══════════════════════════════════════╝'), nl,
    format('Erreur: ~w~n', [Details]),
    write('L\'état fourni ne respecte pas le format taquin 3×3.'), nl,
    write('Vérifiez: 9 éléments, chiffres 0-8 uniques.'), nl, nl.

display_error(unsolvable, Details) :-
    nl,
    write('╔══════════════════════════════════════╗'), nl,
    write('║       🚫 IMPOSSIBLE À RÉSOUDRE     ║'), nl,
    write('╚══════════════════════════════════════╝'), nl,
    format('Erreur: ~w~n', [Details]),
    write('Cette configuration ne peut pas être résolue.'), nl,
    write('Problème de parité des inversions.'), nl, nl.

%! display_thinking_message is det.
%  Affiche un message pendant le calcul A*
display_thinking_message :-
    write('🤔 IA en cours de réflexion...'), nl,
    write('   Exploration de l\'espace d\'états avec A*'), nl,
    flush_output.

%! display_success_message is det.
%  Affiche un message de succès après résolution
display_success_message :-
    write('🎉 Solution optimale trouvée!'), nl, nl.