% =============================================================================
% GAME.PL - Logique du jeu de Taquin (États et Mouvements)
% =============================================================================
% Ce module contient la représentation et la manipulation des états du taquin :
% - Définitions des états de référence (cas tests académiques)
% - Validation complète des configurations (format et solvabilité)
% - Génération de mouvements dans l'ordre strict requis
% - Utilitaires de manipulation d'états optimisés
%
% IMPORTANT: Ordre des mouvements OBLIGATOIRE = HAUT, BAS, GAUCHE, DROITE
%           pour garantir la reproductibilité académique (9 nœuds explorés)
% =============================================================================

% =============================================================================
% SECTION 1: DÉFINITIONS D'ÉTATS DE RÉFÉRENCE
% =============================================================================

%! initial_state(-State:list) is det.
%  État initial du cas test 1 (exemple professeur)
%  Configuration: 1 2 3
%                 5 * 6
%                 4 7 8
initial_state([1,2,3,5,0,6,4,7,8]).

%! goal_state(-State:list) is det.
%  État final du cas test 1 (configuration résolue)
%  Configuration: 1 2 3
%                 4 5 6
%                 7 8 *
goal_state([1,2,3,4,5,6,7,8,0]).

%! custom_initial_state(-State:list) is det.
%  État initial du cas test 2 (personnalisé, >6 mouvements)
%  Configuration plus complexe pour démonstration étendue
custom_initial_state([2,8,3,1,6,4,7,0,5]).

%! custom_goal_state(-State:list) is det.
%  État final du cas test 2
custom_goal_state([1,2,3,8,0,4,7,6,5]).

% =============================================================================
% SECTION 2: VALIDATION D'ÉTATS ET SOLVABILITÉ
% =============================================================================

%! valid_state(+State:list) is semidet.
%  Vérifie qu'un état de taquin est valide
%  @param State Liste de 9 éléments représentant le plateau 3x3
%  Critères: exactement 9 éléments, chiffres 0-8 uniques
valid_state(State) :-
    % Vérifier la longueur (exactement 9 cases)
    length(State, 9),
    % Vérifier que tous les chiffres 0-8 sont présents une seule fois
    sort(State, [0,1,2,3,4,5,6,7,8]).

%! is_solvable(+State:list, +Goal:list) is semidet.
%  Détermine si un état peut être résolu vers l'état but
%  Basé sur la parité des inversions (théorie des permutations)
%  @param State État initial à vérifier
%  @param Goal État but (généralement position finale standard)
is_solvable(State, Goal) :-
    valid_state(State),
    valid_state(Goal),
    count_inversions(State, InversionsState),
    count_inversions(Goal, InversionsGoal),
    % Un taquin est solvable si les inversions ont même parité
    Parity1 is InversionsState mod 2,
    Parity2 is InversionsGoal mod 2,
    Parity1 =:= Parity2.

%! count_inversions(+State:list, -Count:integer) is det.
%  Compte le nombre d'inversions dans un état (case vide ignorée)
%  Une inversion = tuile A avant tuile B mais A > B dans l'ordre final
%  @param State État à analyser
%  @param Count Nombre d'inversions trouvées
count_inversions(State, Count) :-
    % Filtrer la case vide (0) car elle n'affecte pas la solvabilité
    exclude(==(0), State, FilteredState),
    count_inversions_helper(FilteredState, 0, Count).

count_inversions_helper([], Count, Count).
count_inversions_helper([H|T], Acc, Count) :-
    % Pour chaque élément, compter combien d'éléments suivants sont plus petits
    include(<(H), T, SmallerElements),
    length(SmallerElements, Inversions),
    NewAcc is Acc + Inversions,
    count_inversions_helper(T, NewAcc, Count).

%! states_equal(+State1:list, +State2:list) is semidet.
%  Compare deux états pour l'égalité
%  @param State1 Premier état
%  @param State2 Second état
states_equal(State1, State2) :-
    State1 = State2.

% =============================================================================
% SECTION 3: GÉNÉRATION DE MOUVEMENTS (ORDRE CRITIQUE)
% =============================================================================

%! find_blank(+State:list, -Position:integer) is det.
%  Trouve la position de la case vide (représentée par 0)
%  @param State État du taquin
%  @param Position Position de la case vide (0-8)
find_blank(State, Position) :-
    nth0(Position, State, 0).

%! valid_move(+BlankPosition:integer, +Direction:atom) is semidet.
%  Vérifie si un mouvement est valide selon les limites du plateau 3x3
%  @param BlankPosition Position actuelle de la case vide (0-8)
%  @param Direction Direction du mouvement (up/down/left/right)
%
%  Plateau numéroté:  0 1 2
%                     3 4 5
%                     6 7 8
valid_move(BlankPos, up) :-
    % Peut aller vers le haut si pas sur la première ligne (positions 0,1,2)
    BlankPos >= 3.
valid_move(BlankPos, down) :-
    % Peut aller vers le bas si pas sur la dernière ligne (positions 6,7,8)
    BlankPos =< 5.
valid_move(BlankPos, left) :-
    % Peut aller à gauche si pas sur la première colonne (positions 0,3,6)
    BlankPos mod 3 =\= 0.
valid_move(BlankPos, right) :-
    % Peut aller à droite si pas sur la dernière colonne (positions 2,5,8)
    BlankPos mod 3 =\= 2.

%! get_target_position(+BlankPos:integer, +Direction:atom, -TargetPos:integer) is det.
%  Calcule la position cible selon la direction du mouvement
%  @param BlankPos Position actuelle de la case vide
%  @param Direction Direction du mouvement
%  @param TargetPos Position résultante après le mouvement
get_target_position(BlankPos, up, TargetPos) :-
    TargetPos is BlankPos - 3.  % Monter d'une ligne
get_target_position(BlankPos, down, TargetPos) :-
    TargetPos is BlankPos + 3.  % Descendre d'une ligne
get_target_position(BlankPos, left, TargetPos) :-
    TargetPos is BlankPos - 1.  % Aller à gauche
get_target_position(BlankPos, right, TargetPos) :-
    TargetPos is BlankPos + 1.  % Aller à droite

%! apply_move(+State:list, +Direction:atom, -NewState:list) is det.
%  Applique un mouvement dans la direction donnée
%  @param State État initial
%  @param Direction Direction du mouvement (up/down/left/right)
%  @param NewState État résultant après le mouvement
apply_move(State, Direction, NewState) :-
    find_blank(State, BlankPos),
    valid_move(BlankPos, Direction),
    get_target_position(BlankPos, Direction, TargetPos),
    swap_tiles(State, BlankPos, TargetPos, NewState).

%! generate_moves(+State:list, -Successors:list) is det.
%  Génère tous les mouvements valides depuis un état
%  ORDRE CRITIQUE: HAUT, BAS, GAUCHE, DROITE (requis pour validation académique)
%  @param State État de départ
%  @param Successors Liste des états successeurs dans l'ordre déterministe
generate_moves(State, Successors) :-
    find_blank(State, BlankPos),
    % ORDRE OBLIGATOIRE pour reproductibilité: up, down, left, right
    findall(NewState,
        (member(Direction, [up, down, left, right]),
         valid_move(BlankPos, Direction),
         apply_move(State, Direction, NewState)),
        Successors).

% =============================================================================
% SECTION 4: UTILITAIRES DE MANIPULATION D'ÉTATS
% =============================================================================

%! swap_tiles(+State:list, +Pos1:integer, +Pos2:integer, -NewState:list) is det.
%  Échange les tuiles aux positions Pos1 et Pos2
%  @param State État initial
%  @param Pos1 Première position (0-8)
%  @param Pos2 Seconde position (0-8)
%  @param NewState État avec les tuiles échangées
swap_tiles(State, Pos1, Pos2, NewState) :-
    nth0(Pos1, State, Tile1),
    nth0(Pos2, State, Tile2),
    replace_nth0(State, Pos1, Tile2, TempState),
    replace_nth0(TempState, Pos2, Tile1, NewState).

%! replace_nth0(+List:list, +Index:integer, +Element, -NewList:list) is det.
%  Remplace l'élément à l'index donné dans la liste (version optimisée)
%  @param List Liste originale
%  @param Index Index de l'élément à remplacer (0-based)
%  @param Element Nouvel élément
%  @param NewList Liste avec l'élément remplacé
replace_nth0(List, Index, Element, NewList) :-
    length(Prefix, Index),
    append(Prefix, [_|Suffix], List),
    append(Prefix, [Element|Suffix], NewList).

%! is_goal(+State:list) is semidet.
%  Vérifie si l'état est l'état but du cas test 1
%  @param State État à vérifier
is_goal(State) :-
    goal_state(Goal),
    states_equal(State, Goal).

%! is_custom_goal(+State:list) is semidet.
%  Vérifie si l'état est l'état but personnalisé du cas test 2
%  @param State État à vérifier
is_custom_goal(State) :-
    custom_goal_state(Goal),
    states_equal(State, Goal).