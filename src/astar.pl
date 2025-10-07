/** <module> Recherche A* avec heuristique tuiles mal placées
 *
 * Implémentation de l'algorithme A* avec liste ouverte (open list) et
 * ensemble fermé (closed set). Garantit l'optimalité grâce à une
 * heuristique admissible.
 *
 * @author Équipe 6
 * @see [3] Bratko, I. (2012). Prolog Programming for Artificial Intelligence
 * @see [5] Hart, P.E., et al. (1968). IEEE Trans. on Systems Science and Cybernetics
 * @see [7] Russell, S. & Norvig, P. (2020). Artificial Intelligence: A Modern Approach
 * @complexity Temps et espace O(b^d) où b≈2.67, d≤31 pour taquin 3×3
 */

:- encoding(utf8).

:- use_module(library(ordsets)).

:- consult(game).
:- consult(display).

% =============================================================================
% SECTION 1: CONSTANTES ET CONFIGURATION
% =============================================================================

%! max_timeout(-Seconds:float) is det.
%  Temps maximum d'exécution avant timeout (30 secondes pour tests complexes)
max_timeout(30.0).


% =============================================================================
% SECTION 2: TYPES ET STRUCTURES DE DONNÉES
% =============================================================================

%! Structure d'un nœud A*: node(State, G, H, F, Parent)
%  @param State État du taquin [1,2,3,5,0,6,4,7,8]
%  @param G Coût réel depuis l'état initial (g(n) = profondeur)
%  @param H Valeur heuristique estimée vers le but (h(n))
%  @param F Coût total estimé (f(n) = g(n) + h(n))
%  @param Parent Référence au nœud parent (pour reconstruction chemin)

%! node_state(+Node:compound, -State:list) is det.
%  Extrait l'état d'un nœud A*
node_state(node(State,_,_,_,_), State).

%! node_g_cost(+Node:compound, -G:integer) is det.
%  Extrait le coût g(n) d'un nœud A*
node_g_cost(node(_,G,_,_,_), G).

%! node_h_cost(+Node:compound, -H:integer) is det.
%  Extrait l'heuristique h(n) d'un nœud A*
node_h_cost(node(_,_,H,_,_), H).

%! node_f_cost(+Node:compound, -F:integer) is det.
%  Extrait la fonction d'évaluation f(n) d'un nœud A*
node_f_cost(node(_,_,_,F,_), F).

%! node_parent(+Node:compound, -Parent:compound) is det.
%  Extrait le parent d'un nœud A*
node_parent(node(_,_,_,_,Parent), Parent).

%! create_node(+State:list, +G:integer, +H:integer, +Parent:compound, -Node:compound) is det.
%  Crée un nouveau nœud A* avec tous les champs calculés
%  @param State État du taquin
%  @param G Coût réel g(n)
%  @param H Heuristique h(n)
%  @param Parent Nœud parent
%  @param Node Nœud créé avec f(n) = g(n) + h(n)
create_node(State, G, H, Parent, node(State, G, H, F, Parent)) :-
    F is G + H.

% =============================================================================
% SECTION 3: HEURISTIQUES POUR L'ESTIMATION
% =============================================================================

%! misplaced_tiles_heuristic(+State:list, +Goal:list, -Count:integer) is det.
%  Heuristique admissible comptant les tuiles mal placées.
%
%  La case vide (0) est ignorée car elle sert d'outil pour déplacer
%  les tuiles, pas une tuile à placer. Chaque mouvement corrige au plus
%  une tuile, donc h(n) ≤ h*(n) (admissibilité garantie).
%
%  @arg State État actuel du taquin
%  @arg Goal État but à atteindre
%  @arg Count Nombre de tuiles mal placées (h(n))
%  @see [5] Hart et al. (1968) pour propriétés d'admissibilité

misplaced_tiles_heuristic(State, Goal, Count) :-
    misplaced_tiles_helper(State, Goal, 0, Count).

%! misplaced_tiles_helper(+StateList:list, +GoalList:list, +Acc:integer, -Count:integer) is det.
%  Helper récursif pour calculer les tuiles mal placées
%  @param StateList Reste de l'état à traiter
%  @param GoalList Reste du but correspondant
%  @param Acc Accumulateur du compte actuel
%  @param Count Nombre total de tuiles mal placées
misplaced_tiles_helper([], [], Count, Count).
misplaced_tiles_helper([StateHead|StateTail], [GoalHead|GoalTail], Acc, Count) :-
    (   % Si tuile mal placée ET pas case vide, incrémenter compteur
        (StateHead \= GoalHead, StateHead \= 0) ->
        NewAcc is Acc + 1
    ;   % Sinon, garder le même compteur
        NewAcc = Acc
    ),
    misplaced_tiles_helper(StateTail, GoalTail, NewAcc, Count).

%! manhattan_distance_heuristic(+State:list, +Goal:list, -Distance:integer) is det.
%  Heuristique de distance de Manhattan pour le taquin 3×3.
%
%  Pour chaque tuile (sauf la case vide 0), calcule la distance Manhattan :
%  |row_actuelle - row_but| + |col_actuelle - col_but|
%
%  Cette heuristique est :
%  - **Admissible** : h(n) ≤ h*(n) car chaque mouvement déplace une tuile d'une case
%  - **Consistante** : h(n) ≤ c(n,n') + h(n') car |Δh| ≤ 1 par mouvement
%  - **Plus précise** que tuiles mal placées : réduit drastiquement l'espace de recherche
%
%  Pour un puzzle 3×3, position i (0-8) → row = i // 3, col = i mod 3
%
%  @arg State État actuel du taquin [T1,T2,...,T8,0]
%  @arg Goal État but [1,2,3,4,5,6,7,8,0]
%  @arg Distance Somme des distances Manhattan de toutes les tuiles
%  @see [6] Russell & Norvig (2020, p.103-104) pour heuristiques admissibles du taquin
manhattan_distance_heuristic(State, Goal, Distance) :-
    manhattan_sum(State, Goal, 0, 0, Distance).

%! manhattan_sum(+State:list, +Goal:list, +Pos:int, +Acc:int, -Distance:int) is det.
%  Helper récursif pour calculer la somme des distances Manhattan.
%
%  @param State Reste de l'état à traiter
%  @param Goal Reste du but (utilisé pour trouver position but de chaque tuile)
%  @param Pos Position actuelle dans la grille (0-8)
%  @param Acc Accumulateur de la distance totale
%  @param Distance Somme finale des distances Manhattan
manhattan_sum([], [], _, Acc, Acc).
manhattan_sum([Tile|RestState], [_|RestGoal], Pos, Acc, Distance) :-
    (   Tile =:= 0 ->
        % Case vide (blank), distance = 0
        NewAcc = Acc
    ;   % Trouver la position but de cette tuile dans l'état objectif
        nth0(GoalPos, [1,2,3,4,5,6,7,8,0], Tile),
        % Position actuelle : row = Pos // 3, col = Pos mod 3
        CurrentRow is Pos // 3,
        CurrentCol is Pos mod 3,
        % Position but : row = GoalPos // 3, col = GoalPos mod 3
        GoalRow is GoalPos // 3,
        GoalCol is GoalPos mod 3,
        % Distance Manhattan = |ΔRow| + |ΔCol|
        RowDiff is abs(CurrentRow - GoalRow),
        ColDiff is abs(CurrentCol - GoalCol),
        TileDist is RowDiff + ColDiff,
        NewAcc is Acc + TileDist
    ),
    NextPos is Pos + 1,
    manhattan_sum(RestState, RestGoal, NextPos, NewAcc, Distance).


% =============================================================================
% SECTION 4: CŒUR ALGORITHME A* 
% =============================================================================

%! astar_search(+Initial:list, +Goal:list, -Path:list, -Cost:integer, -Expanded:integer) is det.
%  Point d'entrée principal de l'algorithme A*.
%
%  Recherche le chemin optimal depuis Initial vers Goal en utilisant
%  la fonction d'évaluation f(n) = g(n) + h(n). Garantit l'optimalité
%  avec heuristique admissible.
%
%  @arg Initial État de départ
%  @arg Goal État à atteindre
%  @arg Path Chemin solution (liste des états depuis initial vers goal)
%  @arg Cost Coût de la solution (nombre de mouvements)
%  @arg Expanded Nombre de nœuds générés durant la recherche
%  @throws error(timeout, _) Si le temps d'exécution dépasse 10 secondes
%  @throws error(unsolvable, _) Si la configuration est impossible à résoudre
%  @see [5] Hart et al. (1968) pour l'algorithme A* original 
astar_search(Initial, Goal, Path, Cost, Expanded) :-
    % Étape 1: Validation préalable des états
    validate_search_inputs(Initial, Goal),

    % Étape 2: Vérification cas trivial (déjà résolu)
    (   states_equal(Initial, Goal) ->
        % Solution immédiate
        Path = [Initial],
        Cost = 0,
        Expanded = 0
    ;   % Étape 3: Lancer la recherche A* complète
        initialize_search(Initial, Goal, InitialNode, SearchContext),
        execute_astar_search(InitialNode, SearchContext, Result),
        extract_search_results(Result, Path, Cost, Expanded)
    ).

%! validate_search_inputs(+Initial:list, +Goal:list) is det.
%  Valide les paramètres d'entrée de la recherche
validate_search_inputs(Initial, Goal) :-
    valid_state(Initial),
    valid_state(Goal),
    is_solvable(Initial, Goal).

%! initialize_search(+Initial:list, +Goal:list, -InitialNode:compound, -Context:compound) is det.
%  Initialise les structures pour la recherche A*
%  @param Initial État de départ
%  @param Goal État but
%  @param InitialNode Nœud initial avec f(n) calculé
%  @param Context Contexte de recherche search_context(Goal, StartTime, OpenList, ClosedSet, ExpansionCount, GenerationCount)
initialize_search(Initial, Goal, InitialNode, search_context(Goal, StartTime, [InitialNode], [], 0, 0)) :-
    % Calculer l'heuristique pour le nœud initial
    manhattan_distance_heuristic(Initial, Goal, InitialH),
    % Créer le nœud initial avec g=0
    create_node(Initial, 0, InitialH, nil, InitialNode),
    % Enregistrer le temps de début pour le timeout
    get_time(StartTime).

%! execute_astar_search(+InitialNode:compound, +Context:compound, -Result:compound) is det.
%  Exécute la recherche A* principale
execute_astar_search(_InitialNode, Context, Result) :-
    Context = search_context(Goal, StartTime, OpenList, ClosedSet, ExpCount, GenCount),
    astar_main_loop(OpenList, ClosedSet, Goal, StartTime, ExpCount, GenCount, Result).

%! astar_main_loop(+OpenList:list, +ClosedSet:list, +Goal:list, +StartTime:float, +ExpCount:int, +GenCount:int, -Result:compound) is det.
%  Boucle principale de l'algorithme A*.
%
%  Explore les nœuds par ordre de f(n) croissant jusqu'à atteindre le but.
%  Utilise un closed set pour éviter la re-exploration.
%
%  @invariant OpenList triée par f(n) croissant, tie-breaking sur g(n)
%  @invariant États dans ClosedSet ne sont jamais ré-explorés
%  @invariant f(n) monotone non-décroissant lors de l'exploration

% Cas de base: Open list vide = échec de la recherche
astar_main_loop([], _, _, _, ExpCount, GenCount, search_failed(ExpCount, GenCount)) :-
    !,
    fail.

% Cas récursif: Traitement du nœud avec plus petit f(n)
astar_main_loop([CurrentNode|RestOpen], ClosedSet, Goal, StartTime, ExpCount, GenCount, Result) :-
    % Étape 1: Vérification du timeout de sécurité
    check_search_timeout(StartTime),

    % Étape 2: Extraction des données du nœud courant
    node_state(CurrentNode, CurrentState),

    % Étape 3: Test d'arrivée au but
    (   is_goal_reached(CurrentState, Goal) ->
        % SUCCESS: But atteint, finaliser la recherche
        Result = search_success(CurrentNode, ExpCount, GenCount)

    % Étape 4: Vérifier si l'état est déjà exploré
    ;   is_state_in_closed_set(CurrentState, ClosedSet) ->
        % État déjà traité, continuer avec le reste de l'open list
        astar_main_loop(RestOpen, ClosedSet, Goal, StartTime, ExpCount, GenCount, Result)

    % Étape 5: Nouvel état à explorer - EXPANSION
    ;   expand_current_node(CurrentNode, RestOpen, ClosedSet, Goal, StartTime, ExpCount, GenCount, Result)
    ).

%! check_search_timeout(+StartTime:float) is det.
%  Vérifie que le timeout n'est pas dépassé
check_search_timeout(StartTime) :-
    get_time(CurrentTime),
    ElapsedTime is CurrentTime - StartTime,
    max_timeout(MaxTime),
    (   ElapsedTime > MaxTime ->
        throw(error('[TIMEOUT-001] Délai dépassé (>10 secondes)', astar_search))
    ;   true
    ).

%! is_goal_reached(+State:list, +Goal:list) is semidet.
%  Teste si l'état courant est l'état but
is_goal_reached(State, Goal) :-
    states_equal(State, Goal).

%! is_state_in_closed_set(+State:list, +ClosedSet:list) is semidet.
%  Vérifie si un état est déjà dans le closed set (recherche binaire O(log n))
is_state_in_closed_set(State, ClosedSet) :-
    ord_memberchk(State, ClosedSet).

%! expand_current_node(+CurrentNode:compound, +RestOpen:list, +ClosedSet:list, +Goal:list, +StartTime:float, +ExpCount:int, +GenCount:int, -Result:compound) is det.
%  Expanse le nœud courant en générant ses successeurs
expand_current_node(CurrentNode, RestOpen, ClosedSet, Goal, StartTime, ExpCount, GenCount, Result) :-
    % Incrémenter le compteur de nœuds explorés
    NewExpCount is ExpCount + 1,

    % Afficher trace de debug si activé
    debug_expansion_trace(CurrentNode, NewExpCount, ClosedSet),

    % Ajouter l'état courant au closed set (ensemble ordonné pour performance)
    node_state(CurrentNode, CurrentState),
    ord_add_element(ClosedSet, CurrentState, NewClosedSet),

    % Générer et traiter tous les successeurs
    generate_and_process_successors(CurrentNode, Goal, SuccessorNodes, GenCount, NewGenCount),

    % Mise à jour de l'open list et continuation
    update_open_list_and_continue(SuccessorNodes, RestOpen, NewClosedSet, Goal, StartTime, NewExpCount, NewGenCount, Result).

%! generate_and_process_successors(+CurrentNode:compound, +Goal:list, -SuccessorNodes:list, +GenCountIn:int, -GenCountOut:int) is det.
%  Génère tous les successeurs d'un nœud et crée les nœuds A* correspondants
generate_and_process_successors(CurrentNode, Goal, SuccessorNodes, GenCountIn, GenCountOut) :-
    % Extraire l'état et le coût du nœud courant
    node_state(CurrentNode, CurrentState),
    node_g_cost(CurrentNode, CurrentG),

    % Générer tous les états successeurs possibles
    generate_moves(CurrentState, SuccessorStates),

    % Créer les nœuds A* pour chaque successeur
    NextG is CurrentG + 1,
    create_successor_nodes(SuccessorStates, Goal, NextG, CurrentNode, SuccessorNodes, GenCountIn, GenCountOut).

%! update_open_list_and_continue(+SuccessorNodes:list, +RestOpen:list, +ClosedSet:list, +Goal:list, +StartTime:float, +ExpCount:int, +GenCount:int, -Result:compound) is det.
%  Met à jour l'open list et continue la recherche
update_open_list_and_continue(SuccessorNodes, RestOpen, ClosedSet, Goal, StartTime, ExpCount, GenCount, Result) :-
    % Filtrer les successeurs déjà dans le closed set
    filter_closed_successors(SuccessorNodes, ClosedSet, FilteredClosed),

    % Filtrer les successeurs déjà dans l'open list (garder seulement si meilleur g)
    filter_open_duplicates_simple(FilteredClosed, RestOpen, FilteredSuccessors),

    % Ajouter les successeurs filtrés à l'open list
    append(RestOpen, FilteredSuccessors, UpdatedOpenList),

    % Trier par f(n) croissant (avec tie-breaking sur g(n))
    sort_open_list_by_f_value(UpdatedOpenList, SortedOpenList),

    % Continuer la recherche avec les structures mises à jour
    astar_main_loop(SortedOpenList, ClosedSet, Goal, StartTime, ExpCount, GenCount, Result).

%! deduplicate_by_state(+SortedNodes:list, -UniqueNodes:list) is det.
%  Élimine les duplicatas d'états dans une liste triée de nœuds.
%  Garde seulement la première occurrence de chaque état (meilleur g après tri).
%
%  Après le tri par f puis g, pour un même état :
%  - Le nœud avec g=5, h=3 (f=8) apparaît avant
%  - Le nœud avec g=10, h=3 (f=13) apparaît après
%  On garde le premier = meilleur chemin vers cet état
deduplicate_by_state([], []).
deduplicate_by_state([Node|Rest], [Node|UniqueRest]) :-
    node_state(Node, State),
    % Retirer tous les nœuds suivants ayant le même état
    filter_out_state(Rest, State, RemainingNodes),
    % Continuer la déduplication sur le reste
    deduplicate_by_state(RemainingNodes, UniqueRest).

%! filter_out_state(+Nodes:list, +StateToRemove:list, -Filtered:list) is det.
%  Retire tous les nœuds ayant l'état spécifié
filter_out_state([], _, []).
filter_out_state([Node|Rest], StateToRemove, Filtered) :-
    node_state(Node, NodeState),
    (   states_equal(NodeState, StateToRemove) ->
        % Même état, le retirer
        filter_out_state(Rest, StateToRemove, Filtered)
    ;   % État différent, le garder
        filter_out_state(Rest, StateToRemove, RestFiltered),
        Filtered = [Node|RestFiltered]
    ).

%! filter_open_duplicates_simple(+Successors:list, +OpenList:list, -Filtered:list) is det.
%  Filtre les successeurs en excluant ceux déjà dans l'open list avec un g égal ou meilleur
filter_open_duplicates_simple([], _, []).
filter_open_duplicates_simple([Succ|Rest], OpenList, Filtered) :-
    node_state(Succ, SuccState),
    node_g_cost(Succ, SuccG),
    (   find_best_g_in_open(SuccState, OpenList, BestG),
        SuccG >= BestG ->
        % État déjà dans open avec meilleur g, l'exclure
        filter_open_duplicates_simple(Rest, OpenList, Filtered)
    ;   % État nouveau ou avec meilleur g, le garder
        filter_open_duplicates_simple(Rest, OpenList, RestFiltered),
        Filtered = [Succ|RestFiltered]
    ).

%! find_best_g_in_open(+State:list, +OpenList:list, -BestG:int) is semidet.
%  Trouve le meilleur g (plus petit) pour un état dans l'open list
%  Échoue si l'état n'est pas dans l'open list
find_best_g_in_open(State, [Node|Rest], BestG) :-
    node_state(Node, NodeState),
    (   states_equal(State, NodeState) ->
        node_g_cost(Node, G),
        (   find_best_g_in_open(State, Rest, RestG) ->
            BestG is min(G, RestG)
        ;   BestG = G
        )
    ;   find_best_g_in_open(State, Rest, BestG)
    ).

%! filter_closed_successors(+Successors:list, +ClosedSet:list, -Filtered:list) is det.
%  Filtre les successeurs pour exclure ceux déjà dans le closed set (recherche binaire O(log n))
filter_closed_successors([], _, []).
filter_closed_successors([Node|Rest], ClosedSet, Filtered) :-
    node_state(Node, State),
    (   ord_memberchk(State, ClosedSet) ->
        % État déjà visité, l'exclure
        filter_closed_successors(Rest, ClosedSet, Filtered)
    ;   % État nouveau, le garder
        filter_closed_successors(Rest, ClosedSet, RestFiltered),
        Filtered = [Node|RestFiltered]
    ).


% =============================================================================
% SECTION 5: UTILITAIRES A*
% =============================================================================

%! create_successor_nodes(+States:list, +Goal:list, +G:int, +Parent:compound, -Nodes:list, +GenCountIn:int, -GenCountOut:int) is det.
%  Crée les nœuds successeurs avec évaluation f(n) = g(n) + h(n).
%
%  Incrémente le compteur de nœuds générés pour chaque successeur créé.
%
%  @arg States Liste des états successeurs possibles
%  @arg Goal État but (pour calcul heuristique)
%  @arg G Coût g(n) pour ces successeurs
%  @arg Parent Nœud parent (pour reconstruction chemin)
%  @arg Nodes Nœuds successeurs créés
%  @arg GenCountIn Compteur nœuds générés en entrée
%  @arg GenCountOut Compteur nœuds générés en sortie
create_successor_nodes([], _, _, _, [], GenCount, GenCount).
create_successor_nodes([State|RestStates], Goal, G, Parent, [Node|RestNodes], GenCountIn, GenCountOut) :-
    % Incrémenter le compteur pour ce nœud généré
    GenCountMid is GenCountIn + 1,
    % Créer le nœud avec évaluation heuristique
    manhattan_distance_heuristic(State, Goal, H),
    create_node(State, G, H, Parent, Node),
    % Continuer récursivement avec le compteur mis à jour
    create_successor_nodes(RestStates, Goal, G, Parent, RestNodes, GenCountMid, GenCountOut).

%! sort_open_list_by_f_value(+Nodes:list, -SortedNodes:list) is det.
%  Trie les nœuds par valeur f(n) croissante (file de priorité)
%  En cas d'égalité f(n), priorité au plus petit g(n) (tie-breaking)
%  @param Nodes Liste des nœuds à trier
%  @param SortedNodes Nœuds triés par f croissant, puis g croissant
sort_open_list_by_f_value(Nodes, SortedNodes) :-
    predsort(compare_node_f_values, Nodes, SortedNodes).

%! compare_node_f_values(-Order:atom, +Node1:compound, +Node2:compound) is det.
%  Fonction de comparaison pour le tri des nœuds A*
%  Priorité 1: Plus petit f(n) | Priorité 2: Plus petit g(n) (tie-breaking)
%  @param Order Résultat de comparaison (<, =, >)
%  @param Node1 Premier nœud à comparer
%  @param Node2 Second nœud à comparer
compare_node_f_values(Order, Node1, Node2) :-
    node_f_cost(Node1, F1),
    node_f_cost(Node2, F2),
    (   F1 =:= F2 ->
        % Égalité f(n) : départager par g(n) croissant
        node_g_cost(Node1, G1),
        node_g_cost(Node2, G2),
        compare(Order, G1, G2)
    ;   % Différence f(n) : trier par f(n) croissant
        compare(Order, F1, F2)
    ).

%! reconstruct_solution_path(+FinalNode:compound, -Path:list) is det.
%  Reconstruit le chemin solution par remontée des parents
%  @param FinalNode Nœud but atteint
%  @param Path Chemin depuis initial vers but (dans l'ordre correct)
reconstruct_solution_path(FinalNode, Path) :-
    reconstruct_path_helper(FinalNode, PathReversed),
    reverse(PathReversed, Path).

%! reconstruct_path_helper(+Node:compound, -Path:list) is det.
%  Helper récursif pour la reconstruction du chemin
reconstruct_path_helper(node(State, _, _, _, nil), [State]) :-
    % Cas de base : nœud racine (parent = nil)
    !.
reconstruct_path_helper(node(State, _, _, _, Parent), [State|RestPath]) :-
    % Récursion : ajouter état actuel et remonter au parent
    reconstruct_path_helper(Parent, RestPath).

%! extract_search_results(+Result:compound, -Path:list, -Cost:integer, -Expanded:integer) is det.
%  Extrait les résultats d'une recherche réussie
extract_search_results(search_success(FinalNode, _, GenCount), Path, Cost, Expanded) :-
    % Reconstruire le chemin solution
    reconstruct_solution_path(FinalNode, Path),
    % Extraire le coût (= profondeur du nœud final)
    node_g_cost(FinalNode, Cost),
    % Utiliser le comptage des nœuds générés selon énoncé TP1
    Expanded = GenCount.

% =============================================================================
% SECTION 6: INTERFACES PUBLIQUES
% =============================================================================

%! solve_puzzle(+TestCase:atom, -Result:compound) is det.
%  Interface principale pour résoudre les cas de test.
%
%  @arg TestCase Identifiant du cas (case1 | case2)
%  @arg Result Structure result(Path, Cost, Expanded)
%  @throws error(timeout, _) Si temps d'exécution >10s
%  @throws error(unsolvable, _) Si configuration impossible
%  @see astar_search/5 pour l'implémentation de l'algorithme
solve_puzzle(case1, result(Path, Cost, Expanded)) :-
    initial_state(Initial),
    goal_state(Goal),
    astar_search(Initial, Goal, Path, Cost, Expanded).

solve_puzzle(case2, result(Path, Cost, Expanded)) :-
    custom_initial_state(Initial),
    custom_goal_state(Goal),
    astar_search(Initial, Goal, Path, Cost, Expanded).

%! solve_custom_puzzle(+Initial:list, +Goal:list, -Result:compound) is det.
%  Interface pour résoudre des configurations personnalisées
%  @param Initial État de départ personnalisé
%  @param Goal État but personnalisé
%  @param Result Structure result(Path, Cost, Expanded)
solve_custom_puzzle(Initial, Goal, result(Path, Cost, Expanded)) :-
    astar_search(Initial, Goal, Path, Cost, Expanded).

% =============================================================================
% SECTION 7: DEBUG ET INSTRUMENTATION
% =============================================================================

%! debug_expansion_trace(+Node:compound, +Count:int, +ClosedSet:list) is det.
%  Affiche la trace de l'exploration A* pour prouver le calcul réel
%  Mode discret par défaut (peut être activé via flag debug_astar)
%  @param Node Nœud en cours d'exploration
%  @param Count Numéro du nœud exploré
%  @param ClosedSet États déjà explorés
debug_expansion_trace(Node, Count, ClosedSet) :-
    (   current_prolog_flag(debug_astar, true) ->
        % Mode debug activé - afficher la trace détaillée
        node_state(Node, State),
        node_g_cost(Node, G),
        node_h_cost(Node, H),
        node_f_cost(Node, F),
        length(ClosedSet, ClosedCount),
        format('[DEBUG] Expansion ~w : g=~w, h=~w, f=~w | ClosedSet=~w états~n',
               [Count, G, H, F, ClosedCount]),
        display_debug_state(State)
    ;   true  % Mode silencieux par défaut
    ).


%! enable_debug_mode is det.
%  Active le mode debug A* pour voir l'exploration en temps réel
enable_debug_mode :-
    set_prolog_flag(debug_astar, true),
    write('[INFO] Mode debug A* activé - trace exploration visible'), nl.

%! disable_debug_mode is det.
%  Désactive le mode debug A* pour performance optimale
disable_debug_mode :-
    set_prolog_flag(debug_astar, false),
    write('[INFO] Mode debug A* désactivé - mode silencieux'), nl.