/** <module> Recherche A* avec heuristique tuiles mal placées

Algorithme de recherche avec liste ouverte et ensemble fermé.
Garantit une solution optimale pour les configurations solvables.

@complexité Temps et espace O(b^d), b≈3, d≤31 pour taquin 3×3
@sections
  1. Constantes et configuration
  2. Types et structures de données
  3. Heuristiques pour l'estimation
  4. Cœur algorithme A* (SECTION ÉVALUÉE)
  5. Utilitaires A*
  6. Interfaces publiques
  7. Debug et instrumentation

%% GUIDE DE LECTURE DU MODULE
%% ==========================
%%
%% Pour comprendre l'algorithme A*, commencez par :
%% 1. Section 2 - Structure node/5 et son utilisation
%% 2. Section 3 - Heuristique misplaced_tiles
%% 3. Section 6 - astar_search/5 (point d'entrée)
%% 4. Section 4 - astar_loop/9 (cœur de l'algorithme)
%%
%% Points clés de l'implémentation :
%% - Open list : Liste triée par f(n) croissant
%% - Closed set : Liste des états déjà explorés
%% - Comptage : Chaque nœud généré est compté
%% - Tie-breaking : Si f égaux, priorité au plus petit g
*/

:- encoding(utf8).

:- consult(game).

% =============================================================================
% SECTION 1: CONSTANTES ET CONFIGURATION
% =============================================================================

%! max_timeout(-Seconds:float) is det.
%  Temps maximum d'exécution avant timeout (10 secondes)
max_timeout(10.0).


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
%  Heuristique principale : nombre de tuiles mal placées
%  IMPORTANT: Case vide (0) ignorée selon spécifications académiques
%  @param State État actuel du taquin
%  @param Goal État but à atteindre
%  @param Count Nombre de tuiles dans mauvaise position (h(n))
%
%  EXEMPLE DE CALCUL:
%  État: [1,2,3,5,0,6,4,7,8] vs But: [1,2,3,4,5,6,7,8,0]
%
%  Position | État | But | Comparaison
%  ---------|------|-----|-------------
%     0     |  1   |  1  |     ✓
%     1     |  2   |  2  |     ✓
%     2     |  3   |  3  |     ✓
%     3     |  5   |  4  |     ✗ (mal placée)
%     4     |  0   |  5  |  IGNORÉE (case vide)
%     5     |  6   |  6  |     ✓
%     6     |  4   |  7  |     ✗ (mal placée)
%     7     |  7   |  8  |     ✗ (mal placée)
%     8     |  8   |  0  |     ✗ (mal placée)
%
%  RÉSULTAT: h(état_initial) = 4 tuiles mal placées
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


% =============================================================================
% SECTION 4: CŒUR ALGORITHME A* (SECTION ÉVALUÉE)
% =============================================================================

%! astar_search(+Initial:list, +Goal:list, -Path:list, -Cost:integer, -Expanded:integer) is det.
%  ALGORITHME A* - Point d'entrée principal
%
%  VUE D'ENSEMBLE DE L'ALGORITHME:
%  ===============================
%
%  1. INITIALISATION
%     Open = [nœud_initial avec f(n) = g(0) + h(initial)]
%     Closed = []
%     Compteurs = 0
%
%  2. BOUCLE PRINCIPALE (astar_loop)
%     TANT QUE Open ≠ ∅:
%       n = extract_min_f(Open)    // Nœud avec plus petit f(n)
%       SI n.state = goal: SUCCESS → reconstruction chemin
%       Closed ← Closed ∪ {n.state}
%       POUR CHAQUE successeur s de n:
%         SI s ∉ Closed:
%           Open ← Open ∪ {create_node(s, g+1, h(s), n)}
%           counter_generated++
%
%  3. RECONSTRUCTION
%     Path = remonter_parents(nœud_final) → [état_initial, ..., état_final]
%
%  @param Initial État de départ
%  @param Goal État à atteindre
%  @param Path Chemin solution (liste des états depuis initial vers goal)
%  @param Cost Coût de la solution (nombre de mouvements)
%  @param Expanded Nombre de nœuds explorés selon comptage académique
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
    misplaced_tiles_heuristic(Initial, Goal, InitialH),
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
%  BOUCLE PRINCIPALE DE L'ALGORITHME A*
%
%  Cette boucle implémente l'algorithme A* standard avec les optimisations suivantes:
%  - File de priorité triée par f(n) croissant avec tie-breaking sur g(n)
%  - Closed set pour éviter la re-exploration des états
%  - Comptage précis des nœuds générés selon spécifications académiques
%  - Timeout de sécurité pour éviter les boucles infinies
%
%  ÉTAPES DE CHAQUE ITÉRATION:
%  1. Vérifier timeout de sécurité
%  2. Extraire le nœud avec le plus petit f(n)
%  3. Tester si le but est atteint
%  4. Vérifier si l'état n'est pas déjà dans closed set
%  5. Ajouter l'état au closed set et incrémenter compteur expanded
%  6. Générer tous les successeurs valides
%  7. Créer les nœuds successeurs et incrémenter compteur generated
%  8. Ajouter à l'open list et trier par f(n)
%  9. Récursion avec les nouvelles structures

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
%  Vérifie si un état est déjà dans le closed set
is_state_in_closed_set(State, ClosedSet) :-
    member(State, ClosedSet).

%! expand_current_node(+CurrentNode:compound, +RestOpen:list, +ClosedSet:list, +Goal:list, +StartTime:float, +ExpCount:int, +GenCount:int, -Result:compound) is det.
%  Expanse le nœud courant en générant ses successeurs
expand_current_node(CurrentNode, RestOpen, ClosedSet, Goal, StartTime, ExpCount, GenCount, Result) :-
    % Incrémenter le compteur de nœuds explorés
    NewExpCount is ExpCount + 1,

    % Afficher trace de debug si activé
    debug_expansion_trace(CurrentNode, NewExpCount, ClosedSet),

    % Ajouter l'état courant au closed set
    node_state(CurrentNode, CurrentState),
    NewClosedSet = [CurrentState|ClosedSet],

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
    % Ajouter les successeurs à l'open list
    append(RestOpen, SuccessorNodes, UpdatedOpenList),

    % Trier par f(n) croissant (avec tie-breaking sur g(n))
    sort_open_list_by_f_value(UpdatedOpenList, SortedOpenList),

    % Continuer la recherche avec les structures mises à jour
    astar_main_loop(SortedOpenList, ClosedSet, Goal, StartTime, ExpCount, GenCount, Result).

% =============================================================================
% SECTION 5: UTILITAIRES A*
% =============================================================================

%! create_successor_nodes(+States:list, +Goal:list, +G:int, +Parent:compound, -Nodes:list, +GenCountIn:int, -GenCountOut:int) is det.
%  Crée les nœuds successeurs avec évaluation f(n) pour tous états
%  COMPTAGE CRITIQUE : Incrémente le compteur pour chaque nœud généré selon énoncé TP1
%  @param States Liste des états successeurs possibles
%  @param Goal État but (pour calcul heuristique)
%  @param G Coût g(n) pour ces successeurs
%  @param Parent Nœud parent (pour reconstruction chemin)
%  @param Nodes Nœuds successeurs créés
%  @param GenCountIn Compteur nœuds générés en entrée
%  @param GenCountOut Compteur nœuds générés en sortie (incremente)
create_successor_nodes([], _, _, _, [], GenCount, GenCount).
create_successor_nodes([State|RestStates], Goal, G, Parent, [Node|RestNodes], GenCountIn, GenCountOut) :-
    % Incrémenter le compteur pour ce nœud généré
    GenCountMid is GenCountIn + 1,
    % Créer le nœud avec évaluation heuristique
    misplaced_tiles_heuristic(State, Goal, H),
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
%  Interface principale pour résoudre les cas de test académiques
%  @param TestCase Identifiant du cas (case1 | case2)
%  @param Result Structure result(Path, Cost, Expanded)
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

%! display_debug_state(+State:list) is det.
%  Affiche l'état sous forme compacte pour le debug
display_debug_state([A,B,C,D,E,F,G,H,I]) :-
    format('         ~w ~w ~w~n', [A,B,C]),
    format('         ~w ~w ~w~n', [D,E,F]),
    format('         ~w ~w ~w~n', [G,H,I]).

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