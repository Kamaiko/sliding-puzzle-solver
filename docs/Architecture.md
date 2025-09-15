# 🏗️ Architecture Technique - Solveur de Taquin A*

## Vue d'ensemble

Architecture modulaire 4 composants optimisée pour l'évaluation académique IFT-2003.

```
    main.pl (Interface CLI + Orchestration)
       ↓
   astar.pl (Cœur Algorithmique A*)
    ↙    ↘
game.pl  display.pl
   ↓        ↓
tests.pl (Validation + Qualité)
```

## Modules et responsabilités

### 1. **main.pl** - Point d'entrée (~60 lignes)
- Menu CLI interactif 
- Orchestration modules
- Mesure temps de réponse

### 2. **game.pl** - Logique métier (~100 lignes)

**Fonctionnalités principales :**
- Définitions des états de référence (cas tests académiques)
- Validation complète des configurations (format et solvabilité)
- Génération de mouvements dans l'ordre strict requis
- Utilitaires de manipulation d'états optimisés
- Algorithmes de vérification de solvabilité

**Architecture des sections :**
1. Constantes de configuration grille 3×3
2. Définitions d'états de référence
3. Validation des configurations et solvabilité
4. Génération des mouvements (ordre critique)
5. Utilitaires de manipulation d'états

**Interfaces principales :**
- États : `initial_state/1`, `goal_state/1`, `custom_initial_state/1`
- Mouvements : `generate_moves/2`, `apply_move/3`
- Validation : `valid_state/1`, `is_solvable/2`
- Format : `[1,2,3,5,0,6,4,7,8]` (0=vide)

**IMPORTANT** : Ordre des mouvements OBLIGATOIRE = HAUT, BAS, GAUCHE, DROITE pour garantir la reproductibilité académique

### 3. **astar.pl** - Algorithme A* (~150 lignes)

**Fonctionnalités principales :**
- Algorithme A* complet avec open list et closed set
- Heuristique des tuiles mal placées (excluant case vide)
- Comptage précis des nœuds générés selon spécifications académiques
- Reconstruction optimale du chemin solution
- Gestion robuste d'erreurs et timeout de sécurité

**Architecture des sections :**
1. Structures de données et types
2. Heuristiques pour l'estimation
3. Algorithme A* principal avec closed set
4. Reconstruction du chemin solution
5. Interfaces de haut niveau
6. Mode debug et trace

**Interfaces principales :**
- Recherche : `astar_search/5`, `solve_puzzle/2`
- Heuristiques : `misplaced_tiles_heuristic/3`, `manhattan_distance_heuristic/3`
- Structure nœud : `node(State, G, H, F, Parent)`
- File priorité : f(n) = g(n) + h(n)

**CRITIQUE** : Le comptage "nœuds générés" correspond au "nombre total de nœuds générés et explorés" selon énoncé TP1

### 4. **display.pl** - Interface utilisateur (~50 lignes)

**Fonctionnalités principales :**
- Menu ASCII unifié avec titre stylisé et options
- Visualisation plateau 3×3 avec bordures UTF-8 propres
- Affichage séquentiel du chemin solution avec labels A→B→C→D→E
- Messages d'erreur formatés selon type (timeout, invalid_state, unsolvable)
- Feedback en temps réel durant l'exploration A*

**Architecture des sections :**
1. Bannières et menus principaux
2. Affichage d'états du taquin (format compact et détaillé)
3. Affichage des résultats et solutions
4. Messages d'erreur et feedback

**Interfaces principales :**
- Menu : `display_menu/0`
- États : `display_state/2`, `display_state_compact/1`
- Solutions : `display_solution/4` (Path, Cost, Expanded, ResponseTime)
- Erreurs : `display_error/2`, `display_thinking_message/0`
- Format : Case vide affichée comme "#" selon conventions

**IMPORTANT** : Utilise uniquement des caractères ASCII pour maximum compatibilité et professionnalisme

### 5. **main.pl** - Orchestration (~60 lignes)

**Fonctionnalités principales :**
- Point d'entrée principal avec gestion arguments ligne de commande
- Menu interactif avec choix utilisateur (1-4) et validation robuste
- Exécution des cas de test avec mesure précise de performance
- Gestion complète d'erreurs (timeout, états invalides, impossibles)
- Interface pour configurations personnalisées

**Architecture des sections :**
1. Points d'entrée et initialisation
2. Menu principal et navigation
3. Gestion des choix utilisateur
4. Exécution des cas de test
5. Utilitaires essentiels

**Interfaces principales :**
- Entrée : `main/0`, `main/1` (avec arguments)
- Navigation : `main_menu/0`, `handle_choice/1`
- Tests : `execute_test_case/1` (case1 | case2)
- Erreurs : `handle_execution_error/1`
- Personnalisé : `solve_custom/2`

**CRITIQUE** : Effectue un warm-up pour éliminer la compilation JIT et garantir des mesures de temps précises

### 6. **tests.pl** - Validation (~80 lignes)
- Tests unitaires par module
- Cas test 1 : Cost=4, Expanded=9 exact
- Cas test 2 : Configuration personnalisée

## Flux de données optimisé

### Résolution d'un puzzle (workflow principal)
1. **main.pl** → Lecture choix utilisateur et sélection cas de test
2. **main.pl** → **astar.pl** : `solve_puzzle(case1, Result)`
3. **astar.pl** → **game.pl** : `initial_state(State)`, `generate_moves(State, Successors)`
4. **astar.pl** → Calcul interne heuristiques et exploration A*
5. **astar.pl** → **main.pl** : `result(Path, Cost, Expanded)`
6. **main.pl** → **display.pl** : `display_solution(Path, Cost, Expanded, ResponseTime)`

### Structures de données principales

```prolog
% État du puzzle (représentation unique)
State = [1,2,3,5,0,6,4,7,8]  % 0 = case vide

% Nœud A* (structure algorithmique)
Node = node(State, F, G, Parent)
  % F = f(n) = g(n) + h(n) (fonction d'évaluation)
  % G = profondeur (coût réel depuis initial)
  % Parent = nœud parent pour reconstruction chemin

% Résultat de recherche (interface de sortie)
Result = result(Path, Cost, Expanded)
  % Path = [État_Initial, État_Intermédiaire, ..., État_Final]
  % Cost = nombre de mouvements pour atteindre la solution
  % Expanded = nombre de nœuds explorés (excluant initial)
```

## Interfaces entre modules (Couplage minimal)

### game.pl → astar.pl
```prolog
% États de référence
initial_state(-State)
goal_state(-State) 
custom_initial_state(-State)

% Génération des successeurs
generate_moves(+State, -Successors)

% Validation
is_goal(+State)
```

### astar.pl → main.pl
```prolog
% Interface principale de résolution
solve_puzzle(+TestCase, -result(Path, Cost, Expanded))
```

### main.pl → display.pl
```prolog
% Interface d'affichage
display_banner
display_menu
display_solution(+Path, +Cost, +Expanded, +ResponseTime)
```

## Avantages architecture

- **Focus IA** : Algorithme A* centralisé dans astar.pl
- **Charge équilibrée** : 60-150 lignes par module
- **Travail parallèle** : Interfaces découplées
- **Évaluation académique** : Structure claire

## Validation

### Tests critiques ✅ VALIDÉS
- **Cas professeur** : Cost=4, Expanded=9 exactement (comptage arbre visuel)
- **Performance** : < 1 seconde résolution 3x3
- **Tests unitaires** : Chaque module validé séparément
- **Solution "9 nœuds"** : Implémentation selon image ExempleResolution.png

## Robustesse

- **Timeout** : 10s limite + 10k nœuds maximum
- **Validation** : États vérifiés avant traitement
- **Messages d'erreur** : Feedback utilisateur clair