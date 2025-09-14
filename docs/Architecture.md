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
- États : `initial_state/1`, `goal_state/1`
- Mouvements : `generate_moves/2`, `apply_move/3`
- Format : `[1,2,3,5,0,6,4,7,8]` (0=vide)

### 3. **astar.pl** - Algorithme A* (~150 lignes)
- Structure : `node(State, F, G, Parent)`
- Heuristique : Tuiles mal placées (sans case vide)
- Interface : `solve_puzzle(Case, result(Path, Cost, Expanded))`
- File priorité : f(n) = g(n) + h(n)

### 4. **display.pl** - Affichage (~50 lignes)
- Plateau 3x3 ASCII avec bordures
- Formatage Path/Cost/Expanded/Temps
- Menu et bannière

### 5. **tests.pl** - Validation (~80 lignes)
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

### Tests critiques
- **Cas professeur** : Cost=4, Expanded=9 exactement
- **Performance** : < 1 seconde résolution 3x3
- **Tests unitaires** : Chaque module validé séparément

## Robustesse

- **Timeout** : 10s limite + 10k nœuds maximum
- **Validation** : États vérifiés avant traitement
- **Messages d'erreur** : Feedback utilisateur clair