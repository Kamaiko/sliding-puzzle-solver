# Architecture du système

## Vue d'ensemble

Le solveur de taquin est organisé en **4 modules Prolog optimisés** suivant une architecture modulaire équilibrée, spécialement conçue pour un projet d'Intelligence Artificielle académique.

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

### 1. **main.pl** - Interface utilisateur et orchestration
**Responsabilité** : Point d'entrée et coordination du système  
**Taille** : ~60 lignes  
**Dev responsable** : DEV 3

- Menu principal interactif (3 options)
- Point d'entrée `:- initialization(main, main)`
- Gestion des cas de test (case1, case2)
- Mesure des temps de réponse IA
- Orchestration des modules `game` + `astar` + `display`
- Gestion d'erreurs et validation utilisateur

### 2. **game.pl** - Logique du domaine (États et Mouvements)
**Responsabilité** : Mécanique complète du taquin  
**Taille** : ~100 lignes  
**Dev responsable** : DEV 2

- **États de référence** : `initial_state/1`, `goal_state/1`, `custom_initial_state/1`
- **Validation** : `valid_state/1`, `find_blank/2`
- **Mouvements** : `generate_moves/2`, `apply_move/3`, `valid_move/2`
- **Utilitaires** : Conversions positions ↔ coordonnées
- **Format état** : `[1,2,3,5,0,6,4,7,8]` où 0 = case vide

### 3. **astar.pl** - Cœur algorithmique (Intelligence Artificielle)
**Responsabilité** : Algorithme A* complet avec heuristiques intégrées  
**Taille** : ~150 lignes  
**Dev responsable** : DEV 1 ⭐

- **Structure nœud** : `node(State, F, G, Parent)`
- **File de priorité** : Best-First avec f(n) = g(n) + h(n)
- **Heuristique principale** : Tuiles mal placées (excluant case vide)
- **Heuristique optionnelle** : Distance de Manhattan
- **Exploration systématique** : Génération de tous les successeurs
- **Gestion états visités** : Éviter cycles et redondances
- **Reconstruction chemin** : Backtracking depuis solution vers initial
- **Interface principale** : `solve_puzzle(CaseNumber, result(Path, Cost, Expanded))`

### 4. **display.pl** - Présentation et interface utilisateur
**Responsabilité** : Affichage formaté et expérience utilisateur  
**Taille** : ~50 lignes  
**Dev responsable** : DEV 4

- **Bannière** : Interface professionnelle avec titre
- **Menu principal** : Options claires et navigation
- **Affichage plateau 3x3** : Formatage ASCII avec bordures
- **Résultats solution** : Path/Cost/Expanded/Temps de réponse
- **Chemin solution** : Affichage étape par étape A→B→C→D→E
- **Messages d'erreur** : Feedback utilisateur informatif

### 5. **tests.pl** - Validation et qualité
**Responsabilité** : Suite de tests complète et benchmarks  
**Taille** : ~80 lignes  
**Dev responsable** : DEV 4

- **Tests unitaires** : Validation de chaque module individuellement
- **Tests d'intégration** : Validation du workflow complet
- **Cas de test 1** : Validation exacte (Cost=4, Expanded=9)
- **Cas de test 2** : Configuration personnalisée (6+ mouvements)
- **Tests de robustesse** : États impossibles, gestion d'erreurs
- **Benchmarks** : Performance et temps de réponse

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

## Avantages de cette architecture

### 🎯 **Focus IA évident**
- **astar.pl** contient tout l'algorithme A* au même endroit
- Facilite l'évaluation académique (prof trouve immédiatement le cœur)
- Évite la fragmentation algorithmique

### ⚖️ **Charge équilibrée**
- **main.pl** : 60 lignes (interface + orchestration)
- **game.pl** : 100 lignes (domaine + logique métier)
- **astar.pl** : 150 lignes (algorithme A* complet)
- **display.pl** : 50 lignes (présentation)
- **tests.pl** : 80 lignes (validation)

### 🚀 **Travail parallèle optimal**
- Interfaces bien définies permettent développement indépendant
- Chaque dev peut tester son module isolément
- Intégration facilitée par les contrats clairs

### 📚 **Conventions académiques**
- Structure claire pour évaluation par le professeur
- Séparation domaine/algorithme/présentation
- Documentation et tests intégrés

## Tests et validation

### Tests unitaires (par module)
- **game.pl** : États valides, mouvements corrects, validation
- **astar.pl** : Heuristiques exactes, A* optimal, reconstruction chemin
- **display.pl** : Formatage correct, affichage plateau 3x3
- **main.pl** : Menu fonctionnel, intégration modules
- **tests.pl** : Meta-tests, couverture complète

### Tests d'intégration critiques
- **Cas professeur** : Validation exacte **Cost=4, Expanded=9**
- **Cas personnalisé** : Configuration complexe (6+ mouvements)
- **Performance** : Temps < 1s pour résolution taquin 3x3
- **Robustesse** : Gestion états impossibles

## Gestion des erreurs

### Stratégie de robustesse
- **Validation précoce** : États vérifiés avant traitement algorithmique
- **Timeout intelligent** : 10s limite + 10k nœuds maximum
- **Messages clairs** : Erreurs explicites pour utilisateur
- **Fallback gracieux** : Échec contrôlé sans crash système

### Niveaux d'erreur
1. **Erreur utilisateur** : Choix menu invalide, entrée incorrecte
2. **Erreur domaine** : État invalide, mouvement impossible
3. **Erreur algorithme** : État impossible, timeout, limite mémoire

## Extensibilité future

### Ajouts algorithmiques possibles
- **IDA*** : Recherche en profondeur itérative (astar.pl)
- **Recherche bidirectionnelle** : Exploration simultanée (nouveau module)
- **Pattern Database** : Heuristiques sophistiquées (heuristics_advanced.pl)

### Extensions domaine
- **Tailles variables** : 4x4, 5x5 (modification game.pl)
- **Générateur puzzles** : Niveaux difficulté (generator.pl)
- **Mode interactif** : Jeu manuel utilisateur (interactive.pl)

### Architecture ouverte
- **Modules faiblement couplés** : Interfaces bien définies
- **Abstractions claires** : État, mouvement, recherche, affichage
- **Configuration centralisée** : Paramètres dans main.pl
- **Tests distribués** : Validation à tous les niveaux