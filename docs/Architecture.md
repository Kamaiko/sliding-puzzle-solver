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

## 📖 Guide de lecture du code pour l'équipe

### Ordre de lecture recommandé :
1. **game.pl** - Comprendre la représentation des états et mouvements
2. **astar.pl:1-30** - Lire les commentaires de structure et guide de lecture
3. **astar.pl:166-180** - Point d'entrée `astar_search/5`
4. **astar.pl:229-254** - Boucle principale `astar_main_loop/7`
5. **display.pl** - Interface utilisateur pour visualiser les résultats
6. **main.pl** - Orchestration et menu principal

### Points clés d'implémentation :
- **Ordre des mouvements** : OBLIGATOIRE = HAUT, BAS, GAUCHE, DROITE (game.pl:214)
- **Comptage nœuds** : Chaque nœud généré compte dans "Expanded" (astar.pl:338-339)
- **Heuristique** : Tuiles mal placées, case vide IGNORÉE (astar.pl:113-130)
- **Tie-breaking** : Si f(n) égaux, priorité au plus petit g(n) (astar.pl:360-370)

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
- Cas test 1 : Cost=4, Expanded=12 exact
- Cas test 2 : Configuration personnalisée

## 🔄 Diagramme 1 : Flux algorithmique A*

![Flux algorithmique A*](astar_flowchart_refined.svg)

**Points clés :** Ordre critique HAUT→BAS→GAUCHE→DROITE (game.pl:214) • Comptage précis GenCount++ (astar.pl:338) • Tie-breaking f(n) égaux → g(n) min (astar.pl:360)

## 🔧 Diagramme 2 : Gestion des états du taquin

![Gestion des états du taquin](taquin_states_structured.svg)

**Guide de lecture :** Suivez la timeline de progression à gauche (1→2→3→4→5→6) pour comprendre le flux complet de traitement des états. Chaque étape est connectée visuellement pour une lecture fluide et organisée.

## 🔀 Flux de données entre modules optimisé

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

% Nœud A* (structure algorithmique) - CORRECTION DE LA STRUCTURE
Node = node(State, G, H, F, Parent)
  % State = état du taquin [1,2,3,5,0,6,4,7,8]
  % G = coût réel g(n) depuis initial (profondeur)
  % H = heuristique h(n) vers but (tuiles mal placées)
  % F = fonction d'évaluation f(n) = g(n) + h(n)
  % Parent = nœud parent pour reconstruction chemin

% Résultat de recherche (interface de sortie)
Result = result(Path, Cost, Expanded)
  % Path = [État_Initial, État_Intermédiaire, ..., État_Final]
  % Cost = nombre de mouvements pour atteindre la solution
  % Expanded = nombre de nœuds générés selon énoncé TP1
```

## 🔌 Interfaces entre modules (Couplage minimal)

### game.pl → astar.pl
```prolog
% États de référence
initial_state(-State)           % [1,2,3,5,0,6,4,7,8]
goal_state(-State)              % [1,2,3,4,5,6,7,8,0]
custom_initial_state(-State)    % [2,0,3,1,4,6,7,5,8]

% Génération des successeurs (ORDRE CRITIQUE)
generate_moves(+State, -Successors)  % HAUT, BAS, GAUCHE, DROITE

% Validation états
valid_state(+State)             % Format 3×3, chiffres 0-8 uniques
is_solvable(+State, +Goal)      % Parité des inversions
states_equal(+State1, +State2)  % Test égalité pour but atteint
```

### astar.pl → main.pl
```prolog
% Interface principale de résolution
solve_puzzle(+TestCase, -result(Path, Cost, Expanded))
% TestCase = case1 | case2
% Result structure complète avec métriques A*

% Interface personnalisée
solve_custom_puzzle(+Initial, +Goal, -Result)
```

### main.pl → display.pl
```prolog
% Interface d'affichage complète
display_menu/0                      % Menu principal avec ASCII art
display_case1_banner(+Init, +Goal)  % Bannière cas test 1 avec états
display_case2_banner/0              % Bannière cas test 2
display_about_banner/0              % Section à propos

% Affichage résultats
display_solution(+Path, +Cost, +Expanded, +ResponseTime)
display_thinking_message/0          % Message pendant calcul
display_success_message/0           % Confirmation solution trouvée

% Gestion erreurs formatées
display_error(+Type, +Details)      % timeout, invalid_state, unsolvable
```

## 🧩 Points critiques d'implémentation

| Aspect | Localisation | Description critique |
|--------|-------------|---------------------|
| **Ordre mouvements** | `game.pl:214` | OBLIGATOIRE: `[up, down, left, right]` pour reproductibilité |
| **Comptage nœuds** | `astar.pl:338` | `GenCount++` pour chaque nœud créé = métrique "Expanded" |
| **Tie-breaking** | `astar.pl:360` | Si f(n) égaux → priorité g(n) minimum |
| **Heuristique** | `astar.pl:124` | Condition: `(mal_placée ET ≠ case_vide)` pour admissibilité |
| **Structure nœud** | `astar.pl:49` | `node(State, G, H, F, Parent)` - ordre des paramètres exact |
## ✅ Avantages architecture

- **🎯 Focus IA** : Algorithme A* centralisé dans astar.pl avec documentation technique détaillée
- **⚖️ Charge équilibrée** : 60-150 lignes par module permettant travail d'équipe efficace
- **🔗 Interfaces découplées** : Modules indépendants facilitant développement parallèle
- **📊 Structure académique** : Organisation claire pour évaluation et maintenance
- **🔍 Traçabilité** : Références de lignes de code dans documentation pour navigation rapide
- **🎓 Valeur éducative** : Exemples concrets et diagrammes spécifiques à l'implémentation

## 🧪 Validation empirique

### Tests critiques ✅ VALIDÉS
- **📋 Cas test académique** : Cost=4, Expanded=12 exactement selon énoncé TP1
- **⚡ Performance optimisée** : <3ms après warm-up JIT, temps reproductibles
- **🔬 Tests unitaires** : Validation isolée de chaque module (tests.pl)
- **🎯 Métriques exactes** : Comptage nœuds générés conforme spécifications

### Robustesse système
- **⏱️ Timeout sécurisé** : 10s limite avec gestion gracieuse des erreurs
- **✅ Validation entrées** : États vérifiés avant traitement (format + solvabilité)
- **📝 Messages informatifs** : Feedback utilisateur clair selon type d'erreur
- **🔄 Reproductibilité** : Résultats identiques garantis par ordre déterministe

## 📈 Recommandations pour coéquipiers

### Pour comprendre rapidement :
1. **Lire d'abord** le guide de lecture (section 📖)
2. **Examiner** les diagrammes spécifiques avec références aux lignes de code
3. **Tester** les cas validés pour confirmer compréhension
4. **Consulter** les points critiques d'implémentation pour éviter erreurs

### Pour debug efficacement :
- Activer le mode debug A* : `enable_debug_mode.` (astar.pl:465)
- Vérifier l'ordre des mouvements dans game.pl:214
- Contrôler le comptage dans astar.pl:338-339
- Valider l'heuristique avec le tableau des tuiles mal placées