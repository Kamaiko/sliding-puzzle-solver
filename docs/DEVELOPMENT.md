# 🚀 Guide de Développement - Solveur de Taquin A*

## 📋 Vue d'ensemble
**Projet** : Solveur de Taquin 3x3 avec recherche heuristique A*  
**Cours** : IFT-2003 Intelligence Artificielle  
**Durée** : 2 semaines (25-30h total)  
**Équipe** : 4 développeurs  
**Deadline** : 20 octobre 2025

---

## 🛠️ Setup de développement

### Installation SWI-Prolog
```bash
# Windows
winget install SWI-Prolog.SWI-Prolog

# macOS
brew install swi-prolog

# Linux
sudo apt-get install swi-prolog
```

### Cloner le repository
```bash
git clone https://github.com/Kamaiko/sliding-puzzle-solver.git
cd sliding-puzzle-solver
```

### Lancer le programme
```bash
swipl src/main.pl
```

### Exécuter les tests
```bash
swipl src/tests.pl
?- run_all_tests.
```

---

## 📐 Architecture du projet

### Structure des modules
```
src/
├── main.pl       # Point d'entrée + orchestration CLI
├── game.pl       # Logique du taquin (états, mouvements)
├── astar.pl      # Algorithme A* et heuristiques
├── display.pl    # Affichage et formatage
└── tests.pl      # Tests unitaires et validation
```

### Interfaces entre modules
```prolog
% game.pl → astar.pl
initial_state(State).
goal_state(Goal).
generate_moves(State, Successors).

% astar.pl → main.pl
solve_puzzle(CaseNumber, result(Path, Cost, Expanded)).

% main.pl → display.pl
display_solution(Path, Cost, Expanded, ResponseTime).
```

---

## 🎯 Implémentation de l'algorithme A*

### Vue d'ensemble algorithmique
L'algorithme A* utilise une fonction d'évaluation f(n) = g(n) + h(n) où :
- **g(n)** = coût réel depuis l'état initial (profondeur)
- **h(n)** = heuristique (tuiles mal placées)
- **f(n)** = estimation du coût total

### Structure du nœud A*
```prolog
node(State, F, G, Parent)
% State  : Configuration [1,2,3,5,0,6,4,7,8]
% F      : f(n) = g(n) + h(n)
% G      : Profondeur depuis initial
% Parent : Référence au nœud parent pour backtracking
```

### Heuristique des tuiles mal placées
```prolog
misplaced_tiles(State, Goal, H) :-
    misplaced_count(State, Goal, 0, 0, H).

misplaced_count([], [], _, Count, Count).
misplaced_count([S|Ss], [G|Gs], Pos, Acc, H) :-
    ( S =\= 0, S =\= G ->  % Ignore la case vide
        Acc1 is Acc + 1
    ; Acc1 = Acc
    ),
    Pos1 is Pos + 1,
    misplaced_count(Ss, Gs, Pos1, Acc1, H).
```

### Algorithme principal
```prolog
astar_search(Initial, Goal, Path, Cost, Expanded) :-
    misplaced_tiles(Initial, Goal, H0),
    InitialNode = node(Initial, H0, 0, nil),
    astar_loop([InitialNode], Goal, [], Path, Cost, 0, Expanded).

astar_loop([CurrentNode|_], Goal, _, Path, Cost, Exp, Exp) :-
    CurrentNode = node(Goal, _, Cost, _),
    reconstruct_path(CurrentNode, Path), !.

astar_loop([Current|Rest], Goal, Closed, Path, Cost, ExpIn, ExpOut) :-
    Current = node(State, _, G, _),
    \+ member(State, Closed),
    generate_moves(State, Successors),
    G1 is G + 1,
    create_nodes(Successors, Goal, G1, Current, NewNodes),
    append(Rest, NewNodes, Open),
    sort_by_f(Open, Sorted),
    ExpNext is ExpIn + 1,
    astar_loop(Sorted, Goal, [State|Closed], Path, Cost, ExpNext, ExpOut).
```

### Validation critique - Cas test professeur
**IMPORTANT** : L'implémentation DOIT produire exactement :
- **État initial** : `[1,2,3,5,0,6,4,7,8]`
- **État final** : `[1,2,3,4,5,6,7,8,0]`
- **Path** : 5 états (A→B→C→D→E)
- **Cost** : 4 mouvements
- **Expanded** : 9 nœuds explorés

---

## 🔄 Workflow Git

### Branches
- `main` : Code stable et fonctionnel
- `dev-{nom}` : Branches personnelles de développement

### Commits
```bash
# Format des messages
git commit -m "feat(module): description courte"
git commit -m "fix(astar): correction calcul heuristique"
git commit -m "test(game): ajout tests mouvements"
git commit -m "docs: mise à jour README"
```

### Workflow quotidien
```bash
# Récupérer les derniers changements
git pull origin main

# Créer/basculer sur sa branche
git checkout -b dev-prenom

# Après modifications
git add .
git commit -m "feat(module): description"
git push origin dev-prenom

# Créer Pull Request sur GitHub
```

---

## 👥 Répartition des tâches

### DEV 1 - Algorithme A* (7-8h)
**Module** : `astar.pl`  
**Objectif** : Implémenter A* produisant EXACTEMENT Cost=4, Expanded=9

#### Responsabilités
- Structure `node(State, F, G, Parent)`
- File de priorité avec tri par f-value
- Heuristique tuiles mal placées
- Reconstruction du chemin
- Interface `solve_puzzle/2`

#### Livrables
- ✅ `astar.pl` fonctionnel
- ✅ Validation cas professeur exact
- ✅ Tests unitaires A*

---

### DEV 2 - Logique de jeu + Git (7-8h)
**Module** : `game.pl`  
**Objectif** : Mécanique complète du taquin + gestion repository

#### Responsabilités
- États initial/final/custom
- Validation configurations
- Génération mouvements (4 directions)
- Gestion repository Git
- Merge des contributions

#### Livrables
- ✅ `game.pl` complet
- ✅ Tests mouvements valides
- ✅ Repository propre

---

### DEV 3 - Interface CLI (7-8h)
**Module** : `main.pl`  
**Objectif** : Interface utilisateur et orchestration

#### Responsabilités
- Menu principal interactif
- Gestion cas de test
- Mesure temps de réponse
- Orchestration modules
- Guide utilisateur

#### Livrables
- ✅ CLI fonctionnel
- ✅ Intégration modules
- ✅ Documentation utilisateur

---

### DEV 4 - Affichage + Tests (7-8h)
**Modules** : `display.pl` + `tests.pl`  
**Objectif** : Qualité visuelle et validation complète

#### Responsabilités
- Affichage plateau 3x3 ASCII
- Formatage Path/Cost/Expanded
- Suite tests complète
- Documentation technique
- Validation croisée

#### Livrables
- ✅ Affichage professionnel
- ✅ Tests 100% modules
- ✅ Documentation à jour

---

## 🧪 Tests et validation

### Tests unitaires par module
```prolog
% game.pl
test_valid_state :- valid_state([1,2,3,4,5,6,7,8,0]).
test_find_blank :- find_blank([1,2,3,5,0,6,4,7,8], 4).
test_generate_moves :- generate_moves([1,2,3,5,0,6,4,7,8], Moves), length(Moves, 4).

% astar.pl
test_misplaced :- misplaced_tiles([1,2,3,5,0,6,4,7,8], [1,2,3,4,5,6,7,8,0], 4).
test_astar_case1 :- solve_puzzle(case1, result(_, 4, 9)).

% display.pl
test_display :- display_state("Test", [1,2,3,4,5,6,7,8,0]).
```

### Cas de test obligatoires

#### Test 1 - Exemple professeur (CRITIQUE)
```
Initial: 1 2 3    Final: 1 2 3
         5 * 6           4 5 6
         4 7 8           7 8 *
```
**Résultats EXACTS requis** :
- Path : 5 états
- Cost : 4 mouvements
- Expanded : 9 nœuds

#### Test 2 - Cas personnalisé
```
Initial: 2 8 3    Final: 1 2 3
         1 6 4           8 * 4
         7 * 5           7 6 5
```
**Critères** : Minimum 6 mouvements, solvable

### Commande de test
```bash
swipl src/tests.pl
?- run_all_tests.
?- test_case1_results.  % Validation critique
?- benchmark_performance.
```

---

## 📊 Métriques de performance

### Objectifs
- Temps résolution < 1 seconde pour 3x3
- Zéro erreur compilation/exécution
- Tests unitaires 100% passants
- Memory footprint < 100MB

### Benchmarks
```prolog
benchmark_performance :-
    get_time(Start),
    solve_puzzle(case1, _),
    get_time(End),
    Time is End - Start,
    format('Case 1: ~3f seconds~n', [Time]),
    Time < 1.0.
```

---

## 📅 Timeline de développement

### Semaine 1 : Développement indépendant
| Jour | Objectifs |
|------|-----------|
| **Lun-Mar** | Setup environnement + structure modules de base |
| **Mer-Jeu** | Implémentation cœur algorithme/logique |
| **Ven** | Tests unitaires + premiers tests intégration |

### Semaine 2 : Intégration et finalisation
| Jour | Objectifs |
|------|-----------|
| **Lun** | Intégration complète des modules |
| **Mar** | Validation cas test professeur (CRITIQUE) |
| **Mer-Jeu** | Optimisation + documentation + rapport |

### Points de synchronisation
- **Jour 3** : Validation interfaces entre modules (15 min)
- **Jour 7** : Test intégration complet (30 min)
- **Jour 12** : Validation finale avant soumission (1h)

---

## ⚠️ Points d'attention critiques

1. **Validation cas professeur** : Cost=4, Expanded=9 EXACTEMENT
2. **Heuristique** : Tuiles mal placées SANS la case vide
3. **Format état** : Liste [1,2,3,5,0,6,4,7,8] où 0=vide
4. **Performance** : < 1 seconde obligatoire
5. **Documentation** : Chaque prédicat documenté
6. **Tests** : 2 cas obligatoires validés

---

## 📚 Ressources

- [Documentation SWI-Prolog](https://www.swi-prolog.org/pldoc/)
- [Algorithme A* expliqué](https://en.wikipedia.org/wiki/A*_search_algorithm)
- [Énoncé TP1](../archive/TP1_Enonce_Reformule.md)
- [PRD du projet](PRD.md)
- [Architecture technique](Architecture.md)

---

**🎯 Rappel** : La validation du cas professeur avec les métriques exactes est CRITIQUE pour la note !