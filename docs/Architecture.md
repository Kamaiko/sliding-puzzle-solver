# Architecture du système

## Vue d'ensemble

Le solveur de taquin est organisé en **6 modules Prolog indépendants** suivant une architecture modulaire anti-spaghetti.

```
    main.pl (Interface CLI)
       ↓
   search.pl (Algorithme A*)
    ↙    ↘
board.pl  heuristics.pl
    ↓
 moves.pl
    ↓
 utils.pl (Affichage)
```

## Modules et responsabilités

### 1. **main.pl** - Interface utilisateur
**Responsabilité** : Point d'entrée et orchestration
- Menu principal interactif
- Gestion des cas de test
- Affichage des résultats finaux
- Gestion des erreurs utilisateur

### 2. **board.pl** - Représentation des états
**Responsabilité** : Structure de données du puzzle
- États représentés comme listes : `[1,2,3,5,0,6,4,7,8]`
- Validation des états (9 éléments, 0-8 uniques)
- Manipulation positions et coordonnées
- Comparaison d'états

### 3. **moves.pl** - Génération des mouvements
**Responsabilité** : Transitions d'états valides
- 4 directions possibles (haut, bas, gauche, droite)
- Validation des limites du plateau 3x3
- Application des mouvements (échange tuiles)
- Détection des mouvements inverses

### 4. **heuristics.pl** - Fonctions d'évaluation
**Responsabilité** : Estimation distance au but
- **Heuristique principale** : Tuiles mal placées
- **Heuristique optionnelle** : Distance de Manhattan
- Interface unifiée pour sélection heuristique
- Optimisation performance calculs

### 5. **search.pl** - Algorithme A*
**Responsabilité** : Résolution optimale du puzzle
- File de priorité Best-First : f(n) = g(n) + h(n)
- Exploration systématique des successeurs
- Gestion des états visités (éviter cycles)
- Reconstruction du chemin solution
- Comptage précis des nœuds explorés

### 6. **utils.pl** - Utilitaires et affichage
**Responsabilité** : Interface utilisateur et formatage
- Affichage plateau 3x3 lisible
- Formatage des résultats (Path/Cost/Expanded)
- Temps de réponse IA
- Messages d'erreur informatifs

## Flux de données

### Résolution d'un puzzle
1. **main.pl** → Récupère état initial et final
2. **main.pl** → **search.pl** : Lance A* avec heuristique
3. **search.pl** → **board.pl** : Validation états
4. **search.pl** → **moves.pl** : Génération successeurs
5. **search.pl** → **heuristics.pl** : Évaluation f(n)
6. **search.pl** → **main.pl** : Retour chemin + statistiques
7. **main.pl** → **utils.pl** : Affichage résultats

### Structures de données principales

```prolog
% État du puzzle
State = [1,2,3,5,0,6,4,7,8]  % 0 = case vide

% Nœud A*
Node = node(State, F, G, Parent)
  % F = f(n) = g(n) + h(n)
  % G = profondeur (coût réel)
  % Parent = nœud parent pour reconstruction

% Résultat de recherche
Result = result(Path, Cost, Expanded)
  % Path = [État1, État2, ..., ÉtatFinal]
  % Cost = nombre de mouvements
  % Expanded = nœuds explorés
```

## Interfaces entre modules

### board.pl ↔ moves.pl
```prolog
board:get_empty_position(+State, -Position)
moves:generate_moves(+State, -Successors)
```

### search.pl ↔ heuristics.pl  
```prolog
heuristics:heuristic_value(+State, +Goal, +Type, -Value)
search:astar_search(+Initial, +Goal, +HType, -Path, -Cost, -Expanded)
```

### main.pl ↔ utils.pl
```prolog
utils:display_state(+Title, +State)
utils:display_solution(+Path, +Cost, +Expanded)
```

## Gestion des erreurs

### Niveaux d'erreur
1. **Erreur utilisateur** : État invalide, choix menu incorrect
2. **Erreur algorithme** : État impossible, timeout, limite nœuds
3. **Erreur système** : Mémoire insuffisante, fichier non trouvé

### Stratégie de robustesse
- **Validation précoce** : États vérifiés avant traitement
- **Timeout hybride** : 10s + limite 10k nœuds
- **Messages clairs** : Erreurs explicites pour utilisateur
- **Fallback gracieux** : Échec contrôlé sans crash

## Tests et validation

### Tests unitaires (par module)
- **board.pl** : Validation états, positions, coordonnées
- **moves.pl** : Génération mouvements, limites plateau
- **heuristics.pl** : Calculs exacts, états but
- **search.pl** : A* optimal, comptage nœuds
- **utils.pl** : Formatage, affichage

### Tests d'intégration
- **Cas professeur** : Validation exacte (9 nœuds, 4 mouvements)
- **Cas personnalisé** : Robustesse algorithme
- **Performance** : Temps < 1s pour puzzle 3x3

## Extensibilité

### Ajouts possibles
- **Tailles variables** : 4x4, 5x5 (modification board.pl)
- **Nouvelles heuristiques** : Pattern Database (heuristics.pl)  
- **Algorithmes alternatifs** : IDA*, bidirectionnel (search.pl)
- **Interface graphique** : GUI Tkinter (nouveau module)

### Architecture ouverte
- **Interfaces bien définies** : Modules faiblement couplés
- **Abstractions claires** : État, mouvement, heuristique
- **Configuration centralisée** : Paramètres dans main.pl