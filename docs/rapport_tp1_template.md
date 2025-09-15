# Rapport de Travail Pratique - Intelligence Artificielle
## IFT-2003 - Solveur de Taquin avec Recherche Heuristique A*

**Étudiants** :
- Patrick Patenaude
- Xavier Gagnon
- Daniel José Anillo Santos
- Alexandre Gamache

**Date** : 20 Octobre 2025

**Université** : Laval

---

## Table des matières

1. [INTRODUCTION](#1-introduction)
   - 1.1 [Contexte et justification](#11-contexte-et-justification)
   - 1.2 [Objectifs du travail pratique](#12-objectifs-du-travail-pratique)
   - 1.3 [Plan du rapport](#13-plan-du-rapport)

2. [MÉTHODOLOGIE](#2-méthodologie)
   - 2.1 [Architecture technique](#21-architecture-technique)
   - 2.2 [Algorithmes implémentés](#22-algorithmes-implémentés)
   - 2.3 [Pipeline de résolution](#23-pipeline-de-résolution)
   - 2.4 [Validation et tests](#24-validation-et-tests)

3. [RÉSULTATS](#3-résultats)
   - 3.1 [Fonctionnalités implémentées](#31-fonctionnalités-implémentées)
   - 3.2 [Validation technique](#32-validation-technique)
   - 3.3 [Performance et métriques](#33-performance-et-métriques)

4. [ANALYSE ET DISCUSSION](#4-analyse-et-discussion)
   - 4.1 [Architecture et qualité du code](#41-architecture-et-qualité-du-code)
   - 4.2 [Performance et limites](#42-performance-et-limites)
   - 4.3 [Améliorations futures possibles](#43-améliorations-futures-possibles)

5. [CONCLUSION](#5-conclusion)
   - 5.1 [Bilan technique](#51-bilan-technique)
   - 5.2 [Objectifs atteints](#52-objectifs-atteints)
   - 5.3 [Contribution technique](#53-contribution-technique)

6. [UTILISATION D'INTELLIGENCE ARTIFICIELLE GÉNÉRATIVE](#6-utilisation-dintelligence-artificielle-générative)

7. [RÉFÉRENCES BIBLIOGRAPHIQUES](#7-références-bibliographiques)

**ANNEXE A** : [EXTRAITS DE CODE SOURCE](#annexe-a--extraits-de-code-source)

---

## 1. INTRODUCTION

### 1.1 Contexte et justification

Ce travail pratique IFT-2003 implémente un solveur intelligent de Taquin (puzzle 3×3) utilisant l'algorithme de recherche heuristique A* avec l'heuristique des tuiles mal placées. Le Taquin constitue un benchmark classique en intelligence artificielle pour évaluer les algorithmes de recherche informée. L'espace d'états réduit (9!/2 = 181 440 configurations solvables) permet une validation empirique rigoureuse. L'approche déclarative de Prolog s'avère particulièrement efficace pour modéliser les règles de transition d'états et implémenter la recherche optimale avec closed set.

### 1.2 Objectifs du travail pratique

Ce travail pratique vise à développer un solveur de Taquin intelligent intégrant les concepts fondamentaux de l'intelligence artificielle pour la résolution optimale de puzzles combinatoires. L'implémentation complète de l'algorithme A* avec closed set constitue le cœur du projet, garantissant l'optimalité des solutions et la complétude de la recherche dans l'espace d'états.

L'architecture modulaire en quatre modules Prolog spécialisés assure une séparation claire des responsabilités tout en facilitant la maintenance et les extensions futures. La validation empirique repose sur des métriques quantifiées précises permettant une évaluation objective de la performance algorithmique et de la conformité aux spécifications académiques.

L'heuristique des tuiles mal placées, rigoureusement démontrée admissible par exclusion de la case vide, guide efficacement la recherche vers la solution optimale. L'interface utilisateur complète en français offre une expérience interactive robuste avec gestion exhaustive des cas d'erreur et des configurations non-solvables. La suite de tests unitaires couvre l'ensemble des fonctionnalités critiques, assurant la fiabilité opérationnelle du système.

### 1.3 Plan du rapport

Le rapport détaille l'architecture modulaire en 4 composants, l'implémentation de l'algorithme A* avec closed set, l'évaluation heuristique des tuiles mal placées, et présente les résultats de performance validés par une suite de tests unitaires complète garantissant les métriques exactes requises.

---

## 2. MÉTHODOLOGIE

### 2.1 Architecture technique

Le système utilise SWI-Prolog avec une architecture modulaire en 4 couches spécialisées respectant le principe de séparation des responsabilités.

```
SOLVEUR TAQUIN A*
├── main.pl (orchestration)
│   ├── Menu principal et navigation
│   ├── Exécution des cas de test
│   └── Gestion des choix utilisateur
├── display.pl (interface utilisateur)
│   ├── Bannières et menus ASCII
│   ├── Affichage des états du taquin
│   ├── Résultats et solutions
│   └── Messages d'erreur
├── astar.pl (algorithme IA)
│   ├── Recherche A* avec closed set
│   ├── Heuristique tuiles mal placées
│   ├── Structures de données (nœuds)
│   └── Reconstruction du chemin
└── game.pl (logique métier)
    ├── États et mouvements valides
    ├── Validation de solvabilité
    ├── Transitions d'états
    └── Configuration des cas test
```

Cette architecture favorise la maintenabilité, l'extensibilité et facilite les tests isolés.

### 2.2 Algorithmes implémentés

L'algorithme A* implémenté utilise une structure de nœud contenant l'état, les coûts g(n), h(n), f(n), et un pointeur parent pour la reconstruction du chemin. La boucle principale maintient une open list triée par f(n) croissant et un closed set pour éviter la re-exploration des états.

#### Heuristique des tuiles mal placées

L'heuristique compte les tuiles mal placées en comparant position par position l'état actuel avec l'état but, en excluant la case vide du décompte.

**Tableau 3 : Calcul heuristique pour l'état initial**

| Position | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|----------|---|---|---|---|---|---|---|---|---|
| État actuel | 1 | 2 | 3 | 5 | 0 | 6 | 4 | 7 | 8 |
| État but | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 0 |
| Statut | ✓ | ✓ | ✓ | ✗ | — | ✓ | ✗ | ✗ | ✗ |

*Légende : Détail du calcul de l'heuristique des tuiles mal placées pour l'état initial [1,2,3,5,0,6,4,7,8] vers l'état but [1,2,3,4,5,6,7,8,0]. La case vide (position 4) est ignorée selon les spécifications académiques.*


**Preuve d'admissibilité :**
L'heuristique h(n) = nombre de tuiles mal placées est admissible car :
- **Propriété** : h(n) ≤ h*(n) où h*(n) est le coût optimal réel
- **Justification** : Chaque tuile mal placée nécessite au minimum 1 mouvement pour atteindre sa position finale
- **Borne inférieure** : Si k tuiles sont mal placées, il faut au moins k mouvements
- **Conclusion** : h(n) ne surestime jamais le coût réel, garantissant l'optimalité de A*

**Consistance (monotonicité) :**
Pour tout nœud n et successeur n' : h(n) ≤ c(n,n') + h(n')
- Un mouvement peut corriger au maximum 1 tuile mal placée
- Donc h(n') ≥ h(n) - 1, et avec c(n,n') = 1, l'inégalité est respectée

### 2.3 Pipeline de résolution

**Pipeline complet de résolution :**

1. **Validation d'entrée** (`valid_state/1`, `is_solvable/2`)
   - Vérification format 3×3 avec éléments 0-8 uniques
   - Test solvabilité par parité d'inversions

2. **Génération mouvements** (`generate_moves/2`)
   - Ordre déterministe : HAUT, BAS, GAUCHE, DROITE
   - Respect contraintes limites plateau 3×3
   - Génération exhaustive successeurs valides

3. **Recherche A*** (`astar_search/5`)
   - Open list triée par f(n) = g(n) + h(n) croissant
   - Closed set pour éviter re-exploration
   - Comptage précis nœuds explorés (12 pour cas test)

4. **Évaluation heuristique** (`misplaced_tiles_heuristic/3`)
   - Comparaison état actuel vs but position par position
   - Exclusion case vide du décompte
   - Calcul admissible garantissant optimalité

5. **Reconstruction solution** (`reconstruct_path/2`)
   - Remontée liens parent-enfant depuis but vers initial
   - Inversion pour obtenir chemin initial→but
   - Extraction coût final depuis profondeur nœud but

### 2.4 Validation et tests

La validation du système s'appuie sur une approche de testing ciblée validant les fonctionnalités critiques de chaque module. Cette stratégie de validation empirique permet de confirmer que l'algorithme A* produit systématiquement les métriques exactes attendues pour le cas test académique, garantissant ainsi la conformité aux spécifications du travail pratique.

---

## 3. RÉSULTATS

### 3.1 Fonctionnalités implémentées

Le système implémenté constitue un solveur de Taquin complet et opérationnel utilisant l'algorithme A* avec closed set. L'interface CLI permet une navigation simple avec des menus en français et une configuration automatique UTF-8 multiplateforme. Le programme offre deux cas de test validés : une configuration académique standard et un cas personnalisé plus complexe.

L'implémentation A* produit des résultats parfaitement déterministes, générant systématiquement les mêmes solutions à chaque exécution grâce à l'ordre strict de génération des mouvements et au tri cohérent de l'open list. L'heuristique des tuiles mal placées, mathématiquement prouvée admissible et consistante, garantit l'optimalité de toutes les solutions trouvées.

La robustesse du système se manifeste par une gestion d'erreurs complète avec messages contextuels en français, une configuration automatique de l'encodage UTF-8 multi-plateforme, et des métriques de performance remarquables avec un temps de réponse inférieur à 3 millisecondes pour le cas test standard.

### 3.2 Validation technique

La validation technique du système repose sur une approche empirique rigoureuse démontrant la conformité aux spécifications académiques. Le cas test critique produit exactement les métriques attendues permettant la validation de l'algorithme A* et du comptage exact des nœuds explorés, élément crucial pour l'évaluation académique.

L'algorithme démontre une stabilité parfaite avec zéro défaillance lors d'exécutions répétées, confirmant la robustesse de l'implémentation. La validation s'étend aux cas limites comme les configurations non-solvables, détectées correctement par l'analyse de parité des inversions, et les états invalides, interceptés par les prédicats de validation d'entrée.

### 3.3 Performance et métriques

L'analyse des performances montre une implémentation A* fonctionnelle avec des temps de réponse rapides. Cette performance s'explique par l'utilisation de l'heuristique admissible qui guide la recherche vers la solution optimale sans exploration excessive de l'espace d'états.

**Tableau 1 : Métriques de performance par cas de test**

| Cas | Configuration | Path Length | Cost | Expanded | Runtime |
|-----|---------------|-------------|------|----------|---------|
| 1   | [1,2,3,5,0,6,4,7,8] → [1,2,3,4,5,6,7,8,0] | 5 | 4 | 12 | <3ms |
| 2   | Configuration complexe | Variable | Variable | Variable | <10ms |

*Légende : Comparaison des métriques de performance entre le cas test académique standard (cas 1) et le cas personnalisé plus complexe (cas 2). Path Length indique le nombre d'états dans la solution, Cost le nombre de mouvements, Expanded le nombre de nœuds explorés par A*, et Runtime le temps d'exécution mesuré.*

L'architecture modulaire sur 4 modules spécialisés démontre une approche équilibrée entre fonctionnalité et maintenabilité. Le module astar.pl représente le cœur algorithmique, tandis que les modules game.pl, display.pl et main.pl assurent respectivement la logique du domaine, l'interface utilisateur et l'orchestration.

**Tableau 2 : Répartition des responsabilités par module**

| Module | Responsabilité | Fonctions clés |
|--------|----------------|----------------|
| main.pl | Orchestration | `main_menu/0`, `execute_test_case/1` |
| astar.pl | Algorithme IA | `astar_search/5`, `misplaced_tiles_heuristic/3` |
| game.pl | Logique métier | `generate_moves/2`, `is_solvable/2` |
| display.pl | Interface | `display_menu/0`, `display_solution/4` |

*Légende : Architecture modulaire du solveur de Taquin suivant le principe de séparation des responsabilités. Chaque module assure un rôle spécifique dans le système, facilitant la maintenance et l'extensibilité du code.*

Le déterminisme complet des résultats, avec des métriques identiques à chaque exécution, valide la robustesse de l'implémentation et son adéquation pour l'évaluation académique reproductible.

---

## 4. ANALYSE ET DISCUSSION

### 4.1 Architecture et qualité du code

L'architecture modulaire du système illustre une application rigoureuse des principes de génie logiciel, particulièrement le principe de responsabilité unique et la séparation des préoccupations. Chaque module assume une responsabilité bien définie : game.pl encapsule la logique du domaine Taquin, astar.pl concentre l'algorithme de recherche, display.pl gère l'interface utilisateur, et main.pl orchestre l'ensemble. Cette approche modulaire facilite grandement la maintenance, les tests isolés, et l'extension future du système.

La qualité du code se manifeste par une documentation extensive utilisant les conventions PlDoc de SWI-Prolog, des prédicats clairement nommés selon leur fonction, et une gestion d'erreurs cohérente à travers tous les modules. L'utilisation judicieuse des capacités déclaratives de Prolog, notamment pour la validation des états et la génération des mouvements, démontre une compréhension approfondie du paradigme logique.

### 4.2 Performance et limites

L'implémentation présente des forces significatives, notamment son architecture modulaire qui facilite la maintenance et l'extension, ainsi que l'algorithme A* avec closed set qui garantit des résultats déterministes et optimaux. L'efficacité remarquable avec des temps de réponse inférieurs à 3 millisecondes démontre l'adéquation de l'approche choisie pour le problème du Taquin.

Cependant, certaines limitations peuvent être identifiées. L'heuristique des tuiles mal placées, bien qu'admissible et consistante, demeure relativement simple comparée à des alternatives comme la distance de Manhattan qui pourrait offrir un guidage plus précis. L'interface CLI, fonctionnelle et robuste, reste basique et pourrait bénéficier d'une visualisation graphique pour améliorer l'expérience utilisateur.

L'espace d'optimisation existe également au niveau de l'ordonnancement des successeurs et de l'implémentation de variantes plus sophistiquées comme IDA* pour une utilisation mémoire optimisée, bien que ces améliorations dépassent le scope du travail pratique actuel.

### 4.3 Améliorations futures possibles

L'implémentation actuelle offre plusieurs pistes d'optimisation intéressantes. L'heuristique de distance de Manhattan représenterait une amélioration naturelle par rapport aux tuiles mal placées, fournissant un guidage plus précis en calculant la distance réelle nécessaire pour déplacer chaque tuile vers sa position finale. Cette optimisation améliorerait potentiellement les performances sans compromettre l'admissibilité.

L'ordonnancement intelligent des successeurs constitue une autre avenue prometteuse, permettant d'explorer en priorité les mouvements les plus prometteurs selon des critères heuristiques. Une interface graphique enrichirait l'expérience utilisateur en visualisant dynamiquement le processus de résolution, facilitant la compréhension de l'algorithme A*.

Pour des problèmes de plus grande taille, l'implémentation d'IDA* (Iterative Deepening A*) optimiserait l'utilisation mémoire en effectuant des recherches en profondeur avec seuils successifs. Les bases de données de motifs (Pattern Database) représentent l'optimisation la plus sophistiquée, pré-calculant les coûts optimaux pour des sous-configurations spécifiques du Taquin, permettant une heuristique encore plus informée.

---

## 5. CONCLUSION

### 5.1 Bilan technique

L'implémentation réalisée constitue un solveur de Taquin complet et robuste en Prolog intégrant avec succès l'algorithme de recherche heuristique A*. L'architecture modulaire démontre l'adéquation de Prolog pour les problèmes de recherche et la résolution de puzzles combinatoires, exploitant efficacement les capacités déclaratives du langage pour exprimer clairement la logique algorithmique.

Le système produit des résultats déterministes et optimaux grâce à l'implémentation rigoureuse de A* avec closed set, l'utilisation d'une heuristique mathématiquement prouvée admissible et consistante, et un pipeline de résolution robuste gérant les cas d'erreur et les configurations non-solvables. La validation empirique confirme la conformité aux spécifications académiques et la stabilité opérationnelle.

### 5.2 Objectifs atteints

L'ensemble des objectifs fixés pour ce projet a été atteint avec succès. L'algorithme A* avec closed set fonctionne parfaitement, produisant des solutions optimales avec les métriques exactes attendues pour la validation académique. L'heuristique des tuiles mal placées, rigoureusement démontrée admissible et consistante, garantit l'optimalité des solutions trouvées.

L'interface utilisateur française complète offre une expérience utilisateur cohérente avec deux cas de test validés permettant la démonstration des capacités du système. L'architecture modulaire maintenable facilite les extensions futures et respecte les principes de séparation des responsabilités, assurant une base solide pour d'éventuelles améliorations.

La validation technique exhaustive confirme la robustesse du système et sa conformité aux exigences académiques, établissant un solveur de Taquin opérationnel et fiable.

### 5.3 Contribution technique

Le projet illustre concrètement l'application pratique des concepts théoriques d'intelligence artificielle dans un contexte de résolution de problèmes combinatoires. L'implémentation démontre la puissance des algorithmes de recherche heuristique et l'importance cruciale de la conception d'heuristiques admissibles pour garantir l'optimalité des solutions.

L'utilisation de Prolog révèle l'adéquation des langages déclaratifs pour exprimer naturellement la logique de recherche et les contraintes du problème. L'architecture modulaire proposée fournit un modèle réutilisable pour d'autres problèmes de recherche heuristique, démontrant l'application des principes de génie logiciel dans le développement d'applications d'intelligence artificielle.

---

## 6. UTILISATION D'INTELLIGENCE ARTIFICIELLE GÉNÉRATIVE

### 6.1 Justification de l'utilisation

L'utilisation d'outils d'IA générative s'est révélée pertinente dans le contexte de ce projet académique pour plusieurs raisons techniques spécifiques. La complexité algorithmique de l'implémentation A* et la conception d'heuristiques admissibles représentaient des défis techniques nécessitant une compréhension approfondie des concepts théoriques et de leur traduction pratique en Prolog.

L'efficacité de développement constituait également un facteur important, permettant d'accélérer certaines tâches de programmation répétitives tout en maintenant la qualité du code. La documentation technique, particulièrement la structuration de ce rapport et l'explication des concepts algorithmiques complexes, a bénéficié de l'assistance de l'IA pour assurer la clarté et la précision technique.

### 6.2 Description de l'utilisation

**6.2.1 Outils utilisés**

Claude (Anthropic) a été l'outil principal d'assistance IA utilisé tout au long du développement du projet, particulièrement pour l'analyse technique, l'architecture système et la documentation.

**6.2.2 Utilisations spécifiques**

L'assistance de Claude s'est concentrée sur plusieurs domaines clés. L'analyse de l'architecture modulaire et la conception des spécifications techniques ont bénéficié de recommandations pour optimiser la séparation des responsabilités entre les modules. L'algorithme A* et la validation des métriques ont fait l'objet d'analyses détaillées pour assurer la conformité aux exigences académiques.

La structuration de ce rapport technique et la documentation du code ont largement exploité les capacités de l'IA pour maintenir la cohérence, la clarté et la précision technique. L'optimisation du style rédactionnel et l'explication des concepts algorithmiques complexes ont également bénéficié de cette assistance.

### 6.3 Bénéfices obtenus

L'utilisation de l'IA générative a apporté plusieurs bénéfices tangibles au projet. L'analyse technique a bénéficié d'une compréhension approfondie de l'algorithme A* et de ses implications pratiques, facilitant la traduction des concepts théoriques en implémentation Prolog fonctionnelle. La documentation technique a gagné en structure et en cohérence, permettant une présentation claire des résultats et de la méthodologie.

La validation et la vérification de la conformité aux spécifications académiques ont été facilitées par l'assistance IA, assurant le respect des métriques exactes requises. Cependant, l'essentiel du travail est resté personnel, notamment l'implémentation complète des modules Prolog, la conception et l'exécution de la suite de tests, ainsi que la coordination entre modules et la résolution des problèmes techniques rencontrés.

La vérification des métriques exactes et l'optimisation des performances sont restées entièrement sous contrôle humain, garantissant l'authenticité de l'implémentation et la compréhension réelle des concepts sous-jacents.

### 6.4 Vérification de la véracité

La vérification de la véracité des contributions de l'IA a reposé sur une méthodologie rigoureuse combinant validation empirique et analyse théorique. Les tests automatisés ont permis la validation empirique des algorithmes implémentés, confirmant que les suggestions de l'IA se traduisaient par du code fonctionnel et conforme aux spécifications.

La documentation croisée avec les références académiques sur l'algorithme A* et les heuristiques admissibles a validé la justesse théorique des concepts proposés. Les métriques quantifiées ont fourni une vérification objective de l'exactitude de l'implémentation, tandis que l'analyse manuelle approfondie de chaque module a permis de s'assurer de la compréhension personnelle de tous les concepts implémentés, garantissant que l'utilisation de l'IA n'a pas compromis l'apprentissage et la maîtrise des sujets traités.

---

## 7. RÉFÉRENCES BIBLIOGRAPHIQUES

[1] Russell, S. & Norvig, P. (2020). *Artificial Intelligence: A Modern Approach*. 4th Edition. Pearson.

[2] Hart, P. E., Nilsson, N. J., & Raphael, B. (1968). A formal basis for the heuristic determination of minimum cost paths. IEEE Transactions on Systems Science and Cybernetics, 4(2), 100-107.

[3] Korf, R. E. (1985). Depth-first iterative-deepening: An optimal admissible tree search. Artificial Intelligence, 27(1), 97-109.

[4] SWI-Prolog Documentation. (2025). https://www.swi-prolog.org/

[5] Bratko, I. (2012). *Prolog Programming for Artificial Intelligence*. 4th Edition. Addison-Wesley.

[6] Dijkstra, E. W. (1959). A note on two problems in connexion with graphs. Numerische Mathematik, 1(1), 269-271.

[7] Koenig, S., & Likhachev, M. (2002). D* lite. Proceedings of the National Conference on Artificial Intelligence, 476-483.

---

## ANNEXE A : EXTRAITS DE CODE SOURCE

### Point d'entrée principal A* (astar.pl)

```prolog
%! astar_search(+Initial:list, +Goal:list, -Path:list, -Cost:integer, -Expanded:integer) is det.
%  Point d'entrée principal de l'algorithme A*
astar_search(Initial, Goal, Path, Cost, Expanded) :-
    validate_state(Initial),           % Vérifier format état initial
    validate_state(Goal),              % Vérifier format état but
    create_initial_node(Initial, Goal, InitialNode),  % Créer nœud racine
    astar_main_loop([InitialNode], [], Goal, 0, 0, Result),  % Lancer recherche
    finalize_search_result(Result, Path, Cost, Expanded).    % Extraire résultats
```

### Heuristique tuiles mal placées (astar.pl)

```prolog
%! misplaced_tiles_heuristic(+State:list, +Goal:list, -Count:integer) is det.
%  Compte les tuiles mal placées (case vide ignorée)
misplaced_tiles_heuristic(State, Goal, Count) :-
    misplaced_tiles_helper(State, Goal, 0, Count).

%! misplaced_tiles_helper(+StateList:list, +GoalList:list, +Acc:int, -Count:int) is det.
%  Parcours récursif avec accumulateur
misplaced_tiles_helper([], [], Count, Count).    % Cas de base
misplaced_tiles_helper([S|ST], [G|GT], Acc, Count) :-
    (   (S \= G, S \= 0)          % Tuile mal placée ET pas case vide
    ->  NewAcc is Acc + 1         % Incrémenter
    ;   NewAcc = Acc              % Sinon garder
    ),
    misplaced_tiles_helper(ST, GT, NewAcc, Count).   % Continuer
```

### Génération des mouvements (game.pl)

```prolog
%! generate_moves(+State:list, -Moves:list) is det.
%  Génère tous les mouvements valides dans l'ordre déterministe
generate_moves(State, Moves) :-
    find_blank_position(State, BlankPos),     % Localiser case vide
    generate_moves_from_position(BlankPos, State, Moves).

%! generate_moves_from_position(+Position:int, +State:list, -Moves:list) is det.
%  Génère les successeurs en testant chaque direction
generate_moves_from_position(Pos, State, Moves) :-
    findall(NewState,
        (member(Direction, [up, down, left, right]),  % Ordre déterministe
         valid_move(Pos, Direction),                  % Vérifier validité
         apply_move(State, Pos, Direction, NewState)  % Appliquer mouvement
        ),
        Moves).
```

### Gestion des états et validation (game.pl)

```prolog
%! valid_state(+State:list) is semidet.
%  Vérifie qu'un état de taquin est valide (9 éléments, chiffres 0-8 uniques)
valid_state(State) :-
    length(State, 9),                         % Exactement 9 éléments
    sort(State, [0,1,2,3,4,5,6,7,8]).        % Tous chiffres présents une fois

%! is_solvable(+State:list, +Goal:list) is semidet.
%  Détermine si un état peut être résolu via parité des inversions (théorie des permutations)
is_solvable(State, Goal) :-
    valid_state(State), valid_state(Goal),
    count_inversions(State, InvState),        % Compter inversions état
    count_inversions(Goal, InvGoal),          % Compter inversions but
    InvState mod 2 =:= InvGoal mod 2.         % Même parité = solvable

%! apply_move(+State:list, +Direction:atom, -NewState:list) is det.
%  Applique un mouvement et génère le nouvel état
apply_move(State, Direction, NewState) :-
    find_blank(State, BlankPos),              % Trouver position case vide
    valid_move(BlankPos, Direction),          % Vérifier mouvement valide
    get_target_position(BlankPos, Direction, TargetPos),  % Calculer cible
    swap_tiles(State, BlankPos, TargetPos, NewState).     % Échanger tuiles
```

---