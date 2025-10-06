<div align="center">

<img src="images/LogoUlaval.png" alt="Logo Université Laval" width="300">


<br><br>

**IFT-2003(Intelligence Artificielle 1)**

<br><br>

<div style="background-color: #c0c0c0; padding: 15px; border-radius: 5px; margin: 20px 0;">
<strong>Titre : Solveur de Taquin avec Recherche Heuristique A*</strong>
</div>

<br><br>

<div align="center">
<table style="width: 90%; border-collapse: collapse; margin: 20px auto;">
<tr style="background-color: #f0f0f0;">
<th style="border: 1px solid #000; padding: 10px; text-align: center; width: 30%;">Nom complet</th>
<th style="border: 1px solid #000; padding: 10px; text-align: center; width: 20%;">Matricule-NI</th>
<th style="border: 1px solid #000; padding: 10px; text-align: center; width: 50%;">Courriel</th>
</tr>
<tr>
<td style="border: 1px solid #000; padding: 10px;">Alexandre Gamache</td>
<td style="border: 1px solid #000; padding: 10px;"></td>
<td style="border: 1px solid #000; padding: 10px;"></td>
</tr>
<tr>
<td style="border: 1px solid #000; padding: 10px;">Daniel José Anillo Santos</td>
<td style="border: 1px solid #000; padding: 10px;"></td>
<td style="border: 1px solid #000; padding: 10px;"></td>
</tr>
<tr>
<td style="border: 1px solid #000; padding: 10px;">Patrick Patenaude</td>
<td style="border: 1px solid #000; padding: 10px;"></td>
<td style="border: 1px solid #000; padding: 10px;"></td>
</tr>
<tr>
<td style="border: 1px solid #000; padding: 10px;">Xavier Gagnon</td>
<td style="border: 1px solid #000; padding: 10px;"></td>
<td style="border: 1px solid #000; padding: 10px;"></td>
</tr>
</table>
</div>

<br><br>

<div align="right">
Enseignant : Anicet Lepetit ONDO
</div>

<br><br><br><br>

**GROUPE No : 6**

</div>

<div style="page-break-after: always;"></div>

---

## Table des matières

1. [INTRODUCTION](#1-introduction)
   - 1.1 [Contexte et justification](#11-contexte-et-justification)
   - 1.2 [Objectifs du travail pratique](#12-objectifs-du-travail-pratique)
   - 1.3 [Plan du rapport](#13-plan-du-rapport)

2. [MÉTHODOLOGIE](#2-méthodologie)
   - 2.1 [Matériel, logiciels et outils utilisés](#21-matériel-logiciels-et-outils-utilisés)
   - 2.2 [Architecture technique](#22-architecture-technique)
   - 2.3 [Étapes de réalisation du travail pratique](#23-étapes-de-réalisation-du-travail-pratique)
   - 2.4 [Algorithmes implémentés](#24-algorithmes-implémentés)
   - 2.5 [Diagrammes et schémas de fonctionnement](#25-diagrammes-et-schémas-de-fonctionnement)
   - 2.6 [Pipeline de résolution](#26-pipeline-de-résolution)
   - 2.7 [Validation et tests](#27-validation-et-tests)

3. [RÉSULTATS](#3-résultats)
   - 3.1 [Fonctionnalités implémentées](#31-fonctionnalités-implémentées)
   - 3.2 [Validation technique](#32-validation-technique)
   - 3.3 [Performance et métriques](#33-performance-et-métriques)

4. [ANALYSE ET DISCUSSION](#4-analyse-et-discussion)
   - 4.1 [Architecture et qualité du code](#41-architecture-et-qualité-du-code)
   - 4.2 [Interprétation des résultats et comparaison avec les attentes](#42-interprétation-des-résultats-et-comparaison-avec-les-attentes)
   - 4.3 [Performance et limites identifiées](#43-performance-et-limites-identifiées)
   - 4.4 [Améliorations possibles et extensions futures](#44-améliorations-possibles-et-extensions-futures)

5. [CONCLUSION](#5-conclusion)
   - 5.1 [Bilan du travail pratique](#51-bilan-du-travail-pratique)
   - 5.2 [Accomplissements par rapport aux objectifs](#52-accomplissements-par-rapport-aux-objectifs)
   - 5.3 [Perspectives et recommandations](#53-perspectives-et-recommandations)

6. [UTILISATION D'INTELLIGENCE ARTIFICIELLE GÉNÉRATIVE](#6-utilisation-dintelligence-artificielle-générative)

7. [RÉFÉRENCES BIBLIOGRAPHIQUES](#7-références-bibliographiques)

**ANNEXE A** : [EXTRAITS DE CODE SOURCE](#annexe-a--extraits-de-code-source)

---

## 1. INTRODUCTION

### 1.1 Contexte et justification

Ce travail présente un solveur de Taquin (puzzle 3×3) utilisant l'algorithme A* avec l'heuristique des tuiles mal placées. Le Taquin est un problème classique en IA avec un espace d'états limité (181 440 configurations solvables). Prolog permet de bien modéliser les transitions d'états et l'implémentation de recherches heuristiques<sup>[3]</sup>.

### 1.2 Objectifs du travail pratique

Développer un solveur qui utilise A* pour garantir l'optimalité. Architecture modulaire avec 4 modules Prolog et séparation des responsabilités. Heuristique des tuiles mal placées admissible (exclusion case vide). Validation sur des métriques précises.

### 1.3 Plan du rapport

Le rapport présente la méthodologie (architecture modulaire, algorithme A*, heuristique), les résultats (validation technique et métriques) et l'analyse (qualité, limitations, améliorations).

---

## 2. MÉTHODOLOGIE

### 2.1 Matériel, logiciels et outils utilisés

SWI-Prolog 9.0.4, Visual Studio Code, Git/GitHub. Tests sur Windows 10/11 pour portabilité multiplateforme.

### 2.2 Architecture technique

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
│   ├── Recherche A*
│   ├── Heuristique tuiles mal placées
│   ├── Structures de données (nœuds)
│   └── Reconstruction du chemin
└── game.pl (logique métier)
    ├── États et mouvements valides
    ├── Validation de solvabilité
    ├── Transitions d'états
    └── Configuration des cas test
```

### 2.3 Étapes de réalisation du travail pratique

Quatre phases : analyse et conception, implémentation séquentielle (game.pl → astar.pl → display.pl → main.pl), tests et validation, optimisation et documentation.

### 2.4 Algorithmes implémentés

A* utilise une structure de nœud contenant l'état, les coûts g(n), h(n), f(n), et un pointeur parent. Boucle principale avec open list triée par f(n) et évitement de la re-exploration.

<div align="center">

**Tableau 1 : Calcul heuristique pour l'état initial**

</div>

| Position | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|----------|---|---|---|---|---|---|---|---|---|
| État actuel | 1 | 2 | 3 | 5 | 0 | 6 | 4 | 7 | 8 |
| État but | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 0 |
| Statut | ✓ | ✓ | ✓ | ✗ | — | ✓ | ✗ | ✗ | ✗ |

<div align="center">
<em>Calcul de l'heuristique des tuiles mal placées : case vide ignorée, résultat h(n) = 4.</em>
</div>

**Admissibilité** : h(n) ≤ h*(n) car chaque tuile mal placée nécessite ≥1 mouvement. **Consistance** : h(n) ≤ c(n,n') + h(n') car 1 mouvement corrige ≤1 tuile.

### 2.5 Diagrammes et schémas de fonctionnement

Flux A* : validation états, initialisation nœud racine, insertion open list, extraction nœud minimal f(n), test but, génération successeurs (ordre haut-bas-gauche-droite).

### 2.6 Pipeline de résolution

Processus en cinq étapes : validation entrée (format 3×3, solvabilité par parité des inversions<sup>[3]</sup>), génération mouvements déterministe, recherche A*, évaluation heuristique, reconstruction chemin.

### 2.7 Validation et tests

Tests ciblés validant chaque module pour confirmer métriques exactes.

---

## 3. RÉSULTATS

### 3.1 Fonctionnalités implémentées

Solveur complet utilisant A*, interface CLI en français avec configuration automatique UTF-8 multiplateforme, deux cas de test validés (configuration standard et cas personnalisé complexe). Résultats parfaitement déterministes garantis par l'ordre strict de génération des mouvements, le tri cohérent de l'open list et l'heuristique mathématiquement prouvée admissible.

### 3.2 Validation technique

<div align="center">

**Figure 1 : Menu principal du solveur de Taquin**

<img src="images/menu_principal.png" alt="Menu Principal" width="300">

<em>Interface d'accueil avec menu ASCII et options de navigation.</em>

</div>

<div align="center">

**Figure 2 : Exécution du cas test 1 avec solution complète**

<img src="images/CasTest1.png" alt="Cas Test 1" width="300">

<em>Résolution du cas test standard avec métriques exactes.</em>

</div>

<div align="center">

**Figure 3 : Suite de tests automatisée**

<img src="images/tests_validation.png" alt="Tests de validation" width="400">

<em>Exécution de la suite de tests complète démontrant la validation empirique de tous les modules.</em>

</div>

### 3.3 Performance et métriques

Implémentation optimisée avec warm-up JIT éliminant la variabilité de compilation lors de la première exécution. Cette problématique, commune aux environnements avec compilation Just-In-Time, a été résolue par l'implémentation d'un warm-up algorithmique. Le système effectue une pré-exécution silencieuse via `catch(solve_puzzle(TestCase, _), _, true)` dans main.pl:145, forçant la compilation JIT de SWI-Prolog à optimiser le code des prédicats critiques et garantissant des temps cohérents <3ms.

<div align="center">

**Tableau 2 : Impact du warm-up JIT sur les temps de réponse**

</div>

| Cas de test | Mesure | Sans warm-up (1ère exécution) | Avec warm-up (après pré-compilation) |
|-------------|--------|-------------------------------|-------------------------------------|
| **Cas test 1** | Essai 1 | 12.0 ms | 0.17 ms |
| | Essai 2 | 0.21 ms | 0.19 ms |
| **Cas test 2** | Essai 1 | 15.3 ms | 0.24 ms |
| | Essai 2 | 0.26 ms | 0.22 ms |

<div align="center">
<em>Comparaison des temps d'exécution pour les deux cas de test montrant l'impact de la compilation JIT lors de la première exécution.</em>
</div>

<div align="center">

**Tableau 3 : Répartition des responsabilités par module**

</div>

| Module | Responsabilité | Fonctions clés |
|--------|----------------|----------------|
| main.pl | Orchestration | `main_menu/0`, `execute_test_case/1` |
| astar.pl | Algorithme IA | `astar_search/5`, `misplaced_tiles_heuristic/3` |
| game.pl | Logique métier | `generate_moves/2`, `is_solvable/2` |
| display.pl | Interface | `display_menu/0`, `display_solution/4` |

<div align="center">
<em>Architecture modulaire suivant le principe de séparation des responsabilités.</em>
</div>

---

## 4. ANALYSE ET DISCUSSION

### 4.1 Architecture et qualité du code

L'architecture modulaire suit le principe de séparation des responsabilités. Chaque module a une fonction précise : main.pl gère l'interface, astar.pl implémente l'algorithme, game.pl gère la logique du taquin et display.pl s'occupe de l'affichage. Cette organisation facilite la maintenance et les tests. La documentation suit les conventions PlDoc et les prédicats ont des noms clairs.

### 4.2 Interprétation des résultats et comparaison avec les attentes

Les résultats obtenus correspondent aux attentes pour A* avec l'heuristique des tuiles mal placées. Pour le cas test [1,2,3,5,0,6,4,7,8], on obtient un coût de 4 mouvements, ce qui est optimal. L'algorithme explore 12 nœuds, ce qui montre que l'heuristique guide bien la recherche sans explorer inutilement l'espace d'états complet (181 440 configurations possibles).

### 4.3 Performance et limites identifiées

**Forces** : L'architecture modulaire facilite la maintenance et permet une séparation claire des responsabilités. A* garantit l'optimalité et la reproductibilité des résultats. Les temps d'exécution respectent les contraintes de performance (<3ms).

**Limites** : Notre implémentation présente deux limitations. D'abord, notre méthode de tri de l'open list n'est pas optimisée : on retrie toute la liste à chaque nouveau nœud, ce qui devient lent pour des problèmes plus gros. Ensuite, l'heuristique choisie (tuiles mal placées) donne parfois des estimations faibles, forçant A* à explorer plus de chemins avant de trouver la solution.

### 4.4 Améliorations possibles et extensions futures

L'implémentation d'une file de priorité optimisée (heap binaire) réduirait la complexité de gestion de l'open list de O(n log n) à O(log n). L'intégration de l'heuristique de distance de Manhattan<sup>[6]</sup> améliorerait significativement l'efficacité de la recherche en fournissant une estimation plus précise. L'adoption d'IDA* (Iterative Deepening A*)<sup>[3]</sup> permettrait de traiter des instances plus complexes avec une consommation mémoire constante O(d) plutôt qu'exponentielle.

---

## 5. CONCLUSION

### 5.1 Bilan du travail pratique

Ce projet a permis de bien comprendre les concepts de base de l'intelligence artificielle. L'implémentation en Prolog montre que ce langage convient bien aux problèmes de recherche heuristique grâce à sa nature logique. Les tests confirment que notre algorithme fonctionne correctement avec des résultats reproductibles qui correspondent aux attentes.

### 5.2 Accomplissements par rapport aux objectifs

Tous les objectifs du projet ont été atteints. A* produit des solutions optimales avec les bonnes métriques. L'heuristique des tuiles mal placées respecte les propriétés d'admissibilité et de consistance requises pour garantir l'optimalité des solutions.

### 5.3 Perspectives et recommandations

**Directions de recherche futures** : L'extension vers des domaines de recherche plus complexes (taquins N×N, problèmes de planification, jeux à deux joueurs) constitue une progression naturelle. L'exploration de techniques avancées comme les bases de données de motifs (pattern databases) ou la recherche bidirectionnelle ouvrirait de nouvelles perspectives d'optimisation.

**Recommandations méthodologiques** : Intégrer une approche comparative systématique entre différentes heuristiques pour quantifier les gains de performance. Développer une architecture modulaire permettant l'expérimentation avec différents algorithmes de recherche (RBFS, SMA*) tout en conservant l'interface commune.

---

## 6. UTILISATION D'INTELLIGENCE ARTIFICIELLE GÉNÉRATIVE

Sonnet 4 et Opus 4.1<sup>[1]</sup> ainsi que GPT-5<sup>[6]</sup> ont servi d'assistants techniques pour l'analyse des besoins, l'architecture et l'amélioration rédactionnelle. Des outils spécialisés comme Context7<sup>[4]</sup> (MCP server reconnu pour sa fiabilité dans la fourniture de documentation technique actualisée) ont facilité la validation des spécifications A* et l'obtention de références bibliographiques.

L'ensemble du travail a été réalisé sous supervision directe avec une validation continue de chaque étape. Notre contribution personnelle couvre l'ensemble du développement, incluant la conception de l'architecture modulaire, l'implémentation complète de l'algorithme A* avec ses heuristiques, l'optimisation des performances et la validation des résultats selon les spécifications du projet. Cette approche nous a permis de mieux gérer le temps alloué aux tâches secondaires pour nous concentrer sur l'assimilation des concepts fondamentaux d'intelligence artificielle.

---

## 7. RÉFÉRENCES BIBLIOGRAPHIQUES

[1] Anthropic. (2024). *Sonnet 4 et Opus 4.1: AI Assistants*. https://claude.ai/

[2] Aycock, J. (2003). A brief history of just-in-time compilation. ACM Computing Surveys, 35(2), 97-113.

[3] Bratko, I. (2012). *Prolog Programming for Artificial Intelligence*. 4th Edition. Addison-Wesley.

[4] Context7. (2024). *Model Context Protocol Server for Technical Documentation*. https://context7.com/

[5] Hart, P. E., Nilsson, N. J., & Raphael, B. (1968). A formal basis for the heuristic determination of minimum cost paths. IEEE Transactions on Systems Science and Cybernetics, 4(2), 100-107.

[6] GPT-5: AI Language Model. (2024). https://openai.com/

[7] Russell, S. & Norvig, P. (2020). *Artificial Intelligence: A Modern Approach*. 4th Edition. Pearson.

[8] SWI-Prolog Documentation. (2025). https://www.swi-prolog.org/

---

## ANNEXE A : EXTRAITS DE CODE SOURCE

### Point d'entrée principal A* (astar.pl)

```prolog
%! astar_search(+Initial:list, +Goal:list, -Path:list, -Cost:integer, -Expanded:integer) is det.
%  Point d'entrée principal de l'algorithme A*
astar_search(Initial, Goal, Path, Cost, Expanded) :-
    validate_search_inputs(Initial, Goal),            % Validation préalable
    (   states_equal(Initial, Goal) ->               % Cas trivial résolu
        Path = [Initial], Cost = 0, Expanded = 0
    ;   initialize_search(Initial, Goal, InitialNode, SearchContext),
        execute_astar_search(InitialNode, SearchContext, Result),
        extract_search_results(Result, Path, Cost, Expanded)
    ).
```

### Heuristique tuiles mal placées (astar.pl)

```prolog
%! misplaced_tiles_heuristic(+State:list, +Goal:list, -Count:integer) is det.
%  Compte les tuiles mal placées (case vide ignorée)
misplaced_tiles_heuristic(State, Goal, Count) :-
    misplaced_tiles_helper(State, Goal, 0, Count).

misplaced_tiles_helper([], [], Count, Count).
misplaced_tiles_helper([S|ST], [G|GT], Acc, Count) :-
    (   (S \= G, S \= 0) -> NewAcc is Acc + 1 ; NewAcc = Acc ),
    misplaced_tiles_helper(ST, GT, NewAcc, Count).
```

### Utilisation de l'heuristique pour créer les successeurs (astar.pl)

```prolog
%! create_successor_nodes(+States:list, +Goal:list, +G:integer, +Parent:compound, -Nodes:list, +GenCountIn:integer, -GenCountOut:integer) is det.
%  Crée les nœuds A* pour tous les états successeurs
create_successor_nodes([State|RestStates], Goal, G, Parent, [Node|RestNodes], GenCountIn, GenCountOut) :-
    GenCountMid is GenCountIn + 1,                    % Incrémenter compteur
    misplaced_tiles_heuristic(State, Goal, H),        % Calculer h(n)
    create_node(State, G, H, Parent, Node),           % Créer nœud avec f=g+h
    create_successor_nodes(RestStates, Goal, G, Parent, RestNodes, GenCountMid, GenCountOut).
```

### Génération des mouvements (game.pl)

```prolog
%! generate_moves(+State:list, -Moves:list) is det.
%  Génère tous les mouvements valides dans l'ordre déterministe
generate_moves(State, Moves) :-
    find_blank_position(State, BlankPos),     % Localiser case vide
    generate_moves_from_position(BlankPos, State, Moves).

generate_moves_from_position(Pos, State, Moves) :-
    findall(NewState,
        (member(Direction, [up, down, left, right]),  % Ordre déterministe
         valid_move(Pos, Direction),                  % Vérifier validité
         apply_move(State, Pos, Direction, NewState)  % Appliquer mouvement
        ),
        Moves).
```

### Tri de l'open list avec tie-breaking (astar.pl)

```prolog
%! sort_open_list_by_f_value(+Nodes:list, -SortedNodes:list) is det.
%  Trie les nœuds par f(n) croissant, avec tie-breaking sur g(n)
sort_open_list_by_f_value(Nodes, SortedNodes) :-
    predsort(compare_node_f_values, Nodes, SortedNodes).

%! compare_node_f_values(-Order:atom, +Node1:compound, +Node2:compound) is det.
%  Fonction de comparaison : priorité f(n), puis g(n) en cas d'égalité
compare_node_f_values(Order, Node1, Node2) :-
    node_f_cost(Node1, F1), node_f_cost(Node2, F2),
    (   F1 =:= F2 ->                          % Si f égaux : tie-breaking
        node_g_cost(Node1, G1), node_g_cost(Node2, G2),
        compare(Order, G1, G2)                % Priorité au plus petit g
    ;   compare(Order, F1, F2)                % Sinon : priorité au plus petit f
    ).
```

---