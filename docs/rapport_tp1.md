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
<td style="border: 1px solid #000; padding: 10px;">Patrick Patenaude</td>
<td style="border: 1px solid #000; padding: 10px;"></td>
<td style="border: 1px solid #000; padding: 10px;"></td>
</tr>
<tr>
<td style="border: 1px solid #000; padding: 10px;">Xavier Gagnon</td>
<td style="border: 1px solid #000; padding: 10px;"></td>
<td style="border: 1px solid #000; padding: 10px;"></td>
</tr>
<tr>
<td style="border: 1px solid #000; padding: 10px;">Daniel José Anillo Santos</td>
<td style="border: 1px solid #000; padding: 10px;"></td>
<td style="border: 1px solid #000; padding: 10px;"></td>
</tr>
<tr>
<td style="border: 1px solid #000; padding: 10px;">Alexandre Gamache</td>
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

Ce travail présente un solveur de Taquin (puzzle 3×3) utilisant A* avec l'heuristique des tuiles mal placées. Le Taquin est un problème classique en IA avec un espace d'états limité (181 440 configurations solvables). Prolog permet de bien modéliser les transitions d'états et la recherche avec closed set<sup>[3]</sup>.

### 1.2 Objectifs du travail pratique

Développer un solveur qui utilise A* avec closed set pour garantir l'optimalité. Architecture modulaire avec 4 modules Prolog et séparation des responsabilités. Heuristique des tuiles mal placées admissible (exclusion case vide). Validation sur des métriques précises selon les spécifications du projet.

### 1.3 Plan du rapport

Le rapport présente l'architecture, l'implémentation A*, l'heuristique et les résultats validés.

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

### 2.3 Étapes de réalisation du travail pratique

Quatre phases : analyse et conception, implémentation séquentielle (game.pl → astar.pl → display.pl → main.pl), tests et validation, optimisation et documentation.

### 2.4 Algorithmes implémentés

A* utilise une structure de nœud contenant l'état, les coûts g(n), h(n), f(n), et un pointeur parent. Boucle principale avec open list triée par f(n) et closed set.

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

Solveur complet utilisant A* avec closed set, interface CLI en français avec configuration automatique UTF-8 multiplateforme, deux cas de test validés (configuration académique standard et cas personnalisé complexe). Résultats parfaitement déterministes garantis par l'ordre strict de génération des mouvements, le tri cohérent de l'open list et l'heuristique mathématiquement prouvée admissible.

### 3.2 Validation technique

<div align="center">

**Figure 1 : Menu principal du solveur de Taquin**

<img src="images/menu_principal.png" alt="Menu Principal" width="300">

<em>Interface d'accueil avec menu ASCII et options de navigation.</em>

</div>

<div align="center">

**Figure 2 : Exécution du cas test 1 avec solution complète**

<img src="images/CasTest1.png" alt="Cas Test 1" width="300">

<em>Résolution du cas test académique avec métriques exactes conformément à l'énoncé du TP.</em>

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

**Points forts** : L'architecture modulaire rend le code facile à maintenir. A* avec closed set donne des résultats reproductibles et optimaux. Les temps d'exécution sont rapides (<3ms).

**Limites** : L'heuristique des tuiles mal placées est moins efficace que la distance de Manhattan. L'interface console fonctionne bien mais reste basique comparée à une interface graphique.

### 4.4 Améliorations possibles et extensions futures

L'heuristique de distance de Manhattan<sup>[6]</sup> calculerait la distance réelle de chaque tuile vers sa position finale et serait plus efficace. L'implémentation d'IDA* (Iterative Deepening A*)<sup>[3]</sup> permettrait de traiter des problèmes plus complexes. Une interface graphique offrirait une meilleure visualisation du processus de résolution.

---

## 5. CONCLUSION

### 5.1 Bilan du travail pratique

Ce projet a permis de bien comprendre les concepts de base de l'intelligence artificielle. L'implémentation en Prolog montre que ce langage convient bien aux problèmes de recherche heuristique grâce à sa nature logique. Les tests confirment que notre algorithme fonctionne correctement avec des résultats reproductibles qui correspondent aux attentes.

### 5.2 Accomplissements par rapport aux objectifs

Tous les objectifs du projet ont été atteints. A* avec closed set produit des solutions optimales avec les bonnes métriques (coût=4, expanded=12). L'heuristique des tuiles mal placées respecte les propriétés d'admissibilité et de consistance requises pour garantir l'optimalité des solutions.

### 5.3 Perspectives et recommandations

Le solveur pourrait être étendu à des taquins plus grands (4x4, 5x5) ou adapté à d'autres problèmes de recherche comme le problème des 8-reines. Comme améliorations futures, on recommande d'adopter la distance de Manhattan pour plus d'efficacité et de développer une interface graphique pour améliorer l'expérience utilisateur.

---

## 6. UTILISATION D'INTELLIGENCE ARTIFICIELLE GÉNÉRATIVE

Claude Opus (Anthropic)<sup>[1]</sup> et GPT-5 (OpenAI)<sup>[6]</sup> ont servi d'assistants techniques pour l'analyse des besoins, l'architecture et l'optimisation rédactionnelle. Context7<sup>[4]</sup> (MCP server reconnu pour sa fiabilité dans la fourniture de documentation technique actualisée) a facilité la validation des spécifications A* et l'obtention de références bibliographiques.

L'IA a fourni un support technique pour certains aspects du développement. L'ensemble du travail a été réalisé en collaboration active, avec contrôle et validation empirique rigoureuse à chaque étape. Notre contribution personnelle authentique couvre l'intégralité du projet, particulièrement la maîtrise conceptuelle et l'implémentation de l'algorithme A*, la résolution des défis Prolog et l'analyse critique des résultats. Cette collaboration a permis d'accélérer les tâches de développement périphériques, nous concentrant pleinement sur l'apprentissage approfondi des concepts d'intelligence artificielle.

---

## 7. RÉFÉRENCES BIBLIOGRAPHIQUES

[1] Anthropic. (2024). *Claude: AI Assistant*. https://claude.ai/

[2] Aycock, J. (2003). A brief history of just-in-time compilation. ACM Computing Surveys, 35(2), 97-113.

[3] Bratko, I. (2012). *Prolog Programming for Artificial Intelligence*. 4th Edition. Addison-Wesley.

[4] Context7. (2024). *Model Context Protocol Server for Technical Documentation*. https://context7.com/

[5] Hart, P. E., Nilsson, N. J., & Raphael, B. (1968). A formal basis for the heuristic determination of minimum cost paths. IEEE Transactions on Systems Science and Cybernetics, 4(2), 100-107.

[6] OpenAI. (2024). *GPT-5: AI Language Model*. https://openai.com/

[7] Russell, S. & Norvig, P. (2020). *Artificial Intelligence: A Modern Approach*. 4th Edition. Pearson.

[8] SWI-Prolog Documentation. (2025). https://www.swi-prolog.org/

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

misplaced_tiles_helper([], [], Count, Count).
misplaced_tiles_helper([S|ST], [G|GT], Acc, Count) :-
    (   (S \= G, S \= 0) -> NewAcc is Acc + 1 ; NewAcc = Acc ),
    misplaced_tiles_helper(ST, GT, NewAcc, Count).
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

### Gestion des états et validation (game.pl)

```prolog
%! valid_state(+State:list) is semidet.
%  Vérifie qu'un état de taquin est valide (9 éléments, chiffres 0-8 uniques)
valid_state(State) :-
    length(State, 9),                         % Exactement 9 éléments
    sort(State, [0,1,2,3,4,5,6,7,8]).        % Tous chiffres présents une fois

%! is_solvable(+State:list, +Goal:list) is semidet.
%  Détermine si un état peut être résolu via parité des inversions
is_solvable(State, Goal) :-
    valid_state(State), valid_state(Goal),
    count_inversions(State, InvState),        % Compter inversions état
    count_inversions(Goal, InvGoal),          % Compter inversions but
    InvState mod 2 =:= InvGoal mod 2.         % Même parité = solvable
```

---