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

Ce travail implémente un solveur de Taquin (puzzle 3×3) utilisant A* avec l'heuristique des tuiles mal placées. Le Taquin constitue un benchmark classique en IA dans un espace d'états réduit (181 440 configurations solvables). Prolog modélise efficacement les transitions d'états et la recherche avec closed set<sup>[2]</sup>.

### 1.2 Objectifs du travail pratique

Développer un solveur intégrant A* avec closed set garantissant l'optimalité. Architecture modulaire 4 modules Prolog avec séparation des responsabilités. Heuristique des tuiles mal placées prouvée admissible (exclusion case vide). Validation empirique sur métriques précises selon les spécifications académiques.

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

Processus en cinq étapes : validation entrée (format 3×3, solvabilité par parité des inversions<sup>[4]</sup>), génération mouvements déterministe, recherche A*, évaluation heuristique, reconstruction chemin.

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

L'architecture modulaire illustre l'application rigoureuse du principe de responsabilité unique et la séparation des préoccupations. Chaque module assume une responsabilité bien définie facilitant la maintenance, les tests isolés et l'extension future. Documentation extensive utilisant les conventions PlDoc de SWI-Prolog, prédicats clairement nommés et gestion d'erreurs cohérente.

### 4.2 Interprétation des résultats et comparaison avec les attentes

Les métriques obtenues correspondent exactement aux valeurs théoriquement attendues pour A* avec l'heuristique des tuiles mal placées. Le coût de 4 mouvements représente effectivement la solution optimale pour la configuration académique [1,2,3,5,0,6,4,7,8], confirmant que l'algorithme trouve le chemin le plus court possible. L'expansion de 12 nœuds démontre l'efficacité du guidage heuristique, évitant l'exploration exhaustive des 181 440 configurations solvables possibles.

### 4.3 Performance et limites identifiées

**Forces** : architecture modulaire facilitant maintenance et extension, A* avec closed set garantissant résultats déterministes et optimaux, reproductibilité parfaite pour évaluation académique. **Limites** : heuristique des tuiles mal placées relativement conservatrice comparée à la distance de Manhattan<sup>[1]</sup>, interface console fonctionnelle mais limitée dans ses capacités de visualisation pour fins éducatives.

### 4.4 Améliorations possibles et extensions futures

Adoption de l'heuristique de distance de Manhattan<sup>[1]</sup> calculant pour chaque tuile la distance réelle nécessaire, optimisation de l'ordonnancement des successeurs selon leur potentiel heuristique plutôt que l'ordre rigide haut-bas-gauche-droite, extension vers IDA* (Iterative Deepening A*)<sup>[3]</sup> pour problèmes de plus grande complexité, interface graphique interactive offrant visualisation du processus de résolution en temps réel.

---

## 5. CONCLUSION

### 5.1 Bilan du travail pratique

Réussite technique complète démontrant la maîtrise des concepts fondamentaux de l'intelligence artificielle. L'implémentation Prolog illustre parfaitement l'adéquation des langages déclaratifs pour la résolution de problèmes de recherche heuristique, exploitant la nature logique du paradigme pour exprimer naturellement les règles de transition d'états et les contraintes du domaine. La validation empirique rigoureuse confirme l'exactitude avec des métriques parfaitement reproductibles correspondant aux spécifications académiques.

### 5.2 Accomplissements par rapport aux objectifs

L'analyse comparative entre les objectifs initiaux et les réalisations concrètes révèle un taux de succès intégral sur tous les critères établis. A* avec closed set délivre des performances remarquables, générant systématiquement des solutions optimales avec les métriques exactes d'après les exigences du projet requises pour la validation académique. L'heuristique des tuiles mal placées, développée avec une rigueur mathématique stricte, respecte intégralement les propriétés d'admissibilité et de consistance nécessaires à l'optimalité.

### 5.3 Perspectives et recommandations

**Perspectives** : Plateforme solide pour exploration d'heuristiques alternatives, extension vers autres domaines de recherche, potentiel pédagogique excellent. **Recommandations** : Adopter la distance de Manhattan pour améliorer l'efficacité, développer une interface graphique pour l'enseignement, explorer l'applicabilité industrielle pour l'optimisation combinatoire complexe.

---

## 6. UTILISATION D'INTELLIGENCE ARTIFICIELLE GÉNÉRATIVE

Dans un contexte académique où l'usage de l'IA générative suscite encore des débats, cette section vise à présenter une utilisation transparente et éthique de ces outils. Claude (Anthropic) a servi d'assistant technique pour l'analyse des besoins, l'analyse architecturale et l'optimisation du style rédactionnel. Context7 (Model Context Protocol server) a fourni l'accès à une documentation technique actualisée, facilitant la validation des spécifications A* et l'obtention de références bibliographiques fiables.

**Domaines d'assistance** : L'IA a contribué à l'analyse des besoins initiaux, à la définition des spécifications fonctionnelles, à la structuration de l'architecture modulaire, à l'implémentation de l'algorithme A*, à la conception d'heuristiques admissibles, à la génération de tests unitaires et à la documentation technique.

**Travail personnel authentique** : La compréhension des concepts A*, la résolution des défis d'implémentation Prolog et l'analyse des résultats demeurent personnels. L'IA a fourni une assistance substantielle pour l'architecture système, l'optimisation du code, la rédaction des tests unitaires et la documentation technique.

**Valeur ajoutée éthique** : L'assistance IA a permis d'améliorer la qualité de la documentation et d'accélérer certaines tâches répétitives, libérant du temps pour approfondir les aspects techniques fondamentaux appris dans le cadre du cours. La vérification systématique par validation empirique et tests garantit l'exactitude technique et l'authenticité académique du travail présenté.

---

## 7. RÉFÉRENCES BIBLIOGRAPHIQUES

[1] Russell, S. & Norvig, P. (2020). *Artificial Intelligence: A Modern Approach*. 4th Edition. Pearson.

[2] Hart, P. E., Nilsson, N. J., & Raphael, B. (1968). A formal basis for the heuristic determination of minimum cost paths. IEEE Transactions on Systems Science and Cybernetics, 4(2), 100-107.

[3] SWI-Prolog Documentation. (2025). https://www.swi-prolog.org/

[4] Bratko, I. (2012). *Prolog Programming for Artificial Intelligence*. 4th Edition. Addison-Wesley.

[5] Aycock, J. (2003). A brief history of just-in-time compilation. ACM Computing Surveys, 35(2), 97-113.

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