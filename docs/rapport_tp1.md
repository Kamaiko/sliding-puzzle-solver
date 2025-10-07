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
   - 2.2 [Modélisation du problème](#22-modélisation-du-problème)
   - 2.3 [Étapes de réalisation du travail pratique](#23-étapes-de-réalisation-du-travail-pratique)
   - 2.4 [Algorithmes, schémas et diagrammes de fonctionnement](#24-algorithmes-schémas-et-diagrammes-de-fonctionnement)
   - 2.5 [Programme](#25-programme)

3. [RÉSULTATS ET DISCUSSION](#3-résultats-et-discussion)
   - 3.1 [Fonctionnalités implémentées](#31-fonctionnalités-implémentées)
   - 3.2 [Validation technique](#32-validation-technique)
   - 3.3 [Performance et métriques](#33-performance-et-métriques)
   - 3.4 [Architecture et qualité du code](#34-architecture-et-qualité-du-code)
   - 3.5 [Interprétation des résultats et comparaison avec les attentes](#35-interprétation-des-résultats-et-comparaison-avec-les-attentes)
   - 3.6 [Performance et limites identifiées](#36-performance-et-limites-identifiées)
   - 3.7 [Améliorations possibles et extensions futures](#37-améliorations-possibles-et-extensions-futures)

4. [CONCLUSION](#4-conclusion)
   - 4.1 [Bilan du travail pratique](#41-bilan-du-travail-pratique)
   - 4.2 [Accomplissements par rapport aux objectifs](#42-accomplissements-par-rapport-aux-objectifs)
   - 4.3 [Perspectives et recommandations](#43-perspectives-et-recommandations)

5. [UTILISATION D'INTELLIGENCE ARTIFICIELLE GÉNÉRATIVE](#5-utilisation-dintelligence-artificielle-générative)

6. [RÉFÉRENCES BIBLIOGRAPHIQUES](#6-références-bibliographiques)

**ANNEXE A** : [EXTRAITS DE CODE SOURCE](#annexe-a--extraits-de-code-source)

---

## 1. INTRODUCTION

### 1.1 Contexte et justification

Ce travail présente un solveur de Taquin (puzzle 3×3) utilisant l'algorithme A* avec l'heuristique de distance Manhattan. Le Taquin est un problème classique en IA avec un espace d'états limité (181 440 configurations solvables). Prolog permet de bien modéliser les transitions d'états et l'implémentation de recherches heuristiques<sup>[3]</sup>.

### 1.2 Objectifs du travail pratique

Développer un solveur qui utilise A* pour garantir l'optimalité. Architecture modulaire avec 4 modules Prolog et séparation des responsabilités. Heuristique de distance Manhattan admissible et consistante. Validation sur des métriques précises.

### 1.3 Plan du rapport

Le rapport présente la méthodologie (architecture modulaire, algorithme A*, heuristique), les résultats (validation technique et métriques) et l'analyse (qualité, limitations, améliorations).

---

## 2. MÉTHODOLOGIE

### 2.1 Matériel, logiciels et outils utilisés

Le développement a été réalisé avec SWI-Prolog 9.0.4, Visual Studio Code et Git/GitHub, avec des tests sur Windows 10/11 pour assurer la portabilité multiplateforme.

### 2.2 Modélisation du problème

Le problème du taquin consiste à réorganiser des tuiles numérotées sur une grille 3×3 contenant une case vide, en effectuant des déplacements successifs jusqu'à atteindre une configuration cible. Ce problème classique d'intelligence artificielle permet d'illustrer les concepts de recherche heuristique et d'optimalité de solutions.

**État initial.** Deux configurations de départ sont étudiées dans ce travail. Le premier cas de test utilise la configuration classique `[1,2,3,5,0,6,4,7,8]`, où la case vide (représentée par 0) se trouve en position centrale. Cette configuration nécessite 4 mouvements optimaux pour atteindre le but. Le second cas de test présente une configuration plus complexe `[1,3,6,5,2,8,4,0,7]`, requérant 9 mouvements optimaux.

**État final.** L'état but recherché est unique et correspond à la configuration ordonnée `[1,2,3,4,5,6,7,8,0]`, où les tuiles sont placées en ordre croissant de gauche à droite et de haut en bas, la case vide occupant la dernière position.

**Mouvements.** Quatre mouvements sont possibles selon la position de la case vide : déplacer une tuile vers le haut (UP), vers le bas (DOWN), vers la gauche (LEFT) ou vers la droite (RIGHT). Notre implémentation génère les successeurs dans cet ordre précis pour garantir le déterminisme des résultats. Lorsque la case vide se trouve en bordure ou dans un coin, certains mouvements deviennent impossibles et ne sont pas générés.

**Technique de recherche.** Nous avons choisi l'algorithme A* pour garantir l'optimalité des solutions. A* combine le coût réel g(n) depuis l'état initial avec une estimation heuristique h(n) du coût restant vers le but, en utilisant la fonction d'évaluation f(n) = g(n) + h(n) pour prioriser l'exploration. L'heuristique de distance Manhattan calcule pour chaque tuile la somme des déplacements horizontaux et verticaux nécessaires. Cette heuristique est admissible car elle ne surestime jamais le coût réel, et consistante car la différence d'estimation entre deux états successifs ne dépasse jamais le coût du mouvement. L'algorithme maintient une liste ouverte des nœuds à explorer, triée par f(n) croissant, et un ensemble fermé pour éviter la re-exploration d'états déjà visités.

**Résultats attendus.** Pour le cas test classique, nous nous attendons à trouver une solution optimale de 4 mouvements. Pour le cas test avancé, la solution optimale devrait être de 9 mouvements. L'algorithme A* avec l'heuristique Manhattan devrait garantir ces solutions optimales grâce à l'admissibilité de l'heuristique. Le nombre exact de nœuds explorés dépendra de l'implémentation, mais devrait rester raisonnable pour ces configurations relativement simples.

### 2.3 Étapes de réalisation du travail pratique

Le développement s'est déroulé en quatre phases structurées :

1. **Analyse et conception** : Modélisation du problème du taquin, conception de l'architecture modulaire, spécification des interfaces entre modules
2. **Implémentation séquentielle** : Développement itératif des modules dans l'ordre game.pl → astar.pl → display.pl → main.pl
3. **Tests et validation** : Suite de tests unitaires et d'intégration, validation des métriques exactes, vérification de l'optimalité
4. **Optimisation et documentation** : Amélioration des performances (warm-up JIT<sup>[2]</sup>, gestion mémoire), documentation PlDoc, préparation du livrable

### 2.4 Algorithmes, schémas et diagrammes de fonctionnement

L'algorithme A* repose sur une structure de nœud contenant cinq composantes essentielles : l'état du taquin représenté par une liste de neuf éléments, le coût réel g(n) correspondant à la profondeur dans l'arbre de recherche, l'estimation heuristique h(n) calculée par la distance Manhattan, la fonction d'évaluation f(n) égale à g(n) + h(n), et un pointeur vers le nœud parent permettant la reconstruction du chemin solution.

Le flux d'exécution de A* suit une séquence structurée. La validation initiale vérifie le format de l'état de départ et teste la solvabilité de la configuration par calcul de la parité des inversions, garantissant qu'aucun effort de calcul n'est dépensé sur une configuration impossible à résoudre. L'initialisation crée ensuite le nœud racine avec son estimation heuristique h(n) et l'insère dans la liste ouverte. La boucle principale extrait itérativement le nœud ayant la plus petite valeur f(n), teste s'il correspond à l'état but, et dans le cas contraire génère ses successeurs selon l'ordre déterministe UP, DOWN, LEFT, RIGHT. Chaque successeur nouvellement créé reçoit son estimation heuristique et est ajouté à la liste ouverte si son état n'a pas déjà été exploré. Le nœud traité est transféré dans l'ensemble fermé pour éviter toute re-exploration. Ce processus se poursuit jusqu'à l'atteinte du but, moment où la reconstruction du chemin s'effectue par remontée récursive des pointeurs parents depuis le nœud final jusqu'au nœud racine.

L'heuristique de distance Manhattan se calcule en parcourant chaque tuile de l'état actuel et en déterminant sa position cible dans l'état but. Pour une grille 3×3, la position d'une tuile se traduit en coordonnées ligne-colonne par division et modulo de son index. La distance Manhattan d'une tuile correspond à la somme des valeurs absolues des différences de lignes et de colonnes entre sa position actuelle et sa position but. L'accumulation de ces distances pour toutes les tuiles, en excluant la case vide, produit l'estimation h(n). Cette heuristique respecte la propriété d'admissibilité car chaque tuile nécessite au minimum sa distance Manhattan en mouvements pour atteindre sa position finale, et la propriété de consistance car un seul mouvement peut au mieux rapprocher une tuile d'une case vers sa destination.

### 2.5 Programme

**Implémentation.** Le système adopte une architecture modulaire en quatre couches spécialisées respectant le principe de séparation des responsabilités. Le module game.pl encapsule la logique métier du taquin, fournissant les prédicats de validation d'états, de génération de mouvements valides et de test de solvabilité. Le module astar.pl implémente l'algorithme de recherche avec ses structures de données (nœuds, liste ouverte, ensemble fermé) et l'heuristique Manhattan. Le module display.pl gère l'interface utilisateur terminale avec les bannières ASCII, l'affichage formaté des grilles 3×3 et la présentation des résultats. Le module main.pl orchestre l'ensemble en gérant le menu principal, l'exécution des cas de test et la gestion des erreurs. Cette séparation facilite la maintenance du code, permet les tests unitaires indépendants de chaque composante, et respecte les conventions de développement modulaire en Prolog.

**Guide d'utilisation.** Après avoir installé SWI-Prolog, le programme se lance en tapant la commande `swipl run.pl` dans le terminal, ce qui initialise l'environnement Prolog et affiche le menu principal. L'interface propose deux scénarios prédéfinis accessibles par sélection numérique. Le scénario classique valide le fonctionnement de base avec une configuration simple, tandis que le scénario avancé démontre les capacités sur une instance plus complexe. Une section informative présente les détails du projet et les références académiques. La navigation s'effectue par saisie du numéro correspondant à l'option désirée, et le programme boucle jusqu'à la sélection de l'option de sortie.

**Code de la recherche heuristique.** L'implémentation de l'heuristique Manhattan parcourt récursivement l'état actuel en ignorant la case vide. Pour chaque tuile, la fonction détermine sa position but, calcule les coordonnées ligne-colonne de ses positions actuelle et cible par opérations arithmétiques de division entière et modulo, puis accumule la somme des différences absolues :

```prolog
manhattan_distance(Tile, CurrentPos, GoalPos, Distance) :-
    CurrentRow is CurrentPos // 3,
    CurrentCol is CurrentPos mod 3,
    GoalRow is GoalPos // 3,
    GoalCol is GoalPos mod 3,
    Distance is abs(CurrentRow - GoalRow) + abs(CurrentCol - GoalCol).
```

Le prédicat principal `manhattan_distance_heuristic/3` délègue le calcul à un helper récursif qui traite la liste état position par position, maintenant un accumulateur de distance totale jusqu'au cas de base.

**Compilation et exécution.** Le pipeline d'exécution débute par le lancement du programme via `swipl run.pl`, qui provoque la consultation automatique des quatre modules dans l'ordre game.pl, astar.pl, display.pl puis main.pl, assurant la disponibilité de tous les prédicats nécessaires. La configuration UTF-8 s'active automatiquement pour garantir l'affichage correct des caractères étendus sur les systèmes Windows. L'interface présente ensuite le menu principal avec le titre ASCII stylisé et les options de navigation. Lorsque l'utilisateur sélectionne un scénario, le système effectue d'abord la validation de la configuration en vérifiant le format de l'état initial (neuf éléments, valeurs uniques de 0 à 8) et en testant la solvabilité via le calcul de parité des inversions. L'algorithme A* s'exécute ensuite en initialisant le nœud racine avec son heuristique, puis en itérant sa boucle principale qui extrait le meilleur nœud selon f(n), génère les successeurs dans l'ordre déterministe défini, et teste l'atteinte du but. Une fois la solution trouvée, la reconstruction du chemin s'effectue par remontée des parents, et l'affichage présente la séquence complète des états traversés accompagnée des métriques de performance (coût, nœuds explorés, temps d'exécution). Pour la validation du système, la commande `swipl -g run_all_tests src/tests.pl` exécute la suite de tests automatisés couvrant les fonctionnalités de chaque module.

**Documentation.** Le code source suit les conventions PlDoc de SWI-Prolog pour la documentation inline. Chaque prédicat public est accompagné d'un commentaire structuré spécifiant son mode d'utilisation (déterministe, semi-déterministe, non-déterministe), ses paramètres avec leurs annotations (+, -, ?) indiquant si les arguments sont en entrée, en sortie, ou les deux, et une description textuelle de sa fonction. Les prédicats complexes incluent des annotations supplémentaires précisant les invariants, les propriétés garanties et les références bibliographiques pertinentes. Cette approche de documentation permet la génération automatique d'une référence API via l'outil pldoc de SWI-Prolog et facilite la compréhension du code lors de la maintenance.

---

## 3. RÉSULTATS ET DISCUSSION

### 3.1 Fonctionnalités implémentées

Le système implémente un solveur de taquin basé sur l'algorithme A* avec heuristique de distance Manhattan. L'interface en ligne de commande propose un menu de navigation permettant d'accéder à deux scénarios de résolution (configuration classique à 4 mouvements et configuration avancée à 9 mouvements), ainsi qu'une section informative sur le projet. Le système affiche pour chaque résolution la séquence complète des états traversés, le coût de la solution, le nombre de nœuds explorés et le temps d'exécution. La configuration UTF-8 multiplateforme garantit l'affichage correct des caractères ASCII étendus sur Windows, Mac et Linux. Le comportement déterministe assure la reproductibilité exacte des résultats.

### 3.2 Validation technique

La validation du système s'appuie sur une suite de tests automatisée couvrant l'ensemble des modules développés. Le fichier `tests.pl` implémente 14 tests unitaires regroupés en quatre sections correspondant aux modules principaux : game.pl (localisation case vide, génération mouvements, validation états, détection solvabilité), astar.pl (heuristique Manhattan, création nœuds, tri liste ouverte, reconstruction chemin), display.pl (affichage états, formatage tuiles), et main.pl (résolution cas test 1 et 2). Deux tests d'intégration end-to-end complètent cette couverture en validant le pipeline complet depuis l'initialisation jusqu'à l'affichage des résultats, incluant la mesure des temps d'exécution pour garantir les contraintes de performance (<1s pour cas 1, <3s pour cas 2).

Les tests critiques vérifient l'optimalité des solutions : le scénario classique `[1,2,3,5,0,6,4,7,8]` doit produire un coût de 4 mouvements, tandis que le scénario avancé `[1,3,6,5,2,8,4,0,7]` doit retourner un coût de 9 mouvements. Ces validations empiriques confirment que l'algorithme trouve bien les solutions optimales et maintient un comportement déterministe. La suite de tests s'exécute via la commande `swipl -g run_all_tests src/tests.pl` et affiche un résumé détaillé avec le temps total d'exécution, permettant de détecter immédiatement toute régression lors de modifications du code.

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

**Optimisation des temps d'exécution** : Le système implémente un warm-up JIT pour éliminer la variabilité de la première exécution. Une pré-exécution silencieuse force la compilation Just-In-Time de SWI-Prolog, garantissant des temps cohérents <3ms pour les exécutions subséquentes.

### 3.4 Architecture et qualité du code

L'architecture repose sur une organisation en quatre modules spécialisés : game.pl encapsule les règles du taquin et la validation des états, astar.pl contient l'algorithme de recherche et l'heuristique, display.pl gère l'affichage formaté des résultats, et main.pl coordonne l'exécution et l'interface utilisateur. Cette structure modulaire permet de tester chaque composante indépendamment et facilite la localisation des bugs. La documentation PlDoc fournit pour chaque prédicat public des annotations structurées précisant les modes d'utilisation et les paramètres attendus.

### 3.5 Interprétation des résultats et comparaison avec les attentes

Les résultats obtenus correspondent aux attentes pour A* avec l'heuristique de distance Manhattan. Pour le cas test [1,2,3,5,0,6,4,7,8], on obtient un coût de 4 mouvements, ce qui est optimal. L'algorithme explore 12 nœuds, ce qui montre que l'heuristique guide efficacement la recherche sans explorer inutilement l'espace d'états complet (181 440 configurations possibles).

### 3.6 Performance et limites identifiées

**Forces** : L'heuristique Manhattan guide efficacement la recherche en réduisant considérablement l'espace d'exploration (12 nœuds au lieu de potentiellement des milliers). Le comportement déterministe garantit des résultats reproductibles à chaque exécution. Les temps de résolution restent sous la barre des 3ms pour les deux scénarios testés, ce qui démontre l'efficacité pratique de l'implémentation. La modularité du code facilite les tests unitaires et la détection rapide des régressions.

**Limites** : Notre implémentation présente une limitation principale : notre méthode de tri de l'open list n'est pas optimisée. On retrie toute la liste à chaque nouveau nœud, ce qui devient lent pour des problèmes plus gros. L'utilisation d'une file de priorité optimisée (heap binaire) réduirait la complexité de O(n log n) à O(log n) par opération.

### 3.7 Améliorations possibles et extensions futures

L'adoption d'IDA* (Iterative Deepening A*)<sup>[3]</sup> permettrait de traiter des instances plus complexes avec une consommation mémoire constante O(d) plutôt qu'exponentielle. L'exploration de bases de données de motifs (pattern databases) offrirait des heuristiques encore plus informées pour des puzzles plus grands. L'intégration de techniques de parallélisation pourrait accélérer la recherche sur des configurations complexes.

---

## 4. CONCLUSION

### 4.1 Bilan du travail pratique

Ce projet a permis d'approfondir notre compréhension de l'algorithme A* et de ses propriétés d'optimalité. L'implémentation en Prolog s'est révélée bien adaptée pour modéliser les états, les transitions et le backtracking nécessaire à la reconstruction des chemins. La suite de tests automatisée confirme que l'algorithme produit systématiquement les solutions optimales attendues (4 mouvements pour le scénario classique, 9 pour le scénario avancé) avec un comportement parfaitement déterministe.

### 4.2 Accomplissements par rapport aux objectifs

Tous les objectifs du projet ont été atteints. A* produit des solutions optimales avec les bonnes métriques. L'heuristique de distance Manhattan respecte les propriétés d'admissibilité et de consistance requises pour garantir l'optimalité des solutions.

### 4.3 Perspectives et recommandations

**Directions de recherche futures** : L'extension vers des domaines de recherche plus complexes (taquins N×N, problèmes de planification, jeux à deux joueurs) constitue une progression naturelle. L'exploration de techniques avancées comme les bases de données de motifs (pattern databases) ou la recherche bidirectionnelle ouvrirait de nouvelles perspectives d'optimisation.

**Recommandations méthodologiques** : Intégrer une approche comparative systématique entre différentes heuristiques pour quantifier les gains de performance. Développer une architecture modulaire permettant l'expérimentation avec différents algorithmes de recherche (RBFS, SMA*) tout en conservant l'interface commune.

---

## 5. UTILISATION D'INTELLIGENCE ARTIFICIELLE GÉNÉRATIVE

Sonnet 4 et Opus 4.1<sup>[1]</sup> ainsi que GPT-5<sup>[6]</sup> ont servi d'assistants techniques pour l'analyse des besoins, l'architecture et l'amélioration rédactionnelle. Des outils spécialisés comme Context7<sup>[4]</sup> (MCP server reconnu pour sa fiabilité dans la fourniture de documentation technique actualisée) ont facilité la validation des spécifications A* et l'obtention de références bibliographiques.

L'ensemble du travail a été réalisé sous supervision directe avec une validation continue de chaque étape. Notre contribution personnelle couvre l'ensemble du développement, incluant la conception de l'architecture modulaire, l'implémentation complète de l'algorithme A* avec ses heuristiques, l'optimisation des performances et la validation des résultats selon les spécifications du projet. Cette approche nous a permis de mieux gérer le temps alloué aux tâches secondaires pour nous concentrer sur l'assimilation des concepts fondamentaux d'intelligence artificielle.

---

## 6. RÉFÉRENCES BIBLIOGRAPHIQUES

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

### Heuristique distance Manhattan (astar.pl)

```prolog
%! manhattan_distance_heuristic(+State:list, +Goal:list, -Distance:integer) is det.
%  Calcule la somme des distances Manhattan pour toutes les tuiles
manhattan_distance_heuristic(State, Goal, Distance) :-
    manhattan_sum(State, Goal, 0, 0, Distance).

manhattan_sum([], [], _, Acc, Acc).
manhattan_sum([Tile|RestState], [_|RestGoal], Pos, Acc, Distance) :-
    (   Tile =:= 0 -> NewAcc = Acc
    ;   nth0(GoalPos, [1,2,3,4,5,6,7,8,0], Tile),
        CurrentRow is Pos // 3, CurrentCol is Pos mod 3,
        GoalRow is GoalPos // 3, GoalCol is GoalPos mod 3,
        RowDiff is abs(CurrentRow - GoalRow),
        ColDiff is abs(CurrentCol - GoalCol),
        TileDist is RowDiff + ColDiff,
        NewAcc is Acc + TileDist
    ),
    NextPos is Pos + 1,
    manhattan_sum(RestState, RestGoal, NextPos, NewAcc, Distance).
```

### Utilisation de l'heuristique pour créer les successeurs (astar.pl)

```prolog
%! create_successor_nodes(+States:list, +Goal:list, +G:integer, +Parent:compound, -Nodes:list, +GenCountIn:integer, -GenCountOut:integer) is det.
%  Crée les nœuds A* pour tous les états successeurs
create_successor_nodes([State|RestStates], Goal, G, Parent, [Node|RestNodes], GenCountIn, GenCountOut) :-
    GenCountMid is GenCountIn + 1,                    % Incrémenter compteur
    manhattan_distance_heuristic(State, Goal, H),     % Calculer h(n)
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