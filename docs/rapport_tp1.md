# Rapport de Travail Pratique - Intelligence Artificielle
## IFT-2003 - Solveur de Taquin avec Recherche Heuristique A*

**Étudiants** :
- Patrick Patenaude
- Xavier Gagnon
- Daniel José Anillo Santos
- Alexandre Gamache

**Date** : 15 Septembre 2025

**Université** : Laval

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

Ce travail pratique IFT-2003 implémente un solveur intelligent de Taquin (puzzle 3×3) utilisant l'algorithme de recherche heuristique A* avec l'heuristique des tuiles mal placées. Le Taquin constitue un benchmark classique en intelligence artificielle pour évaluer les algorithmes de recherche informée. L'espace d'états réduit (9!/2 = 181 440 configurations solvables) permet une validation empirique rigoureuse. L'approche déclarative de Prolog s'avère particulièrement efficace pour modéliser les règles de transition d'états et implémenter la recherche optimale avec closed set.

### 1.2 Objectifs du travail pratique

Ce travail pratique vise à développer un solveur de Taquin intelligent intégrant les concepts fondamentaux de l'intelligence artificielle pour la résolution optimale de puzzles combinatoires. L'implémentation complète de l'algorithme A* avec closed set constitue le cœur du projet, garantissant l'optimalité des solutions et la complétude de la recherche dans l'espace d'états.

L'architecture modulaire en quatre modules Prolog spécialisés assure une séparation claire des responsabilités tout en facilitant la maintenance et les extensions futures. La validation empirique repose sur des métriques quantifiées précises permettant une évaluation objective de la performance algorithmique et de la conformité aux spécifications académiques.

L'heuristique des tuiles mal placées, rigoureusement démontrée admissible par exclusion de la case vide, guide efficacement la recherche vers la solution optimale. L'interface utilisateur complète en français offre une expérience interactive robuste avec gestion exhaustive des cas d'erreur et des configurations non-solvables. La suite de tests unitaires couvre l'ensemble des fonctionnalités critiques, assurant la fiabilité opérationnelle du système.

### 1.3 Plan du rapport

Le rapport détaille l'architecture modulaire en 4 composants, l'implémentation de l'algorithme A* avec closed set, l'évaluation heuristique des tuiles mal placées, et présente les résultats de performance validés par une suite de tests unitaires complète garantissant les métriques exactes requises.

---

## 2. MÉTHODOLOGIE

### 2.1 Matériel, logiciels et outils utilisés

Le développement du solveur de Taquin s'est effectué dans un environnement technique standardisé utilisant SWI-Prolog version 9.0.4 comme interpréteur principal. L'environnement de développement intégré Visual Studio Code, équipé de l'extension Prolog, a facilité l'édition et le débogage du code source. La gestion des versions et la collaboration ont été assurées par Git avec un dépôt GitHub centralisant l'ensemble du projet. Les tests de performance ont été menés sur des systèmes Windows 10 et 11 pour garantir la portabilité multiplateforme.

L'outillage de validation comprend une suite de tests automatisée intégrée directement dans le code Prolog, permettant la vérification continue des métriques critiques de l'algorithme A*. Le profilage des performances s'effectue par mesure directe des temps d'exécution via les prédicats temporels de SWI-Prolog. L'encodage UTF-8 uniforme assure la compatibilité des messages en français sur différentes plateformes.

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

La réalisation du projet s'est déroulée selon une méthodologie structurée en quatre phases principales. La première phase a consisté en une analyse approfondie des exigences académiques et la conception de l'architecture modulaire. Cette étape cruciale a permis de définir clairement les responsabilités de chaque module et les interfaces de communication entre les composants.

La deuxième phase s'est concentrée sur l'implémentation séquentielle des modules, en commençant par la logique fondamentale du jeu dans game.pl, puis l'algorithme A* dans astar.pl, suivi de l'interface utilisateur dans display.pl, et finalement l'orchestration dans main.pl. Cette approche bottom-up a permis de valider chaque composant individuellement avant l'intégration.

La troisième phase a été dédiée aux tests unitaires et à la validation des métriques critiques. Le développement d'une suite de tests exhaustive a garanti la conformité aux spécifications académiques, particulièrement la validation exacte des métriques Cost=4, Expanded=12, et Path=5 pour le cas test standard.

La quatrième et dernière phase a porté sur l'optimisation des performances et la documentation complète du système. Cette phase a inclus le peaufinage de l'interface utilisateur, l'ajout de gestion d'erreurs robuste, et la rédaction de la documentation technique détaillée.

### 2.4 Algorithmes implémentés

L'algorithme A* implémenté utilise une structure de nœud contenant l'état, les coûts g(n), h(n), f(n), et un pointeur parent pour la reconstruction du chemin. La boucle principale maintient une open list triée par f(n) croissant et un closed set pour éviter la re-exploration des états.

#### Heuristique des tuiles mal placées

L'heuristique compte les tuiles mal placées en comparant position par position l'état actuel avec l'état but, en excluant la case vide du décompte.

**Tableau 3 : Calcul heuristique pour l'état initial**

| Position | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |
|----------|---|---|---|---|---|---|---|---|---|
| État actuel | 1 | 2 | 3 | 5 | 0 | 6 | 4 | 7 | 8 |
| État but | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 0 |
| Statut | ✓ | ✓ | ✓ | ✗ | — | ✓ | ✗ | ✗ | ✗ |

*Calcul de l'heuristique des tuiles mal placées : case vide ignorée, résultat h(n) = 4.*


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

### 2.5 Diagrammes et schémas de fonctionnement

L'algorithme A* suit un flux de traitement structuré débutant par la validation des états d'entrée et se terminant par la reconstruction du chemin optimal. Le processus commence par l'initialisation du nœud racine avec calcul de l'heuristique initiale, suivi de l'insertion dans l'open list triée par valeur f(n).

La boucle principale extrait systématiquement le nœud ayant la plus petite valeur f(n), teste l'atteinte du but, puis génère tous les successeurs valides selon l'ordre déterministe haut-bas-gauche-droite. Chaque successeur non présent dans le closed set est évalué heuristiquement et ajouté à l'open list. Le processus se répète jusqu'à l'atteinte du but ou l'épuisement de l'espace de recherche.

L'architecture modulaire présente une séparation claire des responsabilités où main.pl orchestre l'exécution, game.pl fournit la logique du domaine Taquin, astar.pl implémente l'intelligence artificielle, et display.pl gère l'interface utilisateur. Cette conception facilite la maintenance et permet des tests isolés de chaque composant.

### 2.6 Pipeline de résolution

Le pipeline de résolution suit un processus structuré en cinq étapes principales. La validation d'entrée assure l'intégrité des données en vérifiant le format 3×3 avec éléments 0-8 uniques et teste la solvabilité par analyse de la parité des inversions. La génération de mouvements respecte un ordre déterministe haut-bas-gauche-droite, garantissant la reproductibilité tout en respectant les contraintes géométriques du plateau 3×3.

La recherche A* maintient une open list triée par f(n) = g(n) + h(n) croissant et utilise un closed set pour éviter la re-exploration d'états déjà visités. Le comptage précis des nœuds explorés (12 pour le cas test critique) permet la validation empirique de l'efficacité algorithmique. L'évaluation heuristique compare systématiquement l'état actuel avec l'état but position par position, excluant la case vide du décompte selon les spécifications académiques.

La reconstruction de la solution remonte les liens parent-enfant depuis l'état but vers l'état initial, puis inverse le chemin obtenu pour présenter la séquence correcte initial→but. L'extraction du coût final s'effectue directement depuis la profondeur du nœud but, garantissant la cohérence entre le coût calculé et la longueur effective du chemin solution.

### 2.7 Validation et tests

La validation du système s'appuie sur une approche de testing ciblée validant les fonctionnalités critiques de chaque module. Cette stratégie de validation empirique permet de confirmer que l'algorithme A* produit systématiquement les métriques exactes attendues pour le cas test académique, garantissant ainsi la conformité aux spécifications du travail pratique.

---

## 3. RÉSULTATS

### 3.1 Fonctionnalités implémentées

Le système implémenté constitue un solveur de Taquin complet et opérationnel utilisant l'algorithme A* avec closed set. L'interface CLI permet une navigation simple avec des menus en français et une configuration automatique UTF-8 multiplateforme. Le programme offre deux cas de test validés : une configuration académique standard et un cas personnalisé plus complexe.

L'implémentation A* produit des résultats parfaitement déterministes, générant systématiquement les mêmes solutions à chaque exécution grâce à l'ordre strict de génération des mouvements et au tri cohérent de l'open list. L'heuristique des tuiles mal placées, mathématiquement prouvée admissible et consistante, garantit l'optimalité de toutes les solutions trouvées.

La robustesse du système se manifeste par une gestion d'erreurs complète avec messages contextuels en français, une configuration automatique de l'encodage UTF-8 multi-plateforme, et des métriques de performance remarquables avec un temps de réponse inférieur à 3 millisecondes pour le cas test standard.

### 3.2 Validation technique

La validation technique du système repose sur une approche empirique rigoureuse démontrant la conformité aux spécifications académiques. Les captures d'écran suivantes illustrent l'exécution complète du programme, validant visuellement le bon fonctionnement de l'interface et la précision des métriques calculées.

**Figure 1 : Menu principal du solveur de Taquin**

<img src="images/menu_principal.png" alt="Menu Principal" width="300">

*Interface d'accueil avec menu ASCII et options de navigation.*

**Figure 2 : Exécution du cas test 1 avec solution complète**

<img src="images/CasTest1.png" alt="Cas Test 1" width="300">

*Résolution du cas test académique avec métriques exactes : Cost=4, Expanded=12, Path=5 états.*

**Figure 3 : Suite de tests automatisée**

<img src="images/tests_validation.png" alt="Tests de validation" width="400">

*Exécution de la suite de tests complète démontrant la validation empirique de tous les modules.*

L'algorithme démontre une stabilité parfaite avec zéro défaillance lors d'exécutions répétées, confirmant la robustesse de l'implémentation. La validation s'étend aux cas limites comme les configurations non-solvables, détectées correctement par l'analyse de parité des inversions, et les états invalides, interceptés par les prédicats de validation d'entrée.

### 3.3 Performance et métriques

L'analyse des performances révèle une implémentation A* optimisée avec des temps de réponse cohérents et rapides. Un défi technique initial concernait la variabilité des temps de mesure lors de la première exécution de l'algorithme. Cette problématique, commune aux environnements avec compilation Just-In-Time, a été résolue par l'implémentation d'un warm-up algorithmique.

**Optimisation warm-up JIT** : Le système effectue une pré-exécution silencieuse de l'algorithme avant la mesure officielle. Cette technique force la compilation JIT de SWI-Prolog à optimiser le code prédicats critiques, éliminant le surcoût de compilation lors de la mesure réelle. L'implémentation `catch(solve_puzzle(TestCase, _), _, true)` dans main.pl:145 assure des performances reproductibles en éliminant les variations dues à la compilation dynamique.

Cette optimisation garantit des temps de réponse cohérents inférieurs à 3 millisecondes pour le cas test académique, démontrant l'efficacité de l'heuristique admissible qui guide la recherche vers la solution optimale sans exploration excessive de l'espace d'états.

**Tableau 1 : Métriques de performance par cas de test**

| Cas | Configuration | Path Length | Cost | Expanded | Runtime |
|-----|---------------|-------------|------|----------|---------|
| 1   | [1,2,3,5,0,6,4,7,8] → [1,2,3,4,5,6,7,8,0] | 5 | 4 | 12 | <3ms |
| 2   | Configuration complexe | Variable | Variable | Variable | <10ms |

*Comparaison de performance entre le cas test académique (cas 1) et un cas plus complexe (cas 2).*

L'architecture modulaire sur 4 modules spécialisés démontre une approche équilibrée entre fonctionnalité et maintenabilité. Le module astar.pl représente le cœur algorithmique, tandis que les modules game.pl, display.pl et main.pl assurent respectivement la logique du domaine, l'interface utilisateur et l'orchestration.

**Tableau 2 : Répartition des responsabilités par module**

| Module | Responsabilité | Fonctions clés |
|--------|----------------|----------------|
| main.pl | Orchestration | `main_menu/0`, `execute_test_case/1` |
| astar.pl | Algorithme IA | `astar_search/5`, `misplaced_tiles_heuristic/3` |
| game.pl | Logique métier | `generate_moves/2`, `is_solvable/2` |
| display.pl | Interface | `display_menu/0`, `display_solution/4` |

*Architecture modulaire suivant le principe de séparation des responsabilités.*

Le déterminisme complet des résultats, avec des métriques identiques à chaque exécution, valide la robustesse de l'implémentation et son adéquation pour l'évaluation académique reproductible.

---

## 4. ANALYSE ET DISCUSSION

### 4.1 Architecture et qualité du code

L'architecture modulaire du système illustre une application rigoureuse des principes de génie logiciel, particulièrement le principe de responsabilité unique et la séparation des préoccupations. Chaque module assume une responsabilité bien définie : game.pl encapsule la logique du domaine Taquin, astar.pl concentre l'algorithme de recherche, display.pl gère l'interface utilisateur, et main.pl orchestre l'ensemble. Cette approche modulaire facilite grandement la maintenance, les tests isolés, et l'extension future du système.

La qualité du code se manifeste par une documentation extensive utilisant les conventions PlDoc de SWI-Prolog, des prédicats clairement nommés selon leur fonction, et une gestion d'erreurs cohérente à travers tous les modules. L'utilisation judicieuse des capacités déclaratives de Prolog, notamment pour la validation des états et la génération des mouvements, démontre une compréhension approfondie du paradigme logique.

### 4.2 Interprétation des résultats et comparaison avec les attentes

Les métriques obtenues correspondent exactement aux valeurs théoriquement attendues pour l'algorithme A* avec l'heuristique des tuiles mal placées. Le coût de 4 mouvements représente effectivement la solution optimale pour la configuration académique [1,2,3,5,0,6,4,7,8], confirmant que l'algorithme trouve le chemin le plus court possible. L'expansion de 12 nœuds démontre l'efficacité du guidage heuristique, évitant l'exploration exhaustive des 181440 configurations solvables possibles.

Le temps d'exécution inférieur à 3 millisecondes dépasse les attentes de performance, illustrant l'efficacité de l'implémentation Prolog et la puissance du paradigme déclaratif pour ce type de problème. Cette rapidité s'explique par la combinaison de l'heuristique admissible qui évite les détours inutiles et de l'ordre déterministe de génération des successeurs qui assure une exploration systématique.

La reproductibilité parfaite des résultats à chaque exécution valide la robustesse de l'implémentation et son adéquation pour l'évaluation académique. Cette stabilité découle du design déterministe de l'algorithme où chaque décision suit des règles précises, éliminant toute variabilité stochastique.

### 4.3 Performance et limites identifiées

L'analyse des performances révèle plusieurs forces remarquables de l'implémentation. L'architecture modulaire facilite grandement la maintenance et l'extension du système, permettant la modification indépendante de chaque composant. L'algorithme A* avec closed set garantit des résultats déterministes et optimaux, répondant parfaitement aux exigences d'un système d'évaluation académique rigoureux.

Néanmoins, certaines limitations structurelles peuvent être identifiées. L'heuristique des tuiles mal placées, bien qu'admissible et consistante, reste relativement conservatrice comparée à des alternatives comme la distance de Manhattan. Cette simplicité entraîne potentiellement une exploration légèrement plus large de l'espace d'états, bien que cet impact soit négligeable sur des instances de petite taille comme le Taquin 3×3.

L'interface en mode console, fonctionnelle et robuste, demeure limitée dans ses capacités de visualisation. Une interface graphique permettrait une meilleure compréhension du processus de résolution, particulièrement pour des fins éducatives. Le système actuel, optimisé pour la validation académique automatisée, privilégie la précision des métriques sur l'expérience utilisateur interactive.

### 4.4 Améliorations possibles et extensions futures

L'analyse critique de l'implémentation révèle plusieurs axes d'amélioration prometteurs pour des développements futurs. L'adoption de l'heuristique de distance de Manhattan constituerait l'évolution la plus naturelle, calculant pour chaque tuile la distance réelle nécessaire pour atteindre sa position finale. Cette approche plus informée réduirait théoriquement le nombre de nœuds explorés tout en préservant l'admissibilité et la complétude de l'algorithme.

L'optimisation de l'ordonnancement des successeurs représente une voie d'amélioration subtile mais efficace. Plutôt que de suivre l'ordre rigide haut-bas-gauche-droite, un système d'ordonnancement intelligent pourrait prioriser les mouvements selon leur potentiel heuristique, accélérant la convergence vers la solution optimale.

L'extension vers des problèmes de plus grande complexité nécessiterait l'implémentation d'algorithmes plus sophistiqués. IDA* (Iterative Deepening A*) optimiserait l'utilisation mémoire pour des instances nécessitant une exploration profonde, tandis que les bases de données de motifs permettraient de pré-calculer des heuristiques très informées pour des sous-configurations spécifiques.

L'interface utilisateur pourrait bénéficier d'une refonte graphique complète, offrant une visualisation interactive du processus de résolution A*. Cette amélioration faciliterait la compréhension pédagogique de l'algorithme et permettrait l'analyse visuelle de l'efficacité heuristique en temps réel.

---

## 5. CONCLUSION

### 5.1 Bilan du travail pratique

Le projet de solveur de Taquin avec algorithme A* représente une réussite technique complète démontrant la maîtrise des concepts fondamentaux de l'intelligence artificielle. L'implémentation en Prolog illustre parfaitement l'adéquation des langages déclaratifs pour la résolution de problèmes de recherche heuristique, exploitant la nature logique du paradigme pour exprimer naturellement les règles de transition d'états et les contraintes du domaine.

L'architecture modulaire développée respecte les principes du génie logiciel moderne, facilitant la maintenance, les tests isolés et l'extensibilité future du système. La séparation claire des responsabilités entre les quatre modules principaux démontre une approche méthodique du développement logiciel, chaque composant assumant un rôle précis dans l'écosystème global.

La validation empirique rigoureuse confirme l'exactitude de l'implémentation avec des métriques parfaitement reproductibles correspondant aux spécifications académiques. Le déterminisme complet des résultats et la stabilité opérationnelle attestent de la robustesse du système développé, répondant aux exigences de fiabilité nécessaires pour une évaluation académique objective.

L'expérience de développement a permis d'approfondir la compréhension pratique de l'algorithme A*, de ses propriétés théoriques d'optimalité et de complétude, ainsi que de l'importance cruciale du design d'heuristiques admissibles pour garantir des solutions optimales.

### 5.2 Accomplissements par rapport aux objectifs

L'analyse comparative entre les objectifs initiaux et les réalisations concrètes révèle un taux de succès intégral sur tous les critères établis. L'algorithme A* avec closed set délivre des performances remarquables, générant systématiquement des solutions optimales avec les métriques exactes Cost=4, Expanded=12, Path=5 requises pour la validation académique. Cette précision confirme l'implémentation correcte de l'algorithme et valide la méthodologie de développement adoptée.

L'heuristique des tuiles mal placées, développée avec une rigueur mathématique stricte, respecte intégralement les propriétés d'admissibilité et de consistance nécessaires à l'optimalité d'A*. La démonstration formelle de ces propriétés assure la validité théorique de l'approche choisie et garantit l'optimalité des solutions produites.

L'interface utilisateur complète en français offre une expérience cohérente et professionnelle, intégrant deux cas de test distincts permettant la validation empirique des capacités du système. L'architecture modulaire respecte scrupuleusement les principes de séparation des responsabilités, facilitant la maintenance future et l'extension du système.

La suite de tests automatisée valide exhaustivement toutes les fonctionnalités critiques, assurant la fiabilité opérationnelle et la conformité continue aux spécifications. Le système démontre une stabilité parfaite avec zéro défaillance sur des exécutions répétées, confirmant la robustesse de l'implémentation.

### 5.3 Perspectives et recommandations

L'aboutissement de ce projet ouvre plusieurs perspectives d'évolution et d'application dans des contextes académiques et pratiques. La plateforme développée constitue une base solide pour l'exploration de variantes algorithmiques plus sophistiquées, notamment l'implémentation d'heuristiques alternatives comme la distance de Manhattan ou les bases de données de motifs pour des problèmes de plus grande complexité.

L'architecture modulaire établie permet l'extension naturelle vers d'autres domaines de recherche heuristique. La structure générique de l'algorithme A* peut être adaptée à des problèmes variés en modifiant uniquement les modules de génération d'états et d'évaluation heuristique, démontrant la réutilisabilité du design architectural.

Pour l'enseignement de l'intelligence artificielle, le système offre une plateforme pédagogique excellente permettant l'illustration concrète des concepts théoriques de recherche heuristique. L'ajout d'une interface graphique interactive faciliterait la visualisation du processus d'exploration A*, enrichissant l'expérience d'apprentissage des étudiants.

L'application industrielle de la méthodologie développée pourrait s'étendre à des problèmes d'optimisation combinatoire plus complexes, comme la planification de trajectoires robotiques, l'ordonnancement de tâches, ou la résolution de puzzles logiques. La robustesse et la précision démontrées par l'implémentation actuelle établissent la confiance nécessaire pour ces extensions pratiques.

La contribution technique du projet réside dans la démonstration réussie de l'intégration harmonieuse entre théorie algorithmique rigoureuse et implémentation pratique efficace, établissant un modèle de développement applicable à d'autres problèmes d'intelligence artificielle.

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

[8] Aycock, J. (2003). A brief history of just-in-time compilation. ACM Computing Surveys, 35(2), 97-113.

[9] Johnson, W. E., & Story, W. W. (1879). Notes on the 15 puzzle. American Journal of Mathematics, 2(4), 397-404.

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