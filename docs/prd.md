# 🎯 Document d'Exigences Produit (PRD)
## Solveur de Taquin avec Recherche Heuristique - Projet Universitaire IFT-2003

> **Échéance projet** : 📅 20 octobre 2025

## Table des Matières

1. [Aperçu Produit](#aperçu-produit)
2. [Objectifs Académiques](#objectifs-académiques)
3. [Utilisateurs Cibles](#utilisateurs-cibles)
4. [Exigences Fonctionnelles](#exigences-fonctionnelles)
5. [Expérience Utilisateur](#expérience-utilisateur)
6. [Vision Étudiante (User Story)](#vision-étudiante-user-story)
7. [Critères de Succès](#critères-de-succès)
8. [Contraintes Techniques](#contraintes-techniques)
9. [Phases Développement](#phases-développement)
10. [Cas d'Usage Essentiels](#cas-dusage-essentiels)

---

## Aperçu Produit

| Aspect | Description |
|--------|-------------|
| **Nom du projet** | Solveur de Taquin (Puzzle à 8 cases) avec algorithme A* |
| **Type de projet** | Travail pratique universitaire - Intelligence Artificielle |
| **Plateforme** | SWI-Prolog, interface en ligne de commande |
| **Utilisateurs** | Professeur, étudiants développeurs, évaluateurs pairs |
| **Objectif principal** | Implémenter un solveur optimal pour le puzzle 3x3 en utilisant la recherche heuristique A* |
| **Pondération** | 10% de la note finale du cours |
| **Langage** | Prolog (SWI-Prolog) exclusivement |

## Objectifs Académiques

- ✅ **Maîtriser la recherche heuristique A*** avec distance Manhattan
- ✅ **Programmation logique modulaire** en Prolog SWI
- ✅ **Validation algorithmique précise** (Coût=4, Expanded=12, <3ms)

## Utilisateurs Cibles

| Utilisateur | Rôle | Besoins |
|-------------|------|---------|
| **Professeur** | Évaluateur académique | Validation des résultats exacts, évaluation de la qualité du code, vérification de la compréhension |
| **Étudiant développeur** | Utilisateur principal | Interface claire, guide d'utilisation, temps de réponse acceptables, débogage facilité |
| **Évaluateurs pairs** | Observateurs de démonstration | Démonstration claire du fonctionnement, résultats visibles, explications compréhensibles |

## Exigences Fonctionnelles

### Obligatoires (P0) - Cœur du projet

- **Algorithme A*** : Implémentation complète avec recherche heuristique
- **Heuristique de distance Manhattan** : Calcul somme distances L1, exclusion case vide (0)
- **Architecture modulaire** : 4 modules distincts (main.pl, game.pl, astar.pl, display.pl) + tests.pl
- **Cas de test 1 (classique)** : [1,2,3,5,0,6,4,7,8] → Coût=4, Expanded=12, <3ms
- **Cas de test 2 (avancé)** : [1,3,6,5,2,8,4,0,7] → Coût=9, Expanded=25, <3ms
- **Sortie formatée** : Path (A→E), Cost, Expanded avec temps d'exécution IA
- **Documentation complète** : Rapport académique, guide utilisation, code commenté
- **Menu principal** : Interface CLI avec navigation intuitive (sélection 1/2/3)
- **Qualité zéro défaut** : Compilation et exécution sans erreurs ni warnings

### Importantes (P1)

- **Affichage formaté** : Grilles 3×3 visuelles, métriques détaillées, temps d'exécution
- **Validation robuste** : Gestion erreurs, configurations invalides, solvabilité
- **Suite de tests** : 14 tests unitaires + 2 tests d'intégration automatisés

## Expérience Utilisateur

| Étape | Action | Résultat attendu |
|-------|--------|------------------|
| **Lancement** | `swipl run.pl` | Menu principal affiché avec 3 options claires |
| **Sélection cas test** | Choix option 1 (classique) ou 2 (avancé) | Configuration initiale affichée en grille 3×3 |
| **Résolution automatique** | A* s'exécute automatiquement | Séquence d'états affichée (A→B→...→E) |
| **Métriques finales** | Fin de résolution | Path/Cost/Expanded + temps IA (<3ms) |
| **Navigation** | Sélection option 3 | Retour propre au système (exit) |

## Vision Étudiante (User Story)

> "En tant qu'étudiant en intelligence artificielle, je veux utiliser un solveur de Taquin qui me permet de comprendre concrètement le fonctionnement de l'algorithme A* avec l'heuristique de distance Manhattan. L'outil doit me fournir tous les détails nécessaires (Path, Cost, Expanded, temps d'exécution) pour analyser l'efficacité de la recherche heuristique et valider ma compréhension théorique par des résultats pratiques mesurables et reproductibles."

## Critères de Succès

| Critère | Objectif | Status |
|---------|----------|--------|
| **Validation académique cas 1** | Cost=4, Expanded=12, <3ms pour [1,2,3,5,0,6,4,7,8] | ✅ Atteint |
| **Validation académique cas 2** | Cost=9, Expanded=25, <3ms pour [1,3,6,5,2,8,4,0,7] | ✅ Atteint |
| **Qualité code** | Zéro erreur compilation/exécution, PlDoc complet | ✅ Atteint |
| **Tests automatisés** | 16 tests (14 unitaires + 2 intégration) passent à 100% | ✅ Atteint |
| **Documentation complète** | Rapport académique 5 pages, README, architecture.md | ✅ Atteint |

## Contraintes Techniques

- **SWI-Prolog 9.x** exclusivement, interface CLI ASCII pure
- **Heuristique choisie** : Distance Manhattan (admissible et consistante)
- **Architecture modulaire** : 4 modules principaux + tests (main.pl, game.pl, astar.pl, display.pl, tests.pl)
- **Format sortie académique** : Path (A→E) / Cost (mouvements) / Expanded (nœuds) / Temps IA
- **Performance requise** : <3ms pour tous les cas de test
- **Ordre génération** : UP, DOWN, LEFT, RIGHT (déterministe)

## Phases Développement

1. **Phase 1 - Modélisation** ✅ : Représentation états (liste 9 éléments), transitions (UP/DOWN/LEFT/RIGHT), validation solvabilité
2. **Phase 2 - Algorithme A*** ✅ : Implémentation complète avec Manhattan, open/closed lists, reconstruction chemin
3. **Phase 3 - Interface** ✅ : Menu CLI 3 options, affichage grille 3×3, séquence états avec lettres (A-J)
4. **Phase 4 - Tests et validation** ✅ : 16 tests automatisés, validation métriques exactes (Cost/Expanded)
5. **Phase 5 - Documentation** ✅ : Rapport académique 5 pages, README complet, PlDoc inline, diagramme SVG

## Cas d'Usage Essentiels

| ID | Cas d'usage | Entrée | Sortie attendue | Status |
|----|-------------|--------|-----------------|--------|
| **CU-001** | Menu principal | `swipl run.pl` | 3 options affichées (1: Classique, 2: Avancé, 3: Quitter) | ✅ |
| **CU-002** | Résolution cas classique | Option 1 sélectionnée | Cost=4, Expanded=12, Path A→E (5 états), <3ms | ✅ |
| **CU-003** | Résolution cas avancé | Option 2 sélectionnée | Cost=9, Expanded=25, Path A→J (10 états), <3ms | ✅ |
| **CU-004** | Affichage séquence | Après résolution | Grilles 3×3 pour chaque état (A, B, C...) | ✅ |
| **CU-005** | Métriques finales | Fin d'exécution | Path / Cost / Expanded / Temps IA formatés | ✅ |
| **CU-006** | Tests automatisés | `swipl -g run_all_tests src/tests.pl` | 16/16 tests PASS | ✅ |
| **CU-007** | Gestion erreurs | Configuration non-solvable | Message d'erreur clair | ✅ |