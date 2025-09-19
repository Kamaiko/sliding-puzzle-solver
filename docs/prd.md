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

- ✅ **Maîtriser la recherche heuristique A*** avec tuiles mal placées
- ✅ **Programmation logique modulaire** en Prolog SWI
- ✅ **Validation algorithmique précise** (Coût=4, Expansés=12)

## Utilisateurs Cibles

| Utilisateur | Rôle | Besoins |
|-------------|------|---------|
| **Professeur** | Évaluateur académique | Validation des résultats exacts, évaluation de la qualité du code, vérification de la compréhension |
| **Étudiant développeur** | Utilisateur principal | Interface claire, guide d'utilisation, temps de réponse acceptables, débogage facilité |
| **Évaluateurs pairs** | Observateurs de démonstration | Démonstration claire du fonctionnement, résultats visibles, explications compréhensibles |

## Exigences Fonctionnelles

### Obligatoires (P0) - Cœur du projet

- **Algorithme A*** : Implémentation complète avec recherche heuristique
- **Heuristique des tuiles mal placées** : Exclusion de la case vide dans le calcul
- **Architecture modulaire** : 4 modules distincts (main.pl, game.pl, astar.pl, display.pl, tests.pl)
- **Cas de test 1** : Exemple du professeur avec résultats exacts (Coût=4, Expansés=12)
- **Cas de test 2** : Exemple personnalisé avec minimum 6 mouvements
- **Sortie formatée** : Path complet, coût de la solution, nombre de nœuds expansés
- **Documentation** : Guide d'utilisation et explication du code heuristique
- **Menu principal** : Interface CLI avec options de navigation
- **Gestion d'erreurs** : Compilation et exécution sans erreurs

### Importantes (P1)

- **Affichage formaté** : Configurations 3x3 et temps d'exécution IA
- **Validation robuste** : Gestion erreurs et configurations invalides

## Expérience Utilisateur

| Étape | Action | Résultat attendu |
|-------|--------|------------------|
| **Lancement** | Exécution de main.pl | Menu principal affiché avec options claires |
| **Sélection cas test** | Choix du cas de test 1 ou 2 | Configuration initiale et finale affichées |
| **Résolution** | Lancement du solveur A* | Chemin complet affiché (A→B→C→D→E) |
| **Résultats** | Fin de résolution | Coût, nombre d'expansions, temps d'exécution affichés |
| **Navigation** | Retour au menu | Possibilité de tester autres configurations |

## Vision Étudiante (User Story)

> "En tant qu'étudiant en intelligence artificielle, je veux utiliser un solveur de Taquin qui me permet de comprendre concrètement le fonctionnement de l'algorithme A* et des heuristiques. L'outil doit me fournir tous les détails nécessaires pour analyser l'efficacité de la recherche heuristique et valider ma compréhension théorique par des résultats pratiques mesurables."

## Critères de Succès

| Critère | Objectif |
|---------|----------|
| **Validation académique** | Coût=4, Expansés=9 exactement pour cas professeur |
| **Qualité code** | Compilation/exécution sans erreur, documentation complète |
| **Tests complets** | 2 cas fonctionnels avec métriques vérifiables |

## Contraintes Techniques

- **SWI-Prolog** exclusivement, interface CLI obligatoire
- **Heuristique imposée** : Tuiles mal placées (sans case vide)
- **Architecture** : 4 modules (main, game, astar, display, tests)
- **Format sortie** : Path/Cost/Expanded selon spécifications académiques

## Phases Développement

1. **Phase 1** - Modélisation : Représentation des états et transitions
2. **Phase 2** - Algorithme A* : Implémentation de la recherche heuristique
3. **Phase 3** - Interface : Menu CLI et affichage formaté
4. **Phase 4** - Tests et validation : Cas de test et vérification des résultats
5. **Phase 5** - Documentation : Guide d'utilisation et commentaires code

## Cas d'Usage Essentiels

- **CU-001** : Menu principal et navigation CLI
- **CU-002** : Résolution cas test 1 → Coût=4, Expansés=9, Path A→B→C→D→E  
- **CU-003** : Résolution cas test 2 → Minimum 6 mouvements
- **CU-004** : Affichage métriques (Path/Cost/Expanded + temps IA)
- **CU-005** : Gestion erreurs et configurations invalides
- **CU-006** : Tests automatisés complets via tests.pl