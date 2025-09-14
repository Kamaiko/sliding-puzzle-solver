# Solveur de Taquin A* - Plan de Développement

## Overview
Projet universitaire IFT-2003 implémentant un solveur de puzzle à 8 cases (Taquin 3x3) utilisant l'algorithme A* avec heuristique des tuiles mal placées. Architecture modulaire en 4 modules Prolog avec interface CLI interactive et validation exacte des résultats académiques.

## 1. Configuration Projet et Structure

### 1.1 Validation de l'environnement
- [ ] Vérifier installation SWI-Prolog compatible
  - Tester compilation des modules existants
  - Valider syntaxe Prolog et directives
- [ ] Configurer environnement de développement
  - Variables d'environnement pour SWI-Prolog
  - Scripts de lancement standardisés

### 1.2 Architecture finale des modules
- [ ] Valider structure des 4 modules principaux
  - `main.pl` (~60 lignes) - Interface CLI et orchestration
  - `game.pl` (~100 lignes) - États du puzzle et transitions
  - `astar.pl` (~150 lignes) - Algorithme A* et heuristiques
  - `display.pl` (~50 lignes) - Formatage et interface utilisateur
  - `tests.pl` (~80 lignes) - Tests unitaires et validation
- [ ] Définir interfaces entre modules
  - Prédicats exportés et importés
  - Conventions de nommage en anglais
  - Gestion des erreurs inter-modules

## 2. Fondations Backend - Logique Métier

### 2.1 Module game.pl - États et Transitions
- [ ] Représentation des états du puzzle
  - Structure de données pour grille 3x3
  - Position de la case vide (0 ou blank)
  - Validation de configuration initiale
- [ ] Génération des mouvements possibles
  - Quatre directions (haut, bas, gauche, droite)
  - Vérification des limites de grille
  - Application des mouvements sur l'état
- [ ] Validation des états
  - Configuration valide vs invalide
  - État objectif standardisé [1,2,3,4,5,6,7,8,0]
  - Détection de configurations impossibles
- [ ] Utilitaires de manipulation
  - Conversion format interne/affichage
  - Comparaison d'états pour équivalence
  - Sérialisation pour debug et tests

### 2.2 Module astar.pl - Algorithme de Recherche
- [ ] Implémentation coeur de l'algorithme A*
  - Structure de noeud (état, parent, g, h, f)
  - Liste ouverte et fermée
  - Sélection du meilleur noeud (f minimal)
- [ ] Heuristique des tuiles mal placées (obligatoire)
  - Calcul excluant la case vide
  - Optimisation pour performance
  - Tests de cohérence heuristique
- [ ] Heuristique Manhattan (optionnelle P1)
  - Distance Manhattan pour chaque tuile
  - Comparaison performance avec tuiles mal placées
- [ ] Reconstruction du chemin solution
  - Remontée des parents jusqu'à l'état initial
  - Format Path: A→B→C→D→E
  - Calcul coût total et nombre d'expansions
- [ ] Optimisations de performance
  - Éviter cycles et doublons
  - Gestion mémoire efficace
  - Limitation profondeur si nécessaire

## 3. Interface Utilisateur - Display et Main

### 3.1 Module display.pl - Formatage ASCII
- [ ] Bannière d'accueil avec ASCII art
  - Logo "TAQUIN" stylisé selon mockup 1
  - Informations université et cours
  - Animation d'introduction
- [ ] Affichage des grilles 3x3
  - Bordures avec caractères +, -, |
  - Case vide représentée par #
  - Alignement et espacement cohérents
- [ ] Écrans formatés selon mockups
  - Menu principal avec preview (mockup 2)
  - Écran de résolution avec barres de progression (mockup 3)
  - Résultats détaillés avec métriques (mockup 4)
  - Animation pas-à-pas (mockup 5)
- [ ] Utilitaires d'affichage
  - Effacement écran et repositionnement curseur
  - Messages d'erreur formatés en français
  - Barres de progression pour calculs longs
- [ ] Support couleurs ANSI (optionnel)
  - Détection capacité terminal
  - Code couleur pour états, mouvements
  - Mode fallback ASCII seulement

### 3.2 Module main.pl - Interface CLI et Orchestration
- [ ] Menu principal interactif
  - Option 1: Cas test professeur (Coût=4, Expansés=9)
  - Option 2: Cas test avancé (6+ mouvements)
  - Option 3: Quitter proprement
- [ ] Gestion des cas de test
  - État initial: [1,2,3,5,0,6,4,7,8] → État final: [1,2,3,4,5,6,7,8,0]
  - Configuration cas test 2 personnalisé
  - Validation pré-résolution des états
- [ ] Boucle principale d'exécution
  - Gestion saisie utilisateur robuste
  - Navigation entre écrans fluide
  - Gestion d'erreurs et récupération
- [ ] Intégration des modules
  - Orchestration appels game.pl ↔ astar.pl ↔ display.pl
  - Gestion des résultats et métriques
  - Mesure temps d'exécution de l'IA
- [ ] Animation et interactivité (P1)
  - Mode pas-à-pas de la solution
  - Contrôles utilisateur (N, P, A, M, Q)
  - Feedback visuel temps réel

## 4. Intégration et Validation Académique

### 4.1 Validation des résultats exacts
- [ ] Cas test 1 - Validation obligatoire TP1
  - Configuration: [1,2,3,5,0,6,4,7,8] vers [1,2,3,4,5,6,7,8,0]
  - Résultats attendus: Path=5 états, Cost=4, Expanded=9
  - Vérification séquence exacte A→B→C→D→E
- [ ] Cas test 2 - Configuration personnalisée
  - Minimum 6 mouvements pour solution
  - Validation chemin optimal trouvé
  - Métriques cohérentes avec complexité
- [ ] Tests de régression
  - Stabilité résultats sur multiples exécutions
  - Cohérence temps d'exécution raisonnables
  - Validation format sortie exact

### 4.2 Intégration inter-modules
- [ ] Tests d'intégration game.pl ↔ astar.pl
  - États générés valides pour recherche
  - Heuristiques cohérentes avec logique métier
  - Transitions d'états conformes aux règles
- [ ] Tests d'intégration astar.pl ↔ display.pl
  - Format résultats compatible affichage
  - Métriques correctement transmises
  - Chemin solution exploitable par UI
- [ ] Tests d'intégration main.pl ↔ tous modules
  - Orchestration complète sans erreurs
  - Gestion d'erreurs propagée correctement
  - Interface utilisateur cohérente

## 5. Module tests.pl - Tests et Qualité

### 5.1 Tests unitaires par module
- [ ] Tests game.pl
  - Génération mouvements valides
  - Validation d'états corrects/incorrects
  - Transitions d'états déterministes
- [ ] Tests astar.pl
  - Fonctionnement heuristiques isolées
  - Algorithme A* sur cas simples
  - Reconstruction chemin cohérente
- [ ] Tests display.pl
  - Formatage grilles sans erreurs
  - Cohérence affichage multi-plateforme
  - Gestion caractères spéciaux ASCII
- [ ] Tests main.pl
  - Navigation menu sans blocage
  - Gestion saisies utilisateur invalides
  - Orchestration modules complète

### 5.2 Tests d'intégration complets
- [ ] Scénarios end-to-end
  - Lancement → Menu → Cas test 1 → Résultats → Retour menu
  - Test cas test 2 complet avec validation
  - Scénarios d'erreur et récupération
- [ ] Tests de performance
  - Temps d'exécution acceptable (<5s cas standard)
  - Utilisation mémoire raisonnable
  - Stabilité sur résolutions multiples
- [ ] Tests de validation académique
  - Reproduction exacte des résultats attendus
  - Format de sortie conforme spécifications TP1
  - Couverture de tous les cas de test requis

### 5.3 Framework de tests automatisés
- [ ] Prédicat run_all_tests principal
  - Exécution séquentielle tous tests unitaires
  - Agrégation résultats et reporting
  - Identification tests échoués clairement
- [ ] Utilitaires de test
  - Assertions pour validation résultats
  - Mocking pour isolation modules
  - Benchmarking pour performance
- [ ] Rapports de tests
  - Format lisible pour développeurs
  - Métriques de couverture
  - Historique des régressions

## 6. Documentation et Finalisation

### 6.1 Documentation code (en français)
- [ ] Commentaires dans chaque module
  - Explication algorithmes et heuristiques
  - Documentation des prédicats principaux
  - Examples d'utilisation et cas limites
- [ ] Documentation architecture
  - Diagramme des interactions entre modules
  - Explication des choix de conception
  - Justification de l'approche A*
- [ ] Guide d'utilisation utilisateur final
  - Procédure d'installation et lancement
  - Explication interface et navigation
  - Interprétation des résultats affichés

### 6.2 Documentation académique
- [ ] Rapport technique TP1
  - Explication théorique de l'algorithme A*
  - Justification choix heuristique
  - Analyse des résultats obtenus
- [ ] Documentation développeur
  - Standards de code respectés
  - Processus de test et validation
  - Instructions maintenance et évolution
- [ ] Documentation projet
  - Architecture finale documentée
  - Décisions techniques justifiées
  - Métriques de qualité atteintes

### 6.3 Livraison et présentation
- [ ] Package de livraison
  - Structure de répertoires standardisée
  - Scripts de lancement documentés
  - Fichiers readme et installation
- [ ] Validation finale TP1
  - Tests tous les cas requis
  - Format de sortie exact conforme
  - Performance et stabilité confirmées
- [ ] Préparation démonstration
  - Scénarios de démonstration préparés
  - Réponses aux questions techniques anticipées
  - Interface soignée et professionnelle

## 7. Optimisations et Améliorations (P1)

### 7.1 Performance et robustesse
- [ ] Optimisation algorithme A*
  - Structures de données efficaces
  - Réduction complexité temporelle
  - Gestion mémoire améliorée
- [ ] Validation robuste entrées
  - Gestion configurations impossibles
  - Messages d'erreur informatifs
  - Récupération gracieuse d'erreurs
- [ ] Tests de charge et limites
  - Configurations difficiles (20+ mouvements)
  - Stabilité sur exécutions prolongées
  - Limites mémoire et temps

### 7.2 Expérience utilisateur avancée
- [ ] Fonctionnalités interactives avancées
  - Mode animation pas-à-pas fluide
  - Contrôles temps réel pendant calcul
  - Statistiques détaillées et comparaisons
- [ ] Interface utilisateur polie
  - Transitions écrans fluides
  - Feedback visuel pendant calculs longs
  - Messages d'aide contextuels
- [ ] Options de configuration
  - Choix heuristique (tuiles mal placées vs Manhattan)
  - Modes d'affichage (compact vs détaillé)
  - Sauvegarde/restauration de configurations

## Notes d'Implémentation

### Conventions Techniques
- **Langage code**: Anglais (variables, fonctions, prédicats)
- **Documentation**: Français (commentaires, UI, messages d'erreur)
- **Format case vide**: # pour affichage, 0 pour logique interne
- **Architecture**: 4 modules autonomes avec interfaces définies

### Contraintes Académiques
- **Résultat TP1 exact**: Cost=4, Expanded=9, Path A→B→C→D→E
- **Heuristique obligatoire**: Tuiles mal placées (exclusion case vide)
- **Format sortie**: Path/Cost/Expanded selon spécifications
- **Validation**: Compilation et exécution sans erreurs

### Répartition Équipe (4 développeurs)
- **Dev 1**: main.pl + coordination projet
- **Dev 2**: astar.pl + algorithme coeur
- **Dev 3**: game.pl + logique métier
- **Dev 4**: display.pl + tests.pl + qualité