# 📋 Plan de développement - Solveur Taquin A*

> **Deadline :** 📅 20 octobre 2025
> **Équipe :** 4 développeurs - Architecture 4 modules
> **Objectif critique :** Cost=4, Expanded=9 pour le cas professeur

---

## 🎯 Vue d'ensemble des phases

| Phase | Description | Duration | Priorité |
|-------|-------------|----------|----------|
| **Phase 1** | 🏗️ **Fondations & Architecture** | 2-3 jours | CRITIQUE |
| **Phase 2** | 🧠 **Algorithme A* Core** | 3-4 jours | CRITIQUE |
| **Phase 3** | 🎮 **Logique du jeu** | 2-3 jours | HAUTE |
| **Phase 4** | 🎨 **Interface CLI avancée** | 3-4 jours | HAUTE |
| **Phase 5** | ✅ **Tests & Validation** | 2-3 jours | CRITIQUE |
| **Phase 6** | 📚 **Documentation & Polish** | 1-2 jours | MOYENNE |

---

## 📊 Répartition par développeur

| Dev | Module principal | Responsabilités | Tâches principales |
|-----|------------------|-----------------|-------------------|
| **Dev 1** | `main.pl` | Leadership, intégration, orchestration | Interface CLI, menu, gestion erreurs |
| **Dev 2** | `astar.pl` | Algorithme A*, heuristiques | Recherche, optimisation, validation |
| **Dev 3** | `game.pl` | Logique domaine, états | Mouvements, validation, représentation |
| **Dev 4** | `display.pl` + `tests.pl` | UX, qualité | Affichage, tests, validation |

---

## 🏗️ PHASE 1 : Fondations & Architecture (2-3 jours)

### Dev 1 - Structure projet & main.pl

- [ ] **1.1** Nettoyer la structure des fichiers existants
- [ ] **1.2** Créer la déclaration de module `main.pl` avec exports
- [ ] **1.3** Définir le point d'entrée `main/0` et `main/1`
- [ ] **1.4** Implémenter la boucle principale `main_menu/0`
- [ ] **1.5** Créer `handle_choice/1` pour navigation menu
- [ ] **1.6** Implémenter la gestion d'erreurs de base
- [ ] **1.7** Ajouter la mesure de temps avec `get_time/1`

### Dev 3 - Structure game.pl & états de base

- [ ] **1.8** Créer la déclaration de module `game.pl` avec exports
- [ ] **1.9** Définir les états de test (initial_state, goal_state)
- [ ] **1.10** Implémenter `valid_state/1` pour validation
- [ ] **1.11** Créer `find_blank/2` (position case vide)
- [ ] **1.12** Implémenter les utilitaires de position (coords ↔ linéaire)

### Dev 4 - Structure display.pl & tests.pl

- [ ] **1.13** Créer la déclaration de module `display.pl` avec exports
- [ ] **1.14** Créer la déclaration de module `tests.pl` avec exports
- [ ] **1.15** Implémenter l'affichage basique des états 3x3
- [ ] **1.16** Créer le framework de tests `run_all_tests/0`

### Dev 2 - Structure astar.pl & bases algorithmiques

- [ ] **1.17** Créer la déclaration de module `astar.pl` avec exports
- [ ] **1.18** Définir la structure des nœuds A* : `node(State, F, G, Parent)`
- [ ] **1.19** Implémenter l'heuristique de base `misplaced_tiles/3`
- [ ] **1.20** Créer le framework de l'algorithme A* (squelette)

---

## 🧠 PHASE 2 : Algorithme A* Core (3-4 jours)

### Dev 2 - Implémentation A* complète

- [ ] **2.1** Implémenter `astar_search/6` (entrée principale)
- [ ] **2.2** Créer `astar_loop/6` (boucle principale avec OpenList/ClosedList)
- [ ] **2.3** Implémenter la file de priorité avec `sort_by_f_value/2`
- [ ] **2.4** Créer `compare_f_values/3` pour tri par f(n)
- [ ] **2.5** Implémenter `create_successor_nodes/6`
- [ ] **2.6** Ajouter `reconstruct_path/2` pour reconstruction du chemin
- [ ] **2.7** Créer `solve_puzzle/2` comme interface principale
- [ ] **2.8** **CRITIQUE:** Déboguer pour obtenir exactement Cost=4, Expanded=9

### Dev 3 - Génération de mouvements & intégration

- [ ] **2.9** Implémenter `valid_move/2` (validation par direction)
- [ ] **2.10** Créer `get_target_position/3` (calcul position cible)
- [ ] **2.11** Implémenter `apply_move/3` (application mouvement)
- [ ] **2.12** Créer `generate_moves/2` (tous mouvements valides)
- [ ] **2.13** Implémenter `swap_tiles/4` et `replace_nth0/4`
- [ ] **2.14** Tester l'intégration avec l'algorithme A*

### Dev 4 - Tests critiques de validation

- [ ] **2.15** Créer `test_case1_exact/0` → validation Cost=4, Expanded=9
- [ ] **2.16** Implémenter `test_misplaced_tiles/0` (heuristique)
- [ ] **2.17** Créer les tests de génération de mouvements
- [ ] **2.18** Valider que Path = [A,B,C,D,E] (5 états)

### Dev 1 - Intégration et orchestration

- [ ] **2.19** Connecter les modules (main ↔ astar ↔ game)
- [ ] **2.20** Implémenter les cas de test 1 et 2 dans main.pl
- [ ] **2.21** Ajouter gestion des erreurs algorithme
- [ ] **2.22** Tester l'exécution complète end-to-end

---

## 🎮 PHASE 3 : Logique du jeu complète (2-3 jours)

### Dev 3 - Logique avancée et cas de test 2

- [ ] **3.1** Définir `custom_initial_state/1` et `custom_goal_state/1`
- [ ] **3.2** Implémenter `is_goal/1` et `is_custom_goal/1`
- [ ] **3.3** Créer la validation d'états complexes
- [ ] **3.4** Optimiser la génération de successeurs
- [ ] **3.5** Ajouter des utilitaires de manipulation d'états

### Dev 2 - Heuristiques avancées et optimisations

- [ ] **3.6** Implémenter l'heuristique Manhattan `manhattan_distance/3`
- [ ] **3.7** Créer `heuristic_value/4` (interface unifiée)
- [ ] **3.8** Optimiser la performance de l'algorithme A*
- [ ] **3.9** Ajouter la gestion des états déjà visités (ClosedList optimisée)

### Dev 4 - Tests de logique avancés

- [ ] **3.10** Créer `test_generate_moves/0` (tous les cas)
- [ ] **3.11** Implémenter `test_apply_move/0` (validation mouvements)
- [ ] **3.12** Tester les cas limites (coins, bordures)
- [ ] **3.13** Créer `test_case2_results/0` (cas personnalisé)

---

## 🎨 PHASE 4 : Interface CLI avancée (3-4 jours)

### Dev 1 - Menu principal sophistiqué (selon MOCKUP 2)

- [ ] **4.1** Implémenter l'écran d'accueil animé (MOCKUP 1)
- [ ] **4.2** Créer la bannière ASCII art avec logo "TAQUIN"
- [ ] **4.3** Implémenter le menu principal avec preview (MOCKUP 2)
- [ ] **4.4** Ajouter les statistiques en temps réel
- [ ] **4.5** Créer la navigation fluide entre écrans

### Dev 4 - Affichage sophistiqué et MOCKUPS

- [ ] **4.6** Implémenter l'écran de résolution (MOCKUP 3)
- [ ] **4.7** Créer l'affichage des résultats complet (MOCKUP 4)
- [ ] **4.8** Implémenter l'animation pas-à-pas (MOCKUP 5)
- [ ] **4.9** Ajouter les barres de progression ASCII `[████████████]`
- [ ] **4.10** Créer `display_solution/4` avec statistiques détaillées
- [ ] **4.11** Implémenter `display_path/1` avec flèches et états
- [ ] **4.12** Ajouter les codes couleur ANSI (optionnel)

### Dev 3 - Affichage des états et integration UX

- [ ] **4.13** Améliorer `display_state/2` avec bordures Unicode
- [ ] **4.14** Créer `display_state_compact/1` pour les chemins
- [ ] **4.15** Implémenter le formatage des tuiles (0 → # ou espace)
- [ ] **4.16** Intégrer l'affichage avec la logique du jeu

### Dev 2 - Performance et métriques

- [ ] **4.17** Ajouter le comptage précis des nœuds explorés
- [ ] **4.18** Implémenter les métriques de performance
- [ ] **4.19** Créer l'affichage des statistiques temps réel
- [ ] **4.20** Optimiser les performances pour l'affichage

---

## ✅ PHASE 5 : Tests & Validation (2-3 jours)

### Dev 4 - Suite de tests complète

- [ ] **5.1** Implémenter tous les tests unitaires (`test_game_module/0`)
- [ ] **5.2** Créer les tests A* (`test_astar_module/0`)
- [ ] **5.3** Implémenter les tests d'intégration (`test_integration/0`)
- [ ] **5.4** **CRITIQUE:** Valider exactement Cost=4, Expanded=9 pour cas 1
- [ ] **5.5** Valider Cost≥6 pour cas 2
- [ ] **5.6** Créer `benchmark_performance/0` (temps < 1s pour cas 1)
- [ ] **5.7** Tests de robustesse et gestion d'erreurs

### Dev 2 - Validation algorithmique

- [ ] **5.8** Vérifier l'optimalité des solutions (A* complet)
- [ ] **5.9** Valider les heuristiques (admissibilité)
- [ ] **5.10** Tester les cas complexes et limites
- [ ] **5.11** Profiling de performance

### Dev 1 - Tests d'intégration système

- [ ] **5.12** Tests de l'interface utilisateur complète
- [ ] **5.13** Validation des cas d'usage CU-001 à CU-006
- [ ] **5.14** Tests de gestion d'erreurs
- [ ] **5.15** Tests de navigation dans les menus

### Dev 3 - Validation logique métier

- [ ] **5.16** Tests de tous les mouvements possibles
- [ ] **5.17** Validation des états impossibles
- [ ] **5.18** Tests de cohérence des transformations

---

## 📚 PHASE 6 : Documentation & Polish (1-2 jours)

### Dev 1 - Documentation utilisateur

- [ ] **6.1** Créer le guide d'utilisation complet
- [ ] **6.2** Documenter les commandes de lancement
- [ ] **6.3** Rédiger la documentation de démonstration
- [ ] **6.4** Finaliser le README avec instructions claires

### Dev 4 - Documentation technique

- [ ] **6.5** Commenter tout le code en français
- [ ] **6.6** Documenter l'architecture des modules
- [ ] **6.7** Créer la documentation des tests
- [ ] **6.8** Finaliser le rapport technique

### Dev 2 - Documentation algorithmique

- [ ] **6.9** Documenter l'algorithme A* implémenté
- [ ] **6.10** Expliquer les heuristiques utilisées
- [ ] **6.11** Documenter les optimisations
- [ ] **6.12** Créer les schémas explicatifs

### Dev 3 - Documentation du domaine

- [ ] **6.13** Documenter la représentation des états
- [ ] **6.14** Expliquer la logique des mouvements
- [ ] **6.15** Documenter les cas de test
- [ ] **6.16** Polish final et cohérence

---

## 🎯 Jalons critiques

| Date | Jalon | Validation |
|------|-------|------------|
| **J+5** | ✅ **Architecture complète** | Tous les modules compilent et s'intègrent |
| **J+9** | ✅ **Algorithme A* fonctionnel** | Cost=4, Expanded=9 pour cas 1 |
| **J+12** | ✅ **Logique jeu complète** | Tous mouvements et validations OK |
| **J+16** | ✅ **Interface CLI avancée** | Tous les mockups implémentés |
| **J+19** | ✅ **Tests & validation** | Tous tests passent, performance OK |
| **J+21** | 🚀 **Livraison finale** | Documentation complète, démonstration prête |

---

## ⚡ Priorités par ordre d'importance

1. **🔴 CRITIQUE:** Algorithme A* avec résultats exacts (Cost=4, Expanded=9)
2. **🟠 HAUTE:** Interface CLI selon mockups pour impressionner
3. **🟡 MOYENNE:** Tests complets et robustesse
4. **🟢 BASSE:** Documentation et optimisations finales

---

## 📋 Utilisation de cette TODO list

- **Chaque développeur** prend les tâches de son module principal
- **Cocher** les tâches accomplies au fur et à mesure
- **Signaler** les blocages et dépendances entre modules
- **Tester** après chaque phase critique
- **Valider** ensemble les résultats critiques

> **🎯 Objectif:** Livre le 20 octobre un solveur A* impressionnant avec interface CLI sophistiquée qui produit exactement Cost=4, Expanded=9 pour le cas professeur !