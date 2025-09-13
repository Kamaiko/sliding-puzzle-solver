# Répartition des tâches - Équipe de 4 personnes
**Projet :** Solveur de Taquin 3x3 avec A*  
**Durée :** 2 semaines (25-30h total)  
**Architecture :** 4 modules indépendants + tests
**Objectif :** Travail parallèle optimal avec responsabilités équilibrées

---

## 🎯 **Principe de répartition optimisée**

### **Objectifs prioritaires**
- ✅ **Indépendance maximale** : Chacun peut progresser sans attendre les autres
- ✅ **Responsabilités équilibrées** : Charge de travail ~7-8h par personne
- ✅ **Objectifs précis et mesurables** pour chaque membre
- ✅ **Gestion repo centralisée** par un responsable désigné

### **Jalons de synchronisation**
- **Jour 5** : Modules individuels fonctionnels (tests unitaires)
- **Jour 10** : Intégration et validation croisée
- **Jour 14** : Livraison finale coordonnée

---

## 👥 **Répartition des responsabilités finales**

### **👨‍💻 DEV 1 - Responsable Algorithme A* + Leadership**
**Module principal :** `astar.pl` (~150 lignes)  
**Charge :** 7-8 heures  
**🎯 Objectif précis :** Implémenter l'algorithme A* qui produit EXACTEMENT 9 nœuds explorés et 4 mouvements pour le cas test professeur

#### **Responsabilités autonomes :**
- **astar.pl complet** :
  - Structure `node(State, F, G, Parent)`
  - File de priorité avec f(n) = g(n) + h(n)
  - Boucle A* avec gestion états visités
  - Reconstruction du chemin par backtracking
  - Interface `solve_puzzle(case1, result(Path, Cost, Expanded))`
- **Heuristique intégrée** : Tuiles mal placées (excluant case vide)
- **Tests A* spécifiques** : Validation algorithme isolé
- **Coordination technique** : Point de contact pour questions algorithme

#### **Livrables mesurables :**
- ✅ `astar.pl` fonctionnel avec interface définie
- ✅ Cas test 1 : **Expanded = 9, Cost = 4** (validation exacte)
- ✅ Tests unitaires A* (file priorité, heuristique, reconstruction)

---

### **🎮 DEV 2 - Responsable Logique de Jeu + Repo**
**Module principal :** `game.pl` (~100 lignes)  
**Charge :** 7-8 heures  
**🎯 Objectif précis :** Créer toute la mécanique du taquin et gérer le repository Git

#### **Responsabilités autonomes :**
- **game.pl complet** :
  - États : `initial_state/1`, `goal_state/1`, `custom_initial_state/1`
  - Validation : `valid_state/1`, `find_blank/2`
  - Mouvements : `generate_moves/2`, `apply_move/3`, `valid_move/2`
  - Utilitaires : Conversions positions/coordonnées
- **Gestion repository Git** :
  - Commits réguliers et messages clairs
  - Gestion des branches si nécessaire
  - Merge des contributions équipe
  - Documentation des changements
- **Tests jeu spécifiques** : Validation mécanique isolée

#### **Livrables mesurables :**
- ✅ `game.pl` avec tous les prédicats fonctionnels
- ✅ 4 mouvements générés depuis centre, 2-3 depuis coins/bords
- ✅ Repository Git propre avec historique clair
- ✅ Tests unitaires game (états, mouvements, validation)

---

### **🖥️ DEV 3 - Responsable Interface + Orchestration**  
**Module principal :** `main.pl` (~60 lignes)  
**Charge :** 7-8 heures  
**🎯 Objectif précis :** Créer l'interface utilisateur complète et orchestrer les modules

#### **Responsabilités autonomes :**
- **main.pl complet** :
  - Menu CLI interactif avec 3 options
  - Point d'entrée `:- initialization(main, main)`
  - Gestion des cas de test (case1, case2)
  - Mesure des temps de réponse IA
  - Gestion d'erreurs et validation utilisateur
- **Intégration modules** : Orchestration `game` + `astar` + `display`
- **Guide d'utilisation** : Documentation utilisateur
- **Tests interface** : Validation menu et workflow

#### **Livrables mesurables :**
- ✅ Interface CLI fonctionnelle (menu + navigation)
- ✅ Intégration réussie des 4 modules
- ✅ Guide d'utilisation complet
- ✅ Tests interface (choix menu, gestion erreurs)

---

### **📱 DEV 4 - Responsable Affichage + Tests + Documentation**
**Modules principaux :** `display.pl` (~50 lignes) + `tests.pl` (~80 lignes)  
**Charge :** 7-8 heures  
**🎯 Objectif précis :** Assurer la qualité visuelle, les tests complets et la documentation finale

#### **Responsabilités autonomes :**
- **display.pl complet** :
  - Affichage plateau 3x3 avec bordures ASCII
  - Formatage Path/Cost/Expanded/Temps
  - Bannière et messages utilisateur
  - Affichage chemin solution étape par étape
- **tests.pl complet** :
  - Tests unitaires pour chaque module
  - Validation cas test 1 et 2
  - Tests de robustesse (états impossibles)
  - Benchmarks de performance
- **Documentation projet** :
  - Mise à jour Architecture.md
  - Documentation prédicats importants
  - Rapport final (si requis)

#### **Livrables mesurables :**
- ✅ Affichage plateau 3x3 professionnel
- ✅ Suite tests 100% modules (game, astar, main, display)
- ✅ Documentation technique complète
- ✅ Validation croisée : tous les tests passent

---

## 📅 **Timeline de travail parallèle**

### **Semaine 1 : Développement indépendant**
| Jour | Dev 1 (A*) | Dev 2 (Game + Git) | Dev 3 (Interface) | Dev 4 (Display + Tests) |
|------|------------|---------------------|-------------------|-------------------------|
| **Lun-Mar** | Structure node + heuristique | États + validation | Menu CLI base | Affichage plateau |
| **Mer-Jeu** | File priorité A* | Mouvements + tests | Orchestration modules | Tests unitaires |
| **Ven** | Tests A* isolé | Git setup + commits | Intégration test | Documentation |

### **Semaine 2 : Intégration et validation**
| Jour | Dev 1 (A*) | Dev 2 (Game + Git) | Dev 3 (Interface) | Dev 4 (Display + Tests) |
|------|------------|---------------------|-------------------|-------------------------|
| **Lun** | Optimisation A* | Support intégration | Tests interface | Validation croisée |
| **Mar** | Validation cas prof | Merge contributions | Guide utilisateur | Tests robustesse |
| **Mer-Jeu** | Tests finaux | Repository final | Polish interface | Documentation finale |

---

## 🔄 **Coordination minimale requise**

### **Interfaces définies (indépendantes)**
```prolog
% game.pl → astar.pl
initial_state(State), goal_state(Goal), generate_moves(State, Successors)

% astar.pl → main.pl  
solve_puzzle(CaseNumber, result(Path, Cost, Expanded))

% main.pl → display.pl
display_solution(Path, Cost, Expanded, ResponseTime)
```

### **Points de synchronisation (15 min max)**
- **Jour 3** : Validation interfaces entre modules
- **Jour 7** : Premier test d'intégration
- **Jour 12** : Validation finale coordonnée

---

## 📊 **Critères de succès individuels**

### **Dev 1 - A* (Critique)**
- ✅ Cas professeur : **Cost=4, Expanded=9** (EXACT)
- ✅ Temps résolution < 1 seconde
- ✅ Interface `solve_puzzle` fonctionnelle

### **Dev 2 - Game + Git (Fondations)**
- ✅ Tous les mouvements valides générés correctement
- ✅ Repository propre avec commits réguliers
- ✅ Tests unitaires game 100%

### **Dev 3 - Interface (Expérience)**
- ✅ Menu CLI intuitif sans bugs
- ✅ Intégration sans erreurs modules
- ✅ Guide utilisateur complet

### **Dev 4 - Qualité (Validation)**
- ✅ Affichage professionnel plateau 3x3
- ✅ Suite tests complète (tous modules)
- ✅ Documentation technique à jour

---

## ⚡ **Avantages de cette répartition**

### **🚀 Travail parallèle optimal**
- Chacun peut commencer immédiatement
- Aucune dépendance bloquante entre membres
- Interfaces bien définies pour intégration

### **⚖️ Charge équilibrée**
- **Dev 1** : Module critique mais bien défini (A*)
- **Dev 2** : Logic métier + responsabilité Git
- **Dev 3** : Interface + coordination technique  
- **Dev 4** : Qualité + tests + documentation

### **🎯 Objectifs mesurables**
- Critères de succès précis pour chacun
- Validation indépendante possible
- Responsabilités claires et non-overlappantes

---

**🚀 Cette répartition garantit que chaque membre peut progresser de façon autonome tout en contribuant de manière équitable au succès du projet !**