<div align="center">

# 🧩 Sliding Puzzle Solver

**Solveur intelligent de Taquin 3×3 avec algorithme A* et recherche heuristique**

*Projet IFT-2003 · Intelligence Artificielle · Université Laval*

[![SWI-Prolog](https://img.shields.io/badge/SWI--Prolog-9.x+-blue?style=flat-square)](https://www.swi-prolog.org/)
[![Platforms](https://img.shields.io/badge/Platform-Windows%20%7C%20Linux%20%7C%20macOS-lightgrey?style=flat-square)]()
[![AI Algorithm](https://img.shields.io/badge/AI-A*%20%7C%20Heuristic%20Search-green?style=flat-square)]()

<table>
<tr>
<td width="50%" align="center">
  <!-- Placeholder: Screenshot du menu principal -->
  <img src="docs/images/menu-principal.png" alt="Menu principal du solveur" width="350">
  <br><em>Menu interactif avec cas de test</em>
</td>
<td width="50%" align="center">
  <!-- Placeholder: Screenshot de l'algorithme A* en action -->
  <img src="docs/images/astar-solving.png" alt="A* résolvant le taquin" width="400">
  <br><em>Algorithme A* avec affichage Path/Cost/Expanded</em>
</td>
</tr>
</table>

</div>

---

## 🚀 Installation & Lancement

```bash
# Démarrer le solveur de taquin
swipl -g main src/main.pl
```

## 🏗️ Architecture

<table>
<tr><td><strong>Module</strong></td><td><strong>Responsabilité</strong></td></tr>
<tr><td><code>main.pl</code></td><td>🖥️ Interface CLI, orchestration, cas de test</td></tr>
<tr><td><code>game.pl</code></td><td>🎯 États du taquin, mouvements valides, validation</td></tr>
<tr><td><code>astar.pl</code></td><td>🧠 Algorithme A* complet, heuristiques, recherche optimale</td></tr>
<tr><td><code>display.pl</code></td><td>📱 Affichage formaté 3×3, statistiques, interface utilisateur</td></tr>
<tr><td><code>tests.pl</code></td><td>🧪 Tests unitaires, validation, benchmarks de performance</td></tr>
</table>

## ✨ Fonctionnalités

### 🎯 Résolution Optimale du Taquin
- ✅ **États 3×3 complets** avec validation automatique
- ✅ **Recherche optimale** garantie par l'algorithme A*

### 🤖 Intelligence Artificielle

| Composante | Description | Performance |
|------------|-------------|-------------|
| **Algorithme** | A*¹ avec file de priorité f(n) = g(n) + h(n) | Solution optimale |
| **Heuristiques** | Tuiles mal placées² + Distance Manhattan³ | Guidage efficace |
| **Exploration** | Gestion états visités + reconstruction chemin | Évite cycles |
| **Validation** | Cas professeur : 4 mouvements, 9 nœuds explorés | Résultats exacts |

**Références techniques :**
- ¹ A* : [Artificial Intelligence: A Modern Approach](https://aima.cs.berkeley.edu/) • [AI Course Notes](https://www.cs.cmu.edu/~15381/)
- ² Misplaced Tiles : [Heuristic Search Algorithms](https://en.wikipedia.org/wiki/Admissible_heuristic)
- ³ Manhattan Distance : [Sliding Puzzle Heuristics](https://heuristicswiki.wikispaces.com/)
- 🤖 **Développé avec** [Claude Code](https://claude.ai/code)

## 🎮 Usage

### Format d'utilisation
```
Menu principal : 
1. Cas de test 1 (Exemple professeur)
2. Cas de test 2 (Exemple personnalisé)  
3. Quitter

Résultats : Path/Cost/Expanded + temps IA
```

### Cas de test inclus
- 📚 **Cas professeur** · Validation exacte (4 mouvements, 9 nœuds)
- 🎲 **Cas personnalisé** · Configuration plus complexe (6+ mouvements)

## 🧪 Tests

<!-- Placeholder: Screenshot des tests en cours -->
<img src="docs/images/test-performance.png" alt="Suite de tests unitaires" width="600">

*Performance validée : < 1 seconde pour résolution taquin 3×3*

**Suite de tests complète :** Tests modulaires pour game.pl, astar.pl, et validation d'intégration avec résultats exacts

```bash
# Suite complète de tests
swipl -g run_all_tests src/tests.pl

# Tests spécifiques par modules disponibles
```

## 📋 Prérequis Système

| Composant | Version | Notes |
|-----------|---------|-------|
| **SWI-Prolog** | 9.x+ | Moteur Prolog principal |
| **OS** | Windows/Linux/macOS | Multiplateforme |
| **Mémoire** | 256 MB+ | Algorithme A* avec états visités |

## 🔧 Améliorations Futures

### **🚀 Optimisations Algorithmiques Avancées**

| Amélioration | Description | Bénéfice Attendu |
|-------------|-------------|------------------|
| **🗂️ IDA* (Iterative Deepening)** | Recherche en profondeur itérative limitée | Moins d'utilisation mémoire |
| **🔄 Recherche Bidirectionnelle** | Exploration simultanée début/fin | Performance 2x supérieure |
| **🎯 Pattern Database** | Heuristiques pré-calculées sophistiquées | Guidage optimal pour puzzles complexes |

### **🎮 Extensions du Domaine**
- **Tailles variables** (4×4, 5×5) avec adaptation automatique
- **Générateur de puzzles** avec niveaux de difficulté
- **Mode interactif** permettant à l'utilisateur de jouer manuellement
- **Analyse de solvabilité** avec détection d'états impossibles

### **📊 Interface et Visualisation**
- **Visualisation graphique** des étapes de résolution
- **Animation du chemin solution** étape par étape
- **Statistiques détaillées** (facteur de branchement, temps par nœud)
- **Export des résultats** en format CSV/JSON

### **🔍 Recherche Heuristique Avancée**
- **Comparaison d'heuristiques** (admissibilité, consistance)
- **Métriques d'évaluation** (facteur de branchement effectif)
- **Optimisation automatique** des paramètres

## 📚 Documentation Technique

| Document | Description |
|----------|-------------|
| 📐 [**Architecture Guide**](docs/Architecture.md) | Architecture modulaire complète |
| 👥 [**Team Distribution**](docs/Repartition_Equipe.md) | Répartition des tâches équipe |
| 📋 [**Development Plan**](docs/Plan_Developpement.md) | Planification détaillée du développement |

---

<div align="center">

**🎓 Projet Universitaire** · **🏛️ Université Laval** · **🤖 Intelligence Artificielle**

*A* • Heuristic Search • Sliding Puzzle • Optimal Pathfinding*

</div>