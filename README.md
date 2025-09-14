<div align="center">

# 🧩 Solveur de Taquin A*

**Solveur intelligent de puzzle 3×3 avec algorithme de recherche heuristique A***

*Projet IFT-2003 · Intelligence Artificielle 1 · Université Laval*

[![SWI-Prolog](https://img.shields.io/badge/SWI--Prolog-9.x+-blue?style=flat-square)](https://www.swi-prolog.org/)
[![Platforms](https://img.shields.io/badge/Platform-Windows%20%7C%20Linux%20%7C%20macOS-lightgrey?style=flat-square)]()
[![AI Algorithm](https://img.shields.io/badge/AI-A*%20Search%20%7C%20Misplaced%20Tiles-green?style=flat-square)]()


</div>

---

<div align="center">
<img src="archive/ecran_accueil_fr.png" alt="Écran d'accueil Solveur Taquin" width="450">
</div>

## 🚀 Installation & Lancement

### Lancement Simple (Recommandé)
```bash
# Une seule commande pour tous les systèmes :
swipl -g start run.pl
```

### Autres Commandes
```bash
# Lancement manuel (si problème avec le launcher)
swipl -g main src/main.pl

# Exécuter la suite de tests
swipl -g run_all_tests src/tests.pl
```

### Configuration Automatique
Le fichier `run.pl` configure automatiquement :
- ✅ Encodage UTF-8 pour l'affichage des caractères spéciaux
- ✅ Compatibilité Windows, macOS et Linux
- ✅ Aucune configuration manuelle requise

## 🏗️ Architecture

<table>
<tr><td><strong>Module</strong></td><td><strong>Responsabilité</strong></td></tr>
<tr><td><code>main.pl</code></td><td>🖥️ Interface CLI, orchestration, cas de test</td></tr>
<tr><td><code>game.pl</code></td><td>🎮 États du puzzle, mouvements, validation</td></tr>
<tr><td><code>astar.pl</code></td><td>🧠 Algorithme A*, heuristiques, recherche optimale</td></tr>
<tr><td><code>display.pl</code></td><td>🎨 Affichage formaté, interface utilisateur</td></tr>
</table>


## ✨ Fonctionnalités

<table>
<tr>
<td width="33%" align="center">
  <img src="archive/menu_principal.png" alt="Menu principal" width="280">
  <br><em>Menu principal interactif</em>
</td>
<td width="33%" align="center">
  <img src="archive/resultats.png" alt="Résultats de résolution" width="280">
  <br><em>Résultats validation académique</em>
</td>
<td width="33%" align="center">
  <img src="archive/animation_resultats.png" alt="Validation tests" width="240">
  <br><em>Validation automatisée</em>
</td>
</tr>
</table>

### 🏆 Résolution Optimale de Taquin
- ✅ **Algorithme A*** avec closed set pour solutions déterministes
- ✅ **Validation académique** exacte : Cost=4, Expanded=9, Path=5 États
- 🎯 **Solution "9 nœuds"** : Comptage selon image ExempleResolution.png du professeur

### 🤖 Intelligence Artificielle

| Composante | Description | Performance |
|------------|-------------|-------------|
| **Algorithme** | A* avec closed set¹ | Optimal garanti |
| **Heuristique** | Tuiles mal placées² (excluant case vide) | Admissible + consistante |
| **État-espace** | 9!/2 = 181 440 configurations solvables | Résolution < 1 seconde |
| **Validation** | Métriques exactes cas professeur | 100% conformité académique |

**Références techniques :**
- ¹ A* Search : [Russell & Norvig AI](https://aima.cs.berkeley.edu/)
- ² Misplaced Tiles : [Heuristics for 8-puzzle](https://algorithmsinsight.wordpress.com/)
- 🤖 **Développé avec** [Claude Code](https://claude.ai/code)

## 🎮 Usage

### Format d'entrée
```
États : [1,2,3,5,0,6,4,7,8] (case vide = 0)
Menu  : [1] Cas test classique, [2] Cas avancé, [3] A propos, [4] Quitter
Sortie: Path A→B→C→D→E, Cost: 4, Expanded: 9
```

### Modes disponibles
- 🎯 **Cas classique** · Configuration académique standard (4 mouvements)
- 🚀 **Cas avancé** · Configuration complexe pour démonstration étendue

## 🧪 Tests

*Suite de tests automatisés : 15/15 tests passés, validation académique confirmée*

**Couverture complète :** Tests répartis sur 5 modules (game, astar, display, intégration, validation académique)

```bash
# Suite complète
swipl -g run_all_tests src/tests.pl

# Tests par module disponibles
```

## 📋 Prérequis Système

| Composant | Version | Notes |
|-----------|---------|-------|
| **SWI-Prolog** | 9.x+ | Moteur Prolog principal |
| **OS** | Windows/Linux/macOS | Multiplateforme |
| **Mémoire** | 256 MB+ | Algorithme A* avec closed set |

## 📚 Documentation Technique

| Document | Description |
|----------|-------------|
| 📐 [**Spécifications Techniques**](docs/specifications_techniques.md) | Architecture système complète |
| 🏗️ [**Architecture Guide**](docs/architecture.md) | Design technique détaillé |
| 📄 [**Product Requirements**](docs/prd.md) | Spécifications détaillées |
| 📋 [**Énoncé Original**](archive/TP1_Enonce_Reformule.md) | Exigences académiques officielles |
| 📝 [**Rapport de Remise**](docs/rapport_tp1_template.md) | Template rapport final |

---

<div align="center">

*🎓 Projet Universitaire · 🏛️ Université Laval · 🤖 IFT-2003 Intelligence Artificielle 1*

</div>