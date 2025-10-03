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
<img src="docs/images/menu_principal.png" alt="Menu principal Solveur Taquin" width="450">
</div>

## Installation & Lancement

### Prérequis
- **SWI-Prolog 9.x+** 

| OS | Installation |
|-----|-------------|
| **Windows** | [Télécharger SWI-Prolog](https://www.swi-prolog.org/download/stable) puis installer le .exe<br>`winget install SWI-Prolog.SWI-Prolog` (avec winget) |
| **macOS** | `brew install swi-prolog` |
| **Linux** | `sudo apt install swi-prolog` (Ubuntu/Debian)<br>`sudo dnf install pl` (Fedora) |

### Lancement
```bash
swipl run.pl
```

### Tests
```bash
# Exécuter la suite de tests
swipl -g run_all_tests src/tests.pl
```


## Architecture

<table>
<tr><td><strong>Module</strong></td><td><strong>Responsabilité</strong></td></tr>
<tr><td><code>main.pl</code></td><td>🖥️ Interface CLI, orchestration, cas de test</td></tr>
<tr><td><code>game.pl</code></td><td>🎮 États du puzzle, mouvements, validation</td></tr>
<tr><td><code>astar.pl</code></td><td>🧠 Algorithme A*, heuristiques, recherche optimale</td></tr>
<tr><td><code>display.pl</code></td><td>🎨 Affichage formaté UTF-8, interface utilisateur</td></tr>
</table>


## Intelligence Artificielle

| Composante | Description | Performance |
|------------|-------------|-------------|
| **Algorithme** | A* avec closed set | Optimal garanti |
| **Heuristique** | Tuiles mal placées | Admissible + consistante |
| **État-espace** | 9!/2 = 181 440 configurations solvables | Résolution < 1 seconde |
| **Validation** | Métriques exactes cas professeur | Conformité complète |

### Exemple de résolution A*

<div align="center">
<img src="docs/images/CasTest1.png" alt="Cas Test 1 - Résolution A*" width="400">
<br><em>Démonstration du cas test 1 avec affichage du chemin solution complet et des métriques de performance.</em>
</div>


## Usage

### Format d'entrée
```
États : [1,2,3,5,0,6,4,7,8] (case vide = 0)
Menu  : [1] Cas test classique, [2] Cas avancé, [3] A propos, [4] Quitter
Sortie: Path A→B→C→D→E, Cost: 4, Expanded: 12
```

### Modes disponibles
- **Cas classique** · Configuration standard (4 mouvements)
- **Cas avancé** · Configuration complexe pour démonstration étendue



## Documentation Technique

| Document | Description |
|----------|-------------|
| [**Architecture Guide**](docs/architecture.md) | Guide technique détaillé pour l'équipe |
| [**Product Requirements**](docs/prd.md) | Spécifications et exigences du projet |
| [**Rapport de Remise**](docs/rapport_tp1.md) | Rapport final de TP1 |
| [**Énoncé Original**](archive/TP1_Aut_2025%20(1).pdf) | Exigences officielles du projet |

---

<div align="center">

*Projet Universitaire · Université Laval · IFT-2003 Intelligence Artificielle 1*

</div>
