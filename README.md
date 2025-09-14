# 🧩 Solveur de Taquin A*

**Solveur intelligent 3×3 avec recherche heuristique**  
*Projet IFT-2003 Intelligence Artificielle*

[![SWI-Prolog](https://img.shields.io/badge/SWI--Prolog-9.x+-blue?style=flat-square)](https://www.swi-prolog.org/)
[![AI Algorithm](https://img.shields.io/badge/AI-A*%20Search-green?style=flat-square)]()

---

## 🚀 Démarrage rapide

```bash
# Lancer le solveur
swipl src/main.pl

# Exécuter les tests
swipl src/tests.pl
?- run_all_tests.
```

## 🎯 Fonctionnalités

- ✅ **Algorithme A*** avec heuristique tuiles mal placées
- ✅ **Résultats exacts** : Cost=4, Expanded=9 (cas professeur)
- ✅ **Interface CLI** avec menu interactif
- ✅ **2 cas de test** : professeur + personnalisé
- ✅ **Performance** < 1 seconde pour 3x3

## 🔧 Prérequis

- **SWI-Prolog** 9.x+
- **OS** : Windows/Linux/macOS
- **Mémoire** : 256 MB+

## 📚 Documentation

- [🎯 PRD](docs/prd.md) - Vision et exigences produit
- [🏗️ Architecture](docs/architecture.md) - Design technique
- [🚀 Développement](docs/development.md) - Guide équipe
- [🎨 Mockups UI](docs/ui_mockups.md) - Design interface CLI
- [📋 Plan de développement](docs/todo_list.md) - Todo list détaillée et jalons
- [📄 Rapport](docs/report_template.md) - Template rendu final