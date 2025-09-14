# Plan de Développement - Solveur Taquin A*

## 🎯 Vue d'ensemble
Architecture modulaire 4 modules Prolog avec validation exacte Cost=4, Expanded=9 pour cas test académique.

## 📋 Checklist par module

### Module game.pl
- [ ] Représentation états : liste [1,2,3,5,0,6,4,7,8]
- [ ] Génération mouvements : ordre HAUT, BAS, GAUCHE, DROITE
- [ ] Validation états : configurations valides vs impossibles
- [ ] Utilitaires : find_blank/2, apply_move/3

### Module astar.pl
- [ ] Structure nœud : node(State, G, H, F, Parent)
- [ ] Open list + closed set (OBLIGATOIRE)
- [ ] Heuristique tuiles mal placées (excluant case vide)
- [ ] Reconstruction chemin : Path A→B→C→D→E
- [ ] Comptage exact : 9 nœuds explorés (SANS état initial)

### Module main.pl
- [ ] Menu principal : cas test 1, cas test 2, quitter
- [ ] Orchestration : game.pl ↔ astar.pl ↔ display.pl
- [ ] Mesure temps d'exécution
- [ ] Gestion erreurs

### Module display.pl
- [ ] Affichage grille 3x3 : case vide = #
- [ ] Format résultats : Path/Cost/Expanded
- [ ] Messages français
- [ ] Interface propre

### Module tests.pl
- [ ] Test heuristique : h([1,2,3,5,0,6,4,7,8]) = 4
- [ ] Test cas test 1 : Cost=4, Expanded=9 exact
- [ ] Tests unitaires par module
- [ ] run_all_tests/0

## 🚨 Validations critiques

### Cas test 1 (OBLIGATOIRE)
```
Initial: [1,2,3,5,0,6,4,7,8]
Final:   [1,2,3,4,5,6,7,8,0]
```
**Résultats requis** :
- [ ] Cost = 4 mouvements
- [ ] Expanded = 9 nœuds (sans état initial)
- [ ] Path = 5 états

### Cas test 2 (personnalisé)
- [ ] Minimum 6 mouvements
- [ ] Configuration solvable
- [ ] Résultats cohérents

## ⚡ Commandes rapides

```bash
# Test principal
swipl src/main.pl

# Tests unitaires
swipl src/tests.pl
?- run_all_tests.

# Test cas critique
?- test_case_1_exact.
```

## 📋 Timeline

### Phase 1 : Fondations (Jour 1-3)
- [ ] game.pl : états et mouvements
- [ ] astar.pl : structure de base
- [ ] tests.pl : premiers tests

### Phase 2 : Algorithme (Jour 4-6)
- [ ] A* complet avec closed set
- [ ] Heuristique et validation
- [ ] Cas test 1 fonctionnel

### Phase 3 : Interface (Jour 7-9)
- [ ] main.pl : menu CLI
- [ ] display.pl : affichage
- [ ] Integration complète

### Phase 4 : Validation (Jour 10-12)
- [ ] Tests complets
- [ ] Performance < 1s
- [ ] Documentation finale

## ⚠️ Points d'attention

1. **A* avec closed set** : OBLIGATOIRE pour 9 nœuds exacts
2. **Comptage nœuds** : État initial NON compté
3. **Ordre mouvements** : HAUT, BAS, GAUCHE, DROITE
4. **Heuristique** : Ignorer case vide (position 0)
5. **Format** : snake_case pour fichiers techniques

Voir `specifications_techniques.md` pour détails algorithmiques complets.