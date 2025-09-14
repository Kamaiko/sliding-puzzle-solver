# Cas de Test - Validation Exacte A*

## 🎯 Cas Test 1 - Référence Professeur (CRITIQUE)

### Configurations
```
État initial (A):        État final:
+---+---+---+           +---+---+---+
| 1 | 2 | 3 |           | 1 | 2 | 3 |
+---+---+---+           +---+---+---+
| 5 | # | 6 |           | 4 | 5 | 6 |
+---+---+---+           +---+---+---+
| 4 | 7 | 8 |           | 7 | 8 | # |
+---+---+---+           +---+---+---+

Représentation interne:
Initial: [1,2,3,5,0,6,4,7,8]
Final:   [1,2,3,4,5,6,7,8,0]
```

### Traçage Exact A* - 9 Nœuds Explorés

#### Étape par étape (simulation)

**Initialisation:**
- Open list: [node([1,2,3,5,0,6,4,7,8], g=0, h=4, f=4, parent=nil)]
- Closed set: {}
- Explored count: 0

**Itération 1:**
- Extraire: [1,2,3,5,0,6,4,7,8] (g=0, h=4, f=4)
- Ajouter au closed set
- **Explored count: 1**
- Successeurs (blank en position 4):
  - BAS: [1,2,3,5,7,6,4,0,8] (g=1, h=4, f=5)
  - GAUCHE: [1,2,3,0,5,6,4,7,8] (g=1, h=4, f=5)
  - DROITE: [1,2,3,5,6,0,4,7,8] (g=1, h=4, f=5)

**Itération 2:**
- Extraire: [1,2,3,0,5,6,4,7,8] (g=1, h=4, f=5) [premier par ordre GAUCHE]
- Ajouter au closed set
- **Explored count: 2**
- Successeurs: ...

**Itération 3:**
- Extraire: [1,2,3,5,6,0,4,7,8] (g=1, h=4, f=5)
- Ajouter au closed set
- **Explored count: 3**
- Successeurs: ...

[Continue jusqu'à...]

**Itération 9:**
- Extraire: État qui mène vers la solution
- Ajouter au closed set
- **Explored count: 9**

**Solution trouvée:**
- Path: 5 états A→B→C→D→E
- Cost: 4 mouvements
- Expanded: 9 nœuds

### Validation Heuristique

#### Test h([1,2,3,5,0,6,4,7,8])
```
Position | Valeur | Attendu | Mal placée?
---------|--------|---------|------------
   0     |   1    |    1    |     ✓
   1     |   2    |    2    |     ✓
   2     |   3    |    3    |     ✓
   3     |   5    |    4    |     ✗ (mal placée)
   4     |   0    |    5    |   IGNORÉ (case vide)
   5     |   6    |    6    |     ✓
   6     |   4    |    7    |     ✗ (mal placée)
   7     |   7    |    8    |     ✗ (mal placée)
   8     |   8    |    0    |     ✗ (mal placée)

Total: 4 tuiles mal placées → h = 4
```

#### Test h([1,2,3,4,5,6,7,8,0]) = 0
```
État final: toutes les tuiles sont en position correcte
h(goal) = 0 ✓
```

### Tests Unitaires

```prolog
% Test heuristique critique
test_heuristic_initial :-
    Initial = [1,2,3,5,0,6,4,7,8],
    Goal = [1,2,3,4,5,6,7,8,0],
    misplaced_tiles(Initial, Goal, H),
    assertion(H =:= 4),
    write('✓ Heuristique état initial = 4').

test_heuristic_goal :-
    Goal = [1,2,3,4,5,6,7,8,0],
    misplaced_tiles(Goal, Goal, H),
    assertion(H =:= 0),
    write('✓ Heuristique état final = 0').

% Test cas complet
test_case_1_complete :-
    Initial = [1,2,3,5,0,6,4,7,8],
    Goal = [1,2,3,4,5,6,7,8,0],
    astar_search(Initial, Goal, Path, Cost, Expanded),
    length(Path, PathLength),
    assertion(PathLength =:= 5),
    assertion(Cost =:= 4),
    assertion(Expanded =:= 9),
    write('✓ Cas test 1: Path=5, Cost=4, Expanded=9').

% Suite de tests complète
run_critical_tests :-
    test_heuristic_initial,
    test_heuristic_goal,
    test_case_1_complete,
    write('✅ Tous les tests critiques passés').
```

## 🎯 Cas Test 2 - Configuration Personnalisée

### Proposition
```
État initial:            État final:
+---+---+---+           +---+---+---+
| 2 | 8 | 3 |           | 1 | 2 | 3 |
+---+---+---+           +---+---+---+
| 1 | 6 | 4 |           | 8 | # | 4 |
+---+---+---+           +---+---+---+
| 7 | # | 5 |           | 7 | 6 | 5 |
+---+---+---+           +---+---+---+

Interne: [2,8,3,1,6,4,7,0,5] → [1,2,3,8,0,4,7,6,5]
```

### Critères validation
- [ ] Minimum 6 mouvements requis
- [ ] Configuration solvable (vérification parité)
- [ ] Résultats A* cohérents et reproductibles

### Test personnalisé
```prolog
test_case_2_custom :-
    Initial = [2,8,3,1,6,4,7,0,5],
    Goal = [1,2,3,8,0,4,7,6,5],
    astar_search(Initial, Goal, Path, Cost, Expanded),
    assertion(Cost >= 6),  % Minimum 6 mouvements
    length(Path, PathLength),
    assertion(PathLength =:= Cost + 1),  % Path = mouvements + 1
    write('✓ Cas test 2: minimum 6 mouvements validé').
```

## 🚨 Points de Validation

### Checklist obligatoire
- [ ] **h([1,2,3,5,0,6,4,7,8]) = 4** exactement
- [ ] **Cas test 1: Cost=4, Expanded=9** exactement
- [ ] **Path contient 5 états** (A→B→C→D→E)
- [ ] **Déterminisme**: résultats identiques à chaque exécution
- [ ] **Performance**: résolution < 1 seconde

### Commandes de test
```bash
# Test critique unique
?- test_case_1_complete.

# Batterie complète
?- run_critical_tests.

# Validation heuristique
?- test_heuristic_initial.
```

Ces tests garantissent la conformité exacte aux exigences académiques.