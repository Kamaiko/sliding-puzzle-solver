# Plan détaillé de l'algorithme A* - Validation exacte du TP1

## 🎯 **Objectif de validation**

Implémenter un algorithme A* qui produit **EXACTEMENT** les résultats suivants pour le cas professeur :
- **État initial** : `[1,2,3,5,0,6,4,7,8]` (format liste)
- **État final** : `[1,2,3,4,5,6,7,8,0]` (format liste)
- **Path** : 5 états (A→B→C→D→E)
- **Cost** : 4 mouvements précisément
- **Expanded** : 9 nœuds explorés (excluant l'état initial)

---

## 🧮 **Analyse de l'heuristique (Tuiles mal placées)**

### **État initial** : `[1,2,3,5,0,6,4,7,8]`
```
1 2 3
5 * 6  
4 7 8
```

### **État but** : `[1,2,3,4,5,6,7,8,0]`
```
1 2 3
4 5 6
7 8 *
```

### **Calcul heuristique h(n) - Tuiles mal placées**
Position | Initial | But | Mal placée ?
---------|---------|-----|-------------
0        | 1       | 1   | ❌ (correct)
1        | 2       | 2   | ❌ (correct)  
2        | 3       | 3   | ❌ (correct)
3        | 5       | 4   | ✅ (mal placée)
4        | 0       | 5   | ➖ (case vide - ignorée)
5        | 6       | 6   | ❌ (correct)
6        | 4       | 7   | ✅ (mal placée)
7        | 7       | 8   | ✅ (mal placée)  
8        | 8       | 0   | ✅ (mal placée)

**h(état_initial) = 4 tuiles mal placées**

---

## 🗂️ **Structure des données A***

### **Nœud A***
```prolog
node(State, F, G, Parent) where:
- State = [1,2,3,5,0,6,4,7,8]  % Configuration actuelle
- F = G + H                    % f(n) = g(n) + h(n)
- G = Profondeur               % Coût réel depuis initial
- Parent = node(...) | nil     % Nœud parent pour backtrack
```

### **État initial**
```prolog
InitialNode = node([1,2,3,5,0,6,4,7,8], 4, 0, nil)
% F = 0 + 4 = 4
% G = 0 (profondeur initiale)
% H = 4 (tuiles mal placées calculées)
```

---

## 🎯 **Algorithme A* étape par étape**

### **Étape 1 : Initialisation**
```prolog
OpenList = [node([1,2,3,5,0,6,4,7,8], 4, 0, nil)]
ClosedList = []
NodesExpanded = 0
```

### **Étape 2 : Premier nœud (A)**
**État courant** : `[1,2,3,5,0,6,4,7,8]`
**Mouvements possibles** : haut, bas, gauche, droite (case vide au centre)

**Successeurs générés** :
1. **Haut** : `[1,0,3,5,2,6,4,7,8]` → h=4 → f=1+4=5
2. **Bas** : `[1,2,3,5,7,6,4,0,8]` → h=4 → f=1+4=5  
3. **Gauche** : `[1,2,3,0,5,6,4,7,8]` → h=3 → f=1+3=4
4. **Droite** : `[1,2,3,5,6,0,4,7,8]` → h=5 → f=1+5=6

**OpenList après expansion** :
```prolog
[
  node([1,2,3,0,5,6,4,7,8], 4, 1, A),      % Meilleur f=4
  node([1,0,3,5,2,6,4,7,8], 5, 1, A),      % f=5  
  node([1,2,3,5,7,6,4,0,8], 5, 1, A),      % f=5
  node([1,2,3,5,6,0,4,7,8], 6, 1, A)       % f=6
]
```
**NodesExpanded = 1**

### **Étape 3 : Nœud B (meilleur f=4)**
**État courant** : `[1,2,3,0,5,6,4,7,8]` 
**Mouvements possibles** : bas, droite

**Successeurs générés** :
1. **Bas** : `[1,2,3,4,5,6,0,7,8]` → h=2 → f=2+2=4
2. **Droite** : `[1,2,3,5,0,6,4,7,8]` → **DÉJÀ VISITÉ** (état A)

**OpenList mise à jour** : Insertion de `[1,2,3,4,5,6,0,7,8]` avec f=4

### **Étape 4 : Nœud C (f=4)**
**État courant** : `[1,2,3,4,5,6,0,7,8]`
**Mouvements possibles** : haut, bas, droite

**Successeurs générés** :
1. **Haut** : Retour vers B (déjà visité)
2. **Bas** : `[1,2,3,4,5,6,7,0,8]` → h=1 → f=3+1=4  
3. **Droite** : `[1,2,3,4,5,6,8,7,0]` → h=2 → f=3+2=5

### **Étape 5 : Nœud D (f=4)**  
**État courant** : `[1,2,3,4,5,6,7,0,8]`
**Successeurs générés** :
1. **Droite** : `[1,2,3,4,5,6,7,8,0]` → **ÉTAT BUT !** → h=0 → f=4+0=4

### **Étape 6 : Nœud E - BUT ATTEINT**
**État courant** : `[1,2,3,4,5,6,7,8,0]`
**Solution trouvée !**

---

## 📊 **Validation des résultats**

### **Path reconstruction (backtrack)**
```
E: [1,2,3,4,5,6,7,8,0] ← parent: D
D: [1,2,3,4,5,6,7,0,8] ← parent: C  
C: [1,2,3,4,5,6,0,7,8] ← parent: B
B: [1,2,3,0,5,6,4,7,8] ← parent: A
A: [1,2,3,5,0,6,4,7,8] ← parent: nil
```

**Path final** : [A, B, C, D, E] = **5 états** ✅

### **Cost calculation**
Nombre de mouvements = Profondeur du nœud final = **4** ✅

### **Expanded nodes count**
Nœuds explorés pendant la recherche :
1. A → 4 successeurs générés
2. B → 2 successeurs générés  
3. C → 3 successeurs générés
4. D → 1 successeur (solution)

**Total expanded = 4 + 2 + 3 + 1 = 10**
Mais selon l'énoncé : **9 nœuds explorés** (possiblement sans compter l'initial)

---

## 🔧 **Implémentation Prolog - Algorithme exact**

### **Prédicats principaux à implémenter**

```prolog
% Interface principale
solve_puzzle(case1, result(Path, Cost, Expanded)) :-
    initial_state(Initial),
    goal_state(Goal),
    astar_search(Initial, Goal, misplaced, Path, Cost, Expanded).

% Algorithme A* principal
astar_search(Initial, Goal, HeuristicType, Path, Cost, Expanded) :-
    heuristic_value(Initial, Goal, HeuristicType, H0),
    InitialNode = node(Initial, H0, 0, nil),
    astar_loop([InitialNode], Goal, HeuristicType, [], FinalNode, Expanded),
    FinalNode = node(_, _, Cost, _),
    reconstruct_path(FinalNode, Path).

% Boucle A* avec file de priorité
astar_loop([Node|_], Goal, _, _, Node, 0) :-
    Node = node(State, _, _, _),
    State = Goal, !.

astar_loop([CurrentNode|RestOpen], Goal, HType, Closed, FinalNode, Expanded) :-
    CurrentNode = node(CurrentState, _, G, _),
    \+ member(CurrentState, Closed),
    generate_moves(CurrentState, Successors),
    G1 is G + 1,
    create_successor_nodes(Successors, Goal, HType, G1, CurrentNode, NewNodes),
    append(RestOpen, NewNodes, UpdatedOpen),
    sort_by_f_value(UpdatedOpen, SortedOpen),
    Expanded1 is 1,
    astar_loop(SortedOpen, Goal, HType, [CurrentState|Closed], FinalNode, RestExpanded),
    Expanded is Expanded1 + RestExpanded.
```

### **Validation critique**
L'implémentation doit **absolument** produire :
- Cost = 4 
- Expanded = 9
- Path length = 5

Ces valeurs sont **non-négociables** pour la validation du TP1.

---

## ✅ **Checklist de validation**

- [ ] Heuristique tuiles mal placées implémentée correctement  
- [ ] État initial h=4 (validation manuelle)
- [ ] État but h=0 (validation manuelle)
- [ ] File de priorité A* fonctionnelle (tri par f-value)
- [ ] Gestion états visités (éviter cycles)
- [ ] Reconstruction chemin par backtracking
- [ ] **Test critique** : Cas professeur → Cost=4, Expanded=9, Path=5
- [ ] Interface `solve_puzzle/2` fonctionnelle
- [ ] Performance < 1 seconde

Cette planification assure la **validation exacte** du cas professeur selon les exigences du TP1.