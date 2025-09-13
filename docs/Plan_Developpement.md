# Plan de développement - Jeu du Taquin avec recherche heuristique

## 📊 Estimation d'effort global
**25-30 heures** réparties sur 2 semaines (réduit grâce aux clarifications)

## 🎯 Architecture modulaire proposée

### Structure des fichiers
```
taquin_solver/
├── main.pl           # Point d'entrée et interface utilisateur
├── board.pl          # Représentation et manipulation de l'état
├── moves.pl          # Génération des mouvements légaux
├── heuristics.pl     # Fonctions heuristiques (Manhattan, tuiles mal placées)
├── search.pl         # Algorithme A* et gestion de la file de priorité
├── utils.pl          # Utilitaires (affichage, statistiques)
├── tests.pl          # Tests unitaires et cas de test
└── README.md         # Documentation
```

## ✅ Clarifications reçues

1. **Environnement Prolog** : SWI-Prolog confirmé
2. **Interface** : Interface ligne de commande (CLI) avec menu principal
3. **Heuristiques** : Heuristique principale = tuiles mal placées (énoncé). Manhattan optionnelle pour comparaison
4. **Performance** : Benchmarks mineurs uniquement (temps de réponse IA entre mouvements)
5. **Robustesse** : Afficher message d'erreur pour états impossibles
6. **Format de sortie** : Affichage standard du taquin avec Path/Cost/Expanded + temps de réponse IA

## 🚀 Phases de développement

### Phase 1: Modélisation (4-6h)
- Définir la représentation de l'état (liste ou matrice)
- Créer les prédicats de base pour l'état initial/final
- Implémenter l'affichage du plateau

### Phase 2: Mécanique du jeu (6-8h)
- Générer les mouvements valides
- Valider les transitions d'état
- Implémenter heuristique principale (tuiles mal placées)
- Manhattan optionnelle pour comparaison
- Créer les prédicats de test de solution
- Gestion des états impossibles

### Phase 3: Algorithme A* (8-10h)
- **File de priorité** : Best-First avec f(n) = g(n) + h(n)
- **Exploration systématique** : Générer TOUS les successeurs à chaque niveau
- **Étiquetage des états** : A, B, C, D, E pour traçabilité du chemin
- **Fonction d'évaluation** : g(n) = profondeur, h(n) = tuiles mal placées
- **Gestion des états visités** : Éviter cycles et redondances
- **Reconstruction du chemin** : Backtracking depuis état final vers initial
- **Comptage précis** : Nœuds explorés (excluant état initial)
- **Interface CLI** avec menu principal et temps de réponse IA

### Phase 4: Tests et optimisation (4-6h)
- **Cas de test 1** : Valider contre exemple professeur (Path/Cost/Expanded)
- **Cas de test 2** : Créer et valider cas personnalisé (6+ mouvements)
- **Benchmarks de performance** :
  - Temps de réponse entre chaque mouvement de l'IA
  - Temps total de résolution (< 1s pour 3x3)
  - Comparaison heuristiques (si Manhattan implémentée)
- **Tests de robustesse** : États impossibles, timeout, limites
- **Validation finale** : Tous les critères de succès respectés

### Phase 5: Documentation et rapport (3-4h)
- Guide d'utilisation simplifié
- Documentation code essentielle
- Rapport selon le template
- Captures d'écran

## 🔧 Bonnes pratiques pour éviter le code spaghetti

1. **Séparation des responsabilités** : Chaque module a un rôle unique
2. **Prédicats atomiques** : Petites fonctions réutilisables
3. **Documentation inline** : Commentaires pour chaque prédicat
4. **Conventions de nommage** : Préfixes par module (board_, search_, etc.)
5. **Tests unitaires** : Validation continue de chaque composant
6. **Structure de données cohérente** : État = [1,2,3,5,0,6,4,7,8] où 0=case vide

## 💡 Recommandations techniques

- Utiliser tuiles mal placées comme heuristique principale (selon énoncé)
- Manhattan optionnelle pour comparaison de performance
- Implémenter une table de hachage pour les états visités
- Utiliser assert/retract avec parcimonie pour les performances
- Interface CLI claire avec temps de réponse de l'IA
- Messages d'erreur explicites pour états impossibles

## 📊 Analyse détaillée de l'algorithme A* (ExempleResolution.png)

### **Arbre de recherche complet analysé**

**ÉTAT A (Initial)** : `1 2 3 / 5 * 6 / 4 7 8`

**ÉTAT B (4 successeurs explorés)** :
1. `H=5` - Droite : `1 2 3 / 5 6 * / 4 7 8`
2. `H=4` - Bas : `1 2 3 / 5 7 6 / 4 * 8`  
3. `H=5` - Haut : `1 * 3 / 5 2 6 / 4 7 8`
4. `H=3` - **Gauche (optimal)** : `1 2 3 / * 5 6 / 4 7 8`

**ÉTAT C (2 successeurs depuis H=3)** :
1. `H=4` - Mouvement sous-optimal
2. `H=2` - **Optimal** : `1 2 3 / 4 5 6 / * 7 8`

**ÉTAT D (1 état)** : `1 2 3 / 4 5 6 / 7 * 8` (H=1)

**ÉTAT E (2 successeurs finaux)** :
1. `H=2` - Sous-optimal : `1 2 3 / 4 * 6 / 7 5 8`
2. `H=0` - **SUCCÈS** : `1 2 3 / 4 5 6 / 7 8 *`

### **Résultats confirmés**
- **Path** : A→B(gauche)→C→D→E = 5 configurations
- **Cost** : 4 mouvements 
- **Expanded** : 9 nœuds (1+4+2+1+2 - 1 initial = 9)
- **Heuristique** : Tuiles mal placées (excluant case vide)

## 🔧 Approche hybride pour états impossibles

1. **Timeout** : 10 secondes maximum
2. **Limite de nœuds** : 10,000 nœuds explorés maximum  
3. **Messages d'erreur** :
   - "Timeout - État possiblement impossible à résoudre"
   - "Limite d'exploration atteinte - Vérifiez l'état initial"

## ⏱️ Planning suggéré

**Semaine 1**: Phases 1-2 (modélisation et mécanique)
**Semaine 2**: Phase 3 (algorithme A* exact selon l'exemple)

## 📋 Checklist de développement

- [ ] **Phase 1**: Modélisation de base
  - [ ] Structure de données pour l'état
  - [ ] Prédicats de manipulation de base
  - [ ] Affichage du plateau
  - [ ] États initial et final

- [ ] **Phase 2**: Mécanique du jeu
  - [ ] Génération des mouvements valides (4 directions possibles)
  - [ ] Validation des transitions d'état
  - [ ] **Heuristique principale** : Tuiles mal placées (excluant case vide)
  - [ ] **Heuristique optionnelle** : Distance de Manhattan (pour comparaison)
  - [ ] Test de solution et détection état but
  - [ ] Gestion des états impossibles (timeout + limite nœuds)

- [ ] **Phase 3**: Algorithme A*
  - [ ] **File de priorité Best-First** : f(n) = g(n) + h(n)
  - [ ] **Exploration systématique** : Générer TOUS les successeurs à chaque niveau
  - [ ] **Étiquetage des états** : A, B, C, D, E pour traçabilité
  - [ ] **Fonction d'évaluation** : g(n) = profondeur, h(n) = tuiles mal placées
  - [ ] **Gestion des états visités** : Table de hachage pour éviter cycles
  - [ ] **Reconstruction du chemin** : Backtracking depuis solution vers initial
  - [ ] **Comptage précis des nœuds** : 9 explorés (excluant initial) pour test 1
  - [ ] **Interface CLI** : Menu principal + temps de réponse IA

- [ ] **Phase 4**: Tests et optimisation
  - [ ] 2 cas de test requis
  - [ ] Tests unitaires
  - [ ] Mesure Path/Cost/Expanded
  - [ ] Optimisation performances

- [ ] **Phase 5**: Documentation
  - [ ] Guide d'utilisation
  - [ ] Documentation code
  - [ ] Rapport final
  - [ ] Captures d'écran

## 🎮 Cas de test planifiés

### Test 1 (Exemple du professeur - Validation obligatoire)
```
État initial (A):   État final (E):
1 2 3              1 2 3
5 * 6              4 5 6
4 7 8              7 8 *
```

**Résultats attendus exacts** :
- **Path** : A→B(gauche)→C→D→E (5 configurations affichées)
- **Cost** : 4 mouvements (profondeur de la solution)
- **Expanded** : 9 nœuds explorés (excluant l'état initial)
- **Heuristique** : Tuiles mal placées (excluant case vide)
- **Temps** : < 1 seconde pour résolution

### Test 2 (Personnalisé - À définir par l'équipe)
État initial et final plus complexe pour démontrer la robustesse de l'algorithme.
**Critères** : Minimum 6 mouvements, solvable, différent du test 1.

## 🔍 Critères de succès

1. **Validation exacte Test 1** : 
   - Path: A→B→C→D→E (5 états)
   - Cost: 4 mouvements précisément
   - Expanded: 9 nœuds (excluant initial)
   
2. **Performance optimale** : 
   - Solution optimale trouvée (coût minimal)
   - Temps de résolution < 1 seconde pour 3x3
   - Temps de réponse IA affiché entre chaque mouvement
   
3. **Robustesse** : 
   - Gère les états impossibles (timeout 10s + limite 10k nœuds)
   - Messages d'erreur clairs et informatifs
   
4. **Architecture propre** : 
   - Code modulaire sans spaghetti
   - Documentation complète de chaque prédicat
   - Tests unitaires passent tous
   
5. **Conformité TP** : 
   - Interface CLI avec menu principal
   - Affichage Path/Cost/Expanded conforme
   - 2 cas de test validés
   - Rapport selon template fourni