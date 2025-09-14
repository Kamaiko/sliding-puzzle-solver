# Interface CLI/ASCII - Mockups et Design

## Plan structure pour interface CLI

### MOCKUP 1 : Ecran d'accueil anime

```
+===============================================================================+
|                                                                               |
|     ████████╗ █████╗  ██████╗ ██╗   ██╗██╗███╗   ██╗                       |
|     ╚══██╔══╝██╔══██╗██╔═══██╗██║   ██║██║████╗  ██║                       |
|        ██║   ███████║██║   ██║██║   ██║██║██╔██╗ ██║                       |
|        ██║   ██╔══██║██║▄▄ ██║██║   ██║██║██║╚██╗██║                       |
|        ██║   ██║  ██║╚██████╔╝╚██████╔╝██║██║ ╚████║                       |
|        ╚═╝   ╚═╝  ╚═╝ ╚═════╝  ╚═════╝ ╚═╝╚═╝  ╚═══╝                       |
|                                                                               |
|                    +---+---+---+                                            |
|                    | 1 | 2 | 3 |     SOLVEUR INTELLIGENT A*                 |
|                    +---+---+---+     IFT-2003 - IA                         |
|                    | 4 | 5 | 6 |     Universite Laval                      |
|                    +---+---+---+                                            |
|                    | 7 | 8 | # |     [Appuyez sur ENTREE]                  |
|                    +---+---+---+                                            |
|                                                                               |
+===============================================================================+
```

### MOCKUP 2 : Menu principal

```
+===============================================================================+
|                          MENU PRINCIPAL                                       |
+===============================================================================+
|                                                                               |
|  +---------------------------------+    +------------------------------+    |
|  | [1] CAS TEST CLASSIQUE          |    |  Preview:                    |    |
|  |     Cout optimal: 4             |    |  +---+---+---+  +---+---+---+|    |
|  |     Noeuds explores: 9          |    |  | 1 | 2 | 3 |  | 1 | 2 | 3 ||    |
|  |     Solution garantie           |    |  +---+---+---+  +---+---+---+|    |
|  +---------------------------------+    |  | 5 | # | 6 |->| 4 | 5 | 6 ||    |
|  | [2] CAS TEST AVANCE             |    |  +---+---+---+  +---+---+---+|    |
|  |     Difficulte: ***             |    |  | 4 | 7 | 8 |  | 7 | 8 | # ||    |
|  |     Mouvements min: 6+          |    |  +---+---+---+  +---+---+---+|    |
|  +---------------------------------+    |       Initial        Final    |    |
|  | [3] QUITTER                     |    +------------------------------+    |
|  +---------------------------------+                                         |
|                                                                               |
|  Votre choix: _                                                              |
+===============================================================================+
```

### MOCKUP 3 : Ecran de resolution

```
+===============================================================================+
|                       RESOLUTION - ALGORITHME A*                             |
+===============================================================================+
|                                                                               |
|                                                                               |
|  +-----------------+                           +-----------------+            |
|  |  ETAT INITIAL   |                           |  ETAT OBJECTIF  |            |
|  |  +---+---+---+  |                           |  +---+---+---+  |            |
|  |  | 1 | 2 | 3 |  |      ALGORITHME A*        |  | 1 | 2 | 3 |  |            |
|  |  +---+---+---+  |      EN COURS...          |  +---+---+---+  |            |
|  |  | 5 | # | 6 |  |                           |  | 4 | 5 | 6 |  |            |
|  |  +---+---+---+  |      [████████████████]       |  +---+---+---+  |            |
|  |  | 4 | 7 | 8 |  |                           |  | 7 | 8 | # |  |            |
|  |  +---+---+---+  |      Calcul optimal...    |  +---+---+---+  |            |
|  +-----------------+                           +-----------------+            |
|                                                                               |
|                                                                               |
|                        Heuristique: Tuiles mal placees                      |
|                                                                               |
+===============================================================================+
```

### MOCKUP 4 : Resultats de resolution

```
+===============================================================================+
|                           SOLUTION TROUVEE !                                 |
+===============================================================================+
|                                                                               |
|  CHEMIN OPTIMAL (5 etats, 4 mouvements):                                     |
|                                                                               |
|     ETAT A          ETAT B          ETAT C          ETAT D          ETAT E   |
|   +---+---+---+  +---+---+---+  +---+---+---+  +---+---+---+  +---+---+---+|
|   | 1 | 2 | 3 |  | 1 | 2 | 3 |  | 1 | 2 | 3 |  | 1 | 2 | 3 |  | 1 | 2 | 3 ||
|   +---+---+---+  +---+---+---+  +---+---+---+  +---+---+---+  +---+---+---+|
|   | 5 | # | 6 |->| # | 5 | 6 |->| 4 | 5 | 6 |->| 4 | 5 | 6 |->| 4 | 5 | 6 ||
|   +---+---+---+  +---+---+---+  +---+---+---+  +---+---+---+  +---+---+---+|
|   | 4 | 7 | 8 |  | 4 | 7 | 8 |  | # | 7 | 8 |  | 7 | # | 8 |  | 7 | 8 | # ||
|   +---+---+---+  +---+---+---+  +---+---+---+  +---+---+---+  +---+---+---+|
|       Initial        <-GAUCHE       <-HAUT         <-DROITE       <-DROITE  |
|                                                                               |
| +=========================================================================+ |
| |                          RESULTATS VALIDATION                            | |
| +=========================================================================+ |
| |                                                                           | |
| |  Path   : 5 etats (A->B->C->D->E)    |  Temps execution : 0.042s        | |
| |  Cost   : 4 mouvements               |  Heuristique    : Tuiles mal     | |
| |  Expanded: 9 noeuds explores         |                   placees          | |
| |                                       |                                   | |
| |  Algorithme A*: OPTIMAL              |  Solution       : GARANTIE        | |
| +=========================================================================+ |
|                                                                               |
|  [A] Animer solution  [M] Menu  [2] Cas test 2  [Q] Quitter                |
+===============================================================================+
```

### MOCKUP 5 : Animation pas-a-pas

```
+===============================================================================+
|                      ANIMATION - ETAPE 2/4                                   |
+===============================================================================+
|                                                                               |
|                         ETAT ACTUEL: B                                       |
|                                                                               |
|                        +---+---+---+                                        |
|                        | 1 | 2 | 3 |                                        |
|                        +---+---+---+                                        |
|                        | # | 5 | 6 |  <- Case vide deplacee a gauche       |
|                        +---+---+---+                                        |
|                        | 4 | 7 | 8 |                                        |
|                        +---+---+---+                                        |
|                                                                               |
|  +-----------------------------------------------------------------------+   |
|  | MOUVEMENT: GAUCHE                                                     |   |
|  | De: Position 4 vers Position 3                                        |   |
|  | Heuristique: h=3 (tuiles 4,7,8 mal placees)                         |   |
|  | Profondeur: 1                                                          |   |
|  +-----------------------------------------------------------------------+   |
|                                                                               |
|                 Progression: [██████░░░░░░░░░░] 2/4                        |
|                                                                               |
|  [N] Prochain  [P] Precedent  [A] Auto  [M] Menu  [Q] Quitter              |
+===============================================================================+
```

## Plan de developpement structure

### Phase 1: Interface de base

1. **Banniere ASCII art** au demarrage
2. **Menu interactif** avec previews
3. **Affichage etats** avec symboles clairs (# pour case vide)
4. **Transitions fluides** entre ecrans (clear + redraw)

### Phase 2: Finalisation

1. **Codes couleur ANSI** (optionnel, detectable)
2. **Controles intuitifs** pour navigation
3. **Messages d'erreur** clairs et utiles
4. **Interface coherente** dans tous les modes

## Elements cles pour impressionner

1. **ASCII Art creatif** : Logo, transitions, animations
2. **Feedback visuel** : Barres de progression, graphiques
3. **Interactivite** : Controles pendant l'execution
4. **Pedagogie** : Explications claires de l'algorithme
5. **Performance** : Metriques detaillees et comparaisons
6. **Professionnalisme** : Interface coherente et soignee

## Specifications techniques

### Caracteres ASCII utilises
- Bordures: +, -, |, =
- Cases taquin: +, -, |
- Case vide: # (au lieu de 0)
- Barres de progression: █, ░
- Fleches directionnelles: <-, ->, ^, v
- Arbre: /, \, |, [, ]

### Dimensions ecran
- Largeur: 79 caracteres (compatible terminal standard)
- Hauteur: Variable selon le contenu
- Marge: 2 caracteres de chaque cote

### Controles utilisateur
- Touches numeriques: Selection menu
- Espace: Pause/Resume
- Lettres: Actions specifiques (N, A, V, Q, etc.)
- Entree: Confirmation/Suivant

Cette approche reste 100% CLI/ASCII tout en offrant une experience visuelle 
riche et professionnelle qui impressionnera lors de la demonstration.