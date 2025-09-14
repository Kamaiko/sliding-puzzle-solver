# Interface CLI/ASCII - Mockups et Design

## Plan structure pour interface CLI

### MOCKUP 1 : Menu principal unifié - VERSION UTF-8

```
╔═══════════════════════════════════════════════════════════════════════════════╗
║     ████████╗ █████╗  ██████╗ ██╗   ██╗██╗███╗   ██╗                          ║
║     ╚══██╔══╝██╔══██╗██╔═══██╗██║   ██║██║████╗  ██║                          ║
║        ██║   ███████║██║   ██║██║   ██║██║██╔██╗ ██║                          ║
║        ██║   ██╔══██║██║▄▄ ██║██║   ██║██║██║╚██╗██║                          ║
║        ██║   ██║  ██║╚██████╔╝╚██████╔╝██║██║ ╚████║                          ║
║        ╚═╝   ╚═╝  ╚═╝ ╚═════╝  ╚═════╝ ╚═╝╚═╝  ╚═══╝                          ║
║        ███████╗ ██████╗ ██╗    ██╗   ██╗███████╗██╗   ██╗██████╗              ║
║        ██╔════╝██╔═══██╗██║    ██║   ██║██╔════╝██║   ██║██╔══██╗             ║
║        ███████╗██║   ██║██║    ██║   ██║█████╗  ██║   ██║██████╔╝             ║
║        ╚════██║██║   ██║██║    ╚██╗ ██╔╝██╔══╝  ██║   ██║██╔══██╗             ║
║        ███████║╚██████╔╝███████╗╚████╔╝ ███████╗╚██████╔╝██║  ██║             ║
║        ╚══════╝ ╚═════╝ ╚══════╝ ╚═══╝  ╚══════╝ ╚═════╝ ╚═╝  ╚═╝             ║
║                         ╔═════════════════════════╗                           ║
║                         ║ [1] CAS TEST CLASSIQUE  ║                           ║
║                         ╠═════════════════════════╣                           ║
║                         ║ [2] CAS TEST AVANCE     ║                           ║
║                         ╠═════════════════════════╣                           ║
║                         ║ [3] A PROPOS            ║                           ║
║                         ╠═════════════════════════╣                           ║
║                         ║ [4] QUITTER             ║                           ║
║                         ╚═════════════════════════╝                           ║
║                           SOLVEUR INTELLIGENT A*                              ║
║                               IFT-2003 - IA                                   ║
║                             Universite Laval                                  ║
╚═══════════════════════════════════════════════════════════════════════════════╝

Votre choix: _
```

### MOCKUP 2.5 : A PROPOS - VERSION UTF-8

```
╔═══════════════════════════════════════════════════════════════════════════════╗
║                                   A PROPOS                                    ║
╠═══════════════════════════════════════════════════════════════════════════════╣
║                                                                               ║
║  SOLVEUR DE TAQUIN A*                                                         ║
║  Version 1.0                                                                  ║
║                                                                               ║
║  COURS        : IFT-2003 - Intelligence Artificielle                          ║
║  INSTITUTION  : Universite Laval                                              ║
║  PROJET       : Travail pratique - Algorithme de recherche A*                 ║
║                                                                               ║
║  ALGORITHME   : A* (A-star) avec heuristique tuiles mal placees               ║
║                                                                               ║
║  EQUIPE :                                                                     ║
║    • Patrick Patenaude                                                        ║
║    • Xavier Gagnon                                                            ║
║    • Daniel Jose Anillo Santos                                                ║
║    • Alexandre Gamache                                                        ║
║                                                                               ║
╚═══════════════════════════════════════════════════════════════════════════════╝

[Appuyez sur ENTREE pour continuer...]
```

### MOCKUP 3 : Resultats de resolution - VERSION UTF-8

```
╔══════════════════════════════════════════════════════════╗
║                     SOLUTION TROUVEE                     ║
╚══════════════════════════════════════════════════════════╝

   │ 1 2 3 │
   │ 5 # 6 │
   │ 4 7 8 │
        ↓ GAUCHE
   │ 1 2 3 │
   │ # 5 6 │
   │ 4 7 8 │
        ↓ BAS
   │ 1 2 3 │
   │ 4 5 6 │
   │ # 7 8 │
        ↓ DROITE
   │ 1 2 3 │
   │ 4 5 6 │
   │ 7 # 8 │
        ↓ DROITE
   │ 1 2 3 │
   │ 4 5 6 │
   │ 7 8 # │
   [BUT ATTEINT!]

╔══════════════════════════════════════════════════════════╗
║                    RESULTATS OBTENUS                     ║
╚══════════════════════════════════════════════════════════╝
[INFO] Longueur Path : 5 etats (Initial -> But)
[INFO] Cost         : 4 mouvements
[INFO] Expanded     : 9 noeuds explores
[INFO] Temps IA     : 0.042 secondes

Votre choix: _
```

### MOCKUP 4 : Exécution des tests - VERSION UTF-8

```
╔═══════════════════════════════════════════════════════════════════════════════╗
║                       VALIDATION AUTOMATISEE - TESTS                          ║
╠═══════════════════════════════════════════════════════════════════════════════╣
║                                                                               ║
║  === DEBUT TESTS SOLVEUR TAQUIN A* ===                                        ║
║                                                                               ║
║  [TEST] Module game.pl                                                        ║
║    [OK] find_blank/2         : Position case vide                     [PASS]  ║
║    [OK] generate_moves/2     : Generation 4 directions                [PASS]  ║
║    [OK] validate_state/1     : Validation configuration               [PASS]  ║
║    [OK] apply_move/3         : Application mouvement                  [PASS]  ║
║                                                            4/4 tests passes   ║
║                                                                               ║
║  [TEST] Module astar.pl                                                       ║
║    [OK] misplaced_tiles/3    : h([1,2,3,5,0,6,4,7,8]) = 4             [PASS]  ║
║    [OK] astar_search/5       : Algorithme A* avec closed set          [PASS]  ║
║    [OK] reconstruct_path/2   : Reconstruction chemin parents          [PASS]  ║
║                                                            3/3 tests passes   ║
║                                                                               ║
║  [TEST] Module display.pl                                                     ║
║    [OK] display_grid/1       : Affichage grille 3x3                   [PASS]  ║
║    [OK] format_results/3     : Format Path/Cost/Expanded              [PASS]  ║
║                                                            2/2 tests passes   ║
║                                                                               ║
║  [TEST] CAS TEST CRITIQUE                                                     ║
║    [OK] Cas test 1           : Cost=4, Expanded=9 exact               [PASS]  ║
║    [OK] Heuristique initiale : h=4 pour etat professeur               [PASS]  ║
║    [OK] Path length          : 5 etats (A->B->C->D->E)                [PASS]  ║
║    [OK] Determinisme         : Resultats identiques x3                [PASS]  ║
║                                                            4/4 tests passes   ║
║                                                                               ║
║  [TEST] Integration complete                                                  ║
║    [OK] Pipeline complet     : game -> astar -> display               [PASS]  ║
║    [OK] Performance          : Temps < 1.0 seconde                    [PASS]  ║
║                                                            2/2 tests passes   ║
║                                                                               ║
║ ╔═════════════════════════════════════════════════════════════════════════╗   ║
║ ║                         RESUME DES TESTS                                ║   ║
║ ╠═════════════════════════════════════════════════════════════════════════╣   ║
║ ║                                                                         ║   ║
║ ║  Total : 15/15 tests passes                    Temps total : 0.234s     ║   ║
║ ║  Couverture : 100% des predicats critiques                              ║   ║
║ ║  Statut : [SUCCESS] VALIDATION COMPLETE                                 ║   ║
║ ║                                                                         ║   ║
║ ║                                                                         ║   ║
║ ╚═════════════════════════════════════════════════════════════════════════╝   ║
║                                                                               ║
╚═══════════════════════════════════════════════════════════════════════════════╝
```


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

