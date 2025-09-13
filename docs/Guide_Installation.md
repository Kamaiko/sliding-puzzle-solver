# Guide d'installation et configuration

## Prérequis système

### SWI-Prolog
- **Version requise** : SWI-Prolog 8.0 ou supérieure
- **Installation** :
  - **Windows** : Télécharger depuis [swi-prolog.org](https://www.swi-prolog.org/download/stable)
  - **macOS** : `brew install swi-prolog`
  - **Linux** : `sudo apt-get install swi-prolog` (Ubuntu/Debian)

### Vérification de l'installation
```bash
swipl --version
```

## Structure du projet

```
IFT-2003_IA1/
├── src/            # Code source Prolog
│   ├── main.pl     # Point d'entrée principal
│   ├── board.pl    # Gestion des états
│   ├── moves.pl    # Génération mouvements
│   ├── heuristics.pl # Fonctions heuristiques
│   ├── search.pl   # Algorithme A*
│   └── utils.pl    # Utilitaires affichage
├── tests/          # Tests unitaires
│   └── tests.pl    # Suite de tests complète
├── docs/           # Documentation
│   ├── Plan_Developpement.md
│   ├── README.md
│   └── Guide_Installation.md
├── archive/        # Fichiers de référence
│   ├── TP1_Aut_2025.pdf
│   └── ExempleResolution.png
├── Makefile        # Commandes de build/test
└── .gitignore      # Configuration Git
```

## Commandes rapides

### Lancer le programme
```bash
make run
# ou directement:
swipl -q -t main -s src/main.pl
```

### Exécuter les tests
```bash
make test
# Tests spécifiques:
make test-prof    # Cas du professeur
make test-custom  # Cas personnalisé
```

### Aide
```bash
make help
```

## Configuration de l'environnement

### Variables d'environnement (optionnel)
```bash
export SWIPL_STACK_LIMIT=128m
export SWIPL_TABLE_SPACE=64m
```

### Éditeur recommandé
- **VSCode** avec extension "SWI-Prolog"
- **Vim** avec plugin Prolog
- **Emacs** avec mode Prolog

## Résolution des problèmes courants

### "swipl command not found"
- Vérifier l'installation de SWI-Prolog
- Ajouter SWI-Prolog au PATH système

### Erreurs de mémoire
- Augmenter les limites de pile :
  ```bash
  swipl --stack_limit=256m -s src/main.pl
  ```

### Erreurs de module
- Vérifier que tous les fichiers .pl sont dans src/
- Vérifier la syntaxe des directives `:- use_module()`

## Tests de validation

### Test d'installation
```prolog
?- [src/main].
?- demo1.
```

Résultat attendu : Affichage du puzzle avec solution en 4 mouvements.