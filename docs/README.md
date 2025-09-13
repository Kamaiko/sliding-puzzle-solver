# Solveur de Taquin 3x3 avec A*

Projet de TP1 pour le cours IFT-2003 Intelligence Artificielle.

## Description

Ce projet implémente un système intelligent automatisé capable de résoudre le jeu du taquin (puzzle 3×3) en utilisant l'algorithme de recherche A* avec différentes heuristiques.

## Structure du projet

```
├── main.pl           # Point d'entrée principal
├── board.pl          # Représentation et manipulation de l'état
├── moves.pl          # Génération des mouvements légaux
├── heuristics.pl     # Fonctions heuristiques
├── search.pl         # Algorithme A*
├── utils.pl          # Utilitaires d'affichage
├── tests.pl          # Tests unitaires et cas de test
├── Plan_Developpement.md  # Plan détaillé du projet
├── TP1_Enonce_Reformule.md  # Énoncé reformulé
└── README.md         # Ce fichier
```

## Installation et utilisation

### Prérequis
- SWI-Prolog installé sur votre système

### Utilisation

1. Lancez SWI-Prolog
2. Chargez le fichier principal :
   ```prolog
   ?- [main].
   ```

3. Exécutez les cas de test :
   ```prolog
   ?- demo1.  % Cas de test du professeur
   ?- demo2.  % Cas de test personnalisé
   ```

4. Pour exécuter tous les tests :
   ```prolog
   ?- [tests].
   ?- run_all_tests.
   ```

## Résultats attendus

Le programme génère automatiquement :
- **Path** : Le chemin solution complet
- **Cost** : Le nombre de mouvements nécessaires
- **Expanded** : Le nombre de nœuds explorés

## Heuristiques implémentées

1. **Tuiles mal placées** : Compte le nombre de tuiles non à leur place finale
2. **Distance de Manhattan** : Somme des distances de Manhattan pour chaque tuile
3. **Conflits linéaires** : Heuristique avancée combinant Manhattan et conflits

## Tests

Le projet inclut des tests unitaires pour :
- Opérations sur le plateau
- Génération de mouvements
- Fonctions heuristiques
- Algorithme A*
- Cas de test complets

## Architecture

Le projet suit une architecture modulaire pour éviter le code spaghetti :
- Séparation claire des responsabilités
- Interfaces bien définies entre modules
- Code documenté et réutilisable

## Auteur

[Votre nom]  
Cours IFT-2003 Intelligence Artificielle  
[Date]