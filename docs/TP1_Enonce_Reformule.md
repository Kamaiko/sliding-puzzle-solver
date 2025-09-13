# TP1 - Conception d'un jeu intégrant une recherche heuristique

## Informations générales
- **Pondération**: 10% de la note finale
- **Date limite**: 20 octobre 2025 à 21h00
- **Langage requis**: Prolog
- **Livrable**: Un fichier PDF (rapport) + un fichier .PL (programme)

## Objectif principal

Développer un système intelligent automatisé capable d'explorer les différents états du **jeu du taquin (puzzle 3×3)** pour déterminer le chemin optimal entre un état initial et un état final.

## Exemple de référence

### États de départ et d'arrivée
- **État initial**:
  ```
  1 2 3
  5 * 6
  4 7 8
  ```

- **État final**:
  ```
  1 2 3
  4 5 6
  7 8 *
  ```

### Résultats attendus du système

Le programme doit générer automatiquement:

1. **Path**: Le chemin solution complet montrant toutes les configurations du taquin de l'état initial à l'état but (ex: états A→B→C→D→E)

2. **Cost**: La profondeur de la solution = nombre de déplacements nécessaires (ex: 4 si l'état initial est à profondeur 0)

3. **Expanded**: Mesure d'efficacité = nombre total de nœuds générés et explorés pour atteindre la solution (ex: 9 en excluant l'état initial)

### Heuristique suggérée
Utiliser comme heuristique **H** le nombre de tuiles mal placées (en excluant la case vide).

## Structure du travail (4 parties)

### 1. Modélisation du problème (20%)

**Objectifs**:
- Décrire clairement le problème à résoudre
- Expliquer l'état initial et l'état final
- Détailler tous les mouvements autorisés
- Présenter la/les technique(s) de recherche utilisée(s)
- Définir les résultats attendus
- Illustrer avec des exemples pertinents

### 2. Implémentation (45%)

**Objectifs**:
- Programmer le jeu en Prolog
- Implémenter la/les technique(s) de recherche décrite(s) en (1)
- Développer une heuristique adaptée au jeu du taquin
- Expliquer clairement la traduction des choix de recherche heuristique en code Prolog
- Fournir un guide d'utilisation complet du programme

### 3. Résultats et discussion (25%)

**Objectifs**:
- Analyser les résultats obtenus
- Vérifier si les buts fixés sont atteints
- Décrire les méthodes de test du programme
- Valider la performance du système
- Discuter des limites de l'heuristique implémentée

### 4. Rapport (10%)

**Objectifs**:
- Rédiger un rapport intégrant les parties (1), (2) et (3)
- Soigner l'expression écrite et la présentation
- Utiliser le template fourni par l'enseignant

## Exigences de test

- Tester **2 états initiaux** et **2 états buts** sélectionnés aléatoirement
- Possibilité d'utiliser l'exemple fourni comme premier cas de test
- Obligation de définir le second cas de test par le groupe

## Grille d'auto-évaluation - Critères principaux

### I. Modélisation (20%)
- État initial: expliqué et illustré
- État final: expliqué et illustré  
- Mouvements: tous expliqués
- Technique de recherche: choisie du cours et appliquée
- Résultats attendus: présentés et expliqués

### II. Programme (45%)
- Implémentation réalisée
- Guide d'utilisation complet
- Code de recherche heuristique clairement expliqué
- Programme compile sans erreur
- Programme s'exécute toujours sans erreur
- Documentation complète de chaque prédicat

### III. Résultats et discussion (25%)
- Résultats décrits par des jeux d'essai
- Résultats évalués par rapport aux attentes
- Au moins 1 avantage décrit
- Au moins 2 limites décrites
- Plus de 2 améliorations proposées

### IV. Appréciation globale (10%)
- Expression écrite: aucune faute
- Présentation: format respecté et rapport paginé

## Modalités de remise

- **Plateforme**: Portail des cours, rubrique "Évaluation et résultats"
- **Fichiers**: 1 PDF (rapport) + 1 fichier .PL (programme)
- **Auto-évaluation**: Utiliser la grille fournie avant remise
- **Retard**: Note 0 (sauf entente préalable avec l'enseignant)

---

## Grille d'évaluation détaillée (selon image fournie)

### I. Modélisation (20%)
- **État initial**: expliqué et illustré
- **État final**: expliqué et illustré
- **Mouvements**: tous les mouvements sont expliqués
- **Technique de recherche**: une technique de recherche vue au cours est choisie et appliquée au jeu
- **Résultats attendus**: sont présentés et expliqués

### II. Programme (45%)
- **Implémentation**: une implémentation a été faite
- **Guide d'utilisation**: un guide d'utilisation complet est disponible
- **Code de la recherche heuristique**: le code de la recherche heuristique est expliqué clairement
- **Si implémentation faite**:
  - **Compilation**: le programme compile sans erreur
  - **Exécution**: le programme s'exécute toujours sans erreur
  - **Documentation**: le programme est documenté pour chaque prédicat. Le texte est fonction de l'importance du prédicat

### III. Résultats et discussion (25%)
- **Résultats**: les résultats sont décrits par des jeux d'essai
- **Évaluation**: les résultats sont évalués par rapport à ceux attendus
- **Avantages**: au moins 1 avantage est décrit par rapport au programme initial
- **Limites**: au moins 2 limites sont décrites
- **Travaux futurs**: plus de 2 améliorations sont proposées

### IV. Appréciation globale (10%)
- **Expression écrite**: le rapport ne contient aucune faute (vocabulaire, grammaire, syntaxe, etc.)
- **Présentation**: le format proposé est respecté. Le rapport est paginé

## Exigences spécifiques du TP1

- **2 cas de test obligatoires**
- **Résultats exacts**: Path/Cost/Expanded
- **Exemple professeur DOIT donner**: 9 nœuds explorés, 4 mouvements
- **Heuristique**: Tuiles mal placées (excluant case vide)