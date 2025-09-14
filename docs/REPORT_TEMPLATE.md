# 📄 Rapport TP1 - Solveur de Taquin avec Recherche Heuristique

**Cours** : IFT-2003 Intelligence Artificielle  
**Projet** : Conception d'un jeu intégrant une recherche heuristique  
**Équipe** : [Noms des membres]  
**Date** : [Date de remise]  
**Professeur** : [Nom du professeur]

---

## 🎯 1. Modélisation du problème (20%)

### 1.1 Description du problème à résoudre
> **À compléter** : Description claire du jeu du Taquin 3x3 et de l'objectif de résolution automatisée.

[Votre description ici]

### 1.2 État initial et état final
> **À compléter** : Présenter les configurations de départ et d'arrivée avec illustrations.

#### État initial (Cas test 1)
```
1 2 3
5 * 6
4 7 8
```
**Représentation interne** : `[1,2,3,5,0,6,4,7,8]`

#### État final (But à atteindre)
```
1 2 3
4 5 6
7 8 *
```
**Représentation interne** : `[1,2,3,4,5,6,7,8,0]`

[Ajouter explications sur le format choisi et sa justification]

### 1.3 Mouvements autorisés
> **À compléter** : Détailler les 4 mouvements possibles avec exemples et contraintes.

1. **Mouvement HAUT** : [Description et exemple]
2. **Mouvement BAS** : [Description et exemple]  
3. **Mouvement GAUCHE** : [Description et exemple]
4. **Mouvement DROITE** : [Description et exemple]

[Expliquer les contraintes de bord et validation des mouvements]

### 1.4 Technique de recherche utilisée
> **À compléter** : Justification du choix de l'algorithme A* et présentation de la méthode.

**Algorithme sélectionné** : A* (A-star)

**Justification** :
- [Expliquer pourquoi A* est approprié pour ce problème]
- [Avantages par rapport à d'autres algorithmes]
- [Garantie d'optimalité avec heuristique admissible]

**Principe de fonctionnement** :
- Fonction d'évaluation : f(n) = g(n) + h(n)
- [Expliquer g(n) et h(n) dans le contexte du Taquin]

### 1.5 Heuristique choisie
> **À compléter** : Détailler l'heuristique des tuiles mal placées et sa pertinence.

**Heuristique principale** : Nombre de tuiles mal placées (excluant case vide)

**Définition formelle** : [Formule mathématique]

**Exemple de calcul** : [Calcul détaillé pour l'état initial]

**Propriétés** :
- Admissibilité : [Démonstration que h(n) ≤ h*(n)]
- Consistance : [Explication]

### 1.6 Résultats attendus
> **À compléter** : Présenter les métriques de performance attendues.

Pour le cas test du professeur :
- **Path** : Séquence de 5 états (A→B→C→D→E)
- **Cost** : 4 mouvements précisément
- **Expanded** : 9 nœuds explorés (excluant l'état initial)
- **Temps** : < 1 seconde

### 1.7 Exemples pertinents
> **À compléter** : Illustrer avec des schémas ou captures d'écran.

[Insérer diagrammes, captures d'écran, ou illustrations]

---

## 💻 2. Implémentation (45%)

### 2.1 Architecture du programme
> **À compléter** : Présenter la structure modulaire choisie.

**Modules développés** :
```
src/
├── main.pl       # Interface CLI + orchestration
├── game.pl       # États du taquin + mouvements  
├── astar.pl      # Algorithme A* + heuristiques
├── display.pl    # Affichage et formatage
└── tests.pl      # Tests unitaires + validation
```

[Expliquer le rôle de chaque module et leurs interactions]

### 2.2 Implémentation de l'algorithme de recherche
> **À compléter** : Détailler l'implémentation A* en Prolog.

#### Structure des données
```prolog
% Structure d'un nœud A*
node(State, F, G, Parent)
% - State: Configuration [1,2,3,5,0,6,4,7,8]
% - F: f(n) = g(n) + h(n)
% - G: Coût réel depuis initial (profondeur)
% - Parent: Référence pour reconstruction chemin
```

#### Algorithme principal
```prolog
% Code Prolog principal
[Insérer les prédicats clés avec commentaires explicatifs]
```

### 2.3 Implémentation de l'heuristique
> **À compléter** : Code et explication de l'heuristique des tuiles mal placées.

```prolog
% Prédicat de calcul heuristique
misplaced_tiles(State, Goal, H) :-
    % [Votre implémentation commentée]
```

[Expliquer le fonctionnement ligne par ligne]

### 2.4 Traduction des choix de recherche en Prolog
> **À compléter** : Expliquer comment les concepts théoriques A* ont été traduits en prédicats Prolog.

- **File de priorité** : [Explication implémentation]
- **États visités** : [Gestion de la liste fermée]
- **Génération successeurs** : [Méthode utilisée]
- **Reconstruction chemin** : [Backtracking implémenté]

### 2.5 Guide d'utilisation du programme
> **À compléter** : Instructions complètes pour utiliser le solveur.

#### Installation
```bash
# Étapes d'installation SWI-Prolog
[Instructions détaillées]
```

#### Utilisation
```bash
# Lancement du programme
swipl src/main.pl

# Menu principal
# [Capture d'écran du menu]
```

#### Commandes principales
- Option 1 : [Description]
- Option 2 : [Description]  
- Option 3 : [Description]

#### Tests
```bash
# Exécution des tests
swipl src/tests.pl
?- run_all_tests.
```

---

## 📊 3. Résultats et discussion (25%)

### 3.1 Jeux d'essai et résultats
> **À compléter** : Présenter les résultats des 2 cas de test obligatoires.

#### Cas de test 1 - Exemple du professeur
```
État initial:    État final:
1 2 3           1 2 3
5 * 6           4 5 6  
4 7 8           7 8 *
```

**Résultats obtenus** :
- Path : [Séquence complète des états A→B→C→D→E]
- Cost : [Valeur obtenue]
- Expanded : [Nombre de nœuds explorés]
- Temps d'exécution : [Mesure en secondes]

[Insérer captures d'écran de l'exécution]

#### Cas de test 2 - Configuration personnalisée
> **À compléter** : Présenter votre cas de test personnalisé (min 6 mouvements).

```
État initial:    État final:
[Configuration]  [Configuration]
```

**Résultats obtenus** :
- Path : [Séquence d'états]
- Cost : [Nombre mouvements]  
- Expanded : [Nœuds explorés]
- Temps : [Performance]

### 3.2 Évaluation par rapport aux attentes
> **À compléter** : Analyser si les objectifs ont été atteints.

**Conformité aux exigences** :
- ✅/❌ Cas test 1 : Cost=4, Expanded=9 exactement
- ✅/❌ Performance < 1 seconde
- ✅/❌ Interface CLI fonctionnelle
- ✅/❌ 2 cas de test validés

**Analyse des écarts** : [Si résultats différents, expliquer pourquoi]

### 3.3 Avantages de l'approche (minimum 1)
> **À compléter** : Identifier au moins 1 avantage significatif.

1. **[Titre de l'avantage]** : [Description détaillée]
2. **[Autre avantage si applicable]** : [Description]

### 3.4 Limites identifiées (minimum 2)
> **À compléter** : Analyser honnêtement les limitations de votre solution.

1. **[Première limite]** : [Description et impact]
2. **[Deuxième limite]** : [Description et conséquences]
3. **[Limite supplémentaire si applicable]** : [Analyse]

### 3.5 Travaux futurs (minimum 2 améliorations)
> **À compléter** : Proposer des améliorations concrètes.

1. **[Amélioration 1]** : [Description et bénéfices attendus]
2. **[Amélioration 2]** : [Faisabilité et impact]
3. **[Amélioration 3]** : [Innovation possible]

### 3.6 Méthodes de test utilisées
> **À compléter** : Décrire votre stratégie de validation.

**Tests unitaires** :
- Tests par module (game, astar, display)
- [Liste des prédicats testés]

**Tests d'intégration** :
- [Méthodes de validation globale]

**Tests de robustesse** :
- Gestion états impossibles
- [Autres scénarios testés]

---

## 📝 Conclusion

> **À compléter** : Synthèse personnelle de l'apprentissage et du projet.

[Réflexion sur les apprentissages techniques, les défis rencontrés, et l'utilité du projet pour votre formation en IA]

---

## 📎 Annexes

### Annexe A : Code source complet
> **À joindre** : Fichiers .pl commentés

### Annexe B : Captures d'écran
> **À insérer** : Interface et résultats d'exécution

### Annexe C : Tests et validation
> **À documenter** : Résultats des tests automatisés

---

## ✅ Checklist avant remise

- [ ] **Modélisation (20%)** : Toutes les sections complétées
- [ ] **Implémentation (45%)** : Code documenté et guide d'utilisation
- [ ] **Résultats (25%)** : 2 cas de test validés avec analyses
- [ ] **Présentation (10%)** : Format respecté, aucune faute
- [ ] **Fichiers** : PDF rapport + fichier .PL fonctionnel
- [ ] **Auto-évaluation** : Grille remplie selon critères TP

**Date de remise** : 20 octobre 2025, 21h00  
**Plateforme** : Portail des cours, section "Évaluation et résultats"

---

*Template généré pour faciliter la rédaction du rapport final TP1*