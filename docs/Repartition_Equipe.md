# Répartition des tâches - Équipe de 4 personnes
**Projet :** Solveur de Taquin 3x3 avec A*  
**Durée :** 2 semaines (25-30h total)  
**Objectif :** Livraison coordonnée et efficace du TP1

---

## 🎯 **Organisation générale**

### **Principe de répartition**
- **Travail en parallèle** maximisé sur modules indépendants
- **Spécialisation** selon les forces de chacun
- **Leadership clair** pour coordination et intégration
- **Tests distribués** mais cohérents

### **Jalons importants**
- **Fin Semaine 1** : Modules de base terminés et testés
- **Milieu Semaine 2** : Intégration complète, tests validés
- **Fin Semaine 2** : Rapport et livrables finaux

---

## 👥 **Répartition des responsabilités**

### **PERSONNE 1 - Chef de projet / Architecte A*** 
**Rôle :** Leadership technique et intégration  
**Charge :** 8-10 heures

#### **Responsabilités principales :**
- **Coordination d'équipe** : Synchronisation et communication
- **main.pl** : Interface principale avec menu CLI 
- **search.pl** : **Algorithme A* complet** (module critique)
  - File de priorité Best-First
  - Exploration systématique de tous les successeurs
  - Étiquetage des états (A, B, C, D, E)
  - Reconstruction du chemin
  - Comptage précis des nœuds
- **Intégration finale** : Assemblage de tous les modules
- **Tests d'intégration** : Validation cas de test 1 (professeur)

#### **Livrables :**
- Interface CLI fonctionnelle avec menu
- Algorithme A* validé contre l'exemple (9 nœuds, 4 mouvements)
- Intégration complète testée

---

### **PERSONNE 2 - Spécialiste Structures de données**
**Rôle :** Fondations logiques du puzzle  
**Charge :** 6-8 heures

#### **Responsabilités principales :**
- **board.pl** : Représentation et manipulation des états
  - Structure de données [1,2,3,5,0,6,4,7,8]
  - Prédicats de base (validation, comparaison, manipulation)
  - Conversion positions ↔ coordonnées
- **moves.pl** : Génération des mouvements légaux
  - 4 directions possibles depuis case vide
  - Validation des mouvements (limites plateau)
  - Application des mouvements (échange de tuiles)
- **Tests unitaires** : Modules board.pl et moves.pl

#### **Livrables :**
- Modules board.pl et moves.pl fonctionnels
- Suite de tests validant toutes les opérations de base
- Documentation des prédicats principaux

---

### **PERSONNE 3 - Spécialiste Algorithmes/Heuristiques**
**Rôle :** Optimisation et performance  
**Charge :** 6-8 heures

#### **Responsabilités principales :**
- **heuristics.pl** : Fonctions heuristiques complètes
  - **Heuristique principale** : Tuiles mal placées (selon énoncé)
  - **Heuristique optionnelle** : Distance de Manhattan (comparaison)
  - Interface unifiée pour sélection heuristique
- **Optimisation des performances** : 
  - Algorithme A* efficace
  - Gestion mémoire des états visités
- **Benchmarks et comparaisons** :
  - Temps de réponse entre mouvements IA
  - Comparaison performance heuristiques

#### **Livrables :**
- Module heuristics.pl complet et optimisé
- Benchmarks de performance documentés
- Recommandations d'optimisation

---

### **PERSONNE 4 - Spécialiste Interface/Tests/Documentation**
**Rôle :** Qualité et expérience utilisateur  
**Charge :** 6-8 heures

#### **Responsabilités principales :**
- **utils.pl** : Utilitaires d'affichage et formatage
  - Affichage plateau 3x3 lisible
  - Formatage des résultats (Path/Cost/Expanded)
  - Temps de réponse IA affiché
  - Messages d'erreur clairs
- **tests.pl** : Suite de tests complète
  - Tests unitaires de tous les modules
  - Cas de test 1 (professeur) et cas de test 2 (personnalisé)
  - Tests de robustesse (états impossibles)
- **Documentation et rapport final** :
  - Guide d'utilisation
  - Documentation code
  - Rapport selon template professeur

#### **Livrables :**
- Interface utilisateur claire et professionnelle
- Suite de tests complète (100% des modules)
- Documentation et rapport final

---

## 📅 **Timeline détaillée**

### **Semaine 1 : Développement des modules**

| Jour | Personne 1 | Personne 2 | Personne 3 | Personne 4 |
|------|------------|-------------|-------------|-------------|
| **Lun-Mar** | Coordination + main.pl base | board.pl | heuristics.pl base | utils.pl base |
| **Mer-Jeu** | search.pl (A* core) | moves.pl | Heuristiques complètes | tests.pl structure |
| **Ven** | Tests search.pl | Tests modules | Optimisation | Tests intégrés |

### **Semaine 2 : Intégration et finalisation**

| Jour | Personne 1 | Personne 2 | Personne 3 | Personne 4 |
|------|------------|-------------|-------------|-------------|
| **Lun** | Intégration modules | Support intégration | Benchmarks | Tests validation |
| **Mar** | Tests cas professeur | Corrections | Performance | Guide utilisation |
| **Mer-Jeu** | Validation finale | Tests robustesse | Docs techniques | Rapport final |

---

## 🔄 **Coordination et communication**

### **Réunions quotidiennes (15 min)**
- **Objectif** : Synchronisation et résolution blocages
- **Format** : Chacun annonce : fait hier, plan aujourd'hui, obstacles

### **Points de synchronisation critiques**
1. **Jour 3** : Modules de base fonctionnels
2. **Jour 7** : Intégration première version
3. **Jour 10** : Tests complets validés
4. **Jour 14** : Livraison finale

### **Gestion des dépendances**
- **board.pl → moves.pl** : Personne 2 coordonne
- **modules base → search.pl** : Personne 1 attend validation P2
- **tous modules → tests.pl** : Personne 4 teste au fur et à mesure
- **search.pl → utils.pl** : Coordination P1-P4 pour affichage

---

## ⚠️ **Gestion des risques**

### **Risques identifiés et mitigation**

| Risque | Probabilité | Impact | Mitigation |
|--------|-------------|--------|------------|
| Retard sur search.pl (critique) | Moyen | Élevé | P2 et P3 supportent P1 si nécessaire |
| Incompatibilité modules | Faible | Élevé | Tests d'intégration quotidiens |
| Complexité A* sous-estimée | Moyen | Élevé | Buffer temps P1, support équipe |
| Tests incomplets | Faible | Moyen | P4 commence tests dès modules disponibles |

### **Plan de contingence**
- **Si retard critique** : Focus équipe sur module bloquant
- **Si problème technique** : Session debug collective
- **Si absence membre** : Réassignation dynamique des tâches

---

## 📋 **Critères de validation par module**

### **board.pl (Personne 2)**
- ✅ États valides reconnus correctement
- ✅ Mouvements générés sans erreur
- ✅ Conversions position ↔ coordonnées exactes

### **moves.pl (Personne 2)** 
- ✅ 4 mouvements générés pour case vide centre
- ✅ 2-3 mouvements pour cases vide bords/coins
- ✅ Aucun mouvement invalide généré

### **heuristics.pl (Personne 3)**
- ✅ Heuristique tuiles mal placées = 5 pour état initial test 1
- ✅ Heuristique = 0 pour état but
- ✅ Performance correcte (< 1ms par calcul)

### **search.pl (Personne 1)**
- ✅ **Test critique** : Cas professeur donne exactement 9 nœuds, 4 mouvements
- ✅ Reconstruction chemin A→B→C→D→E
- ✅ Interface CLI fonctionnelle

### **utils.pl (Personne 4)**
- ✅ Affichage plateau 3x3 lisible
- ✅ Temps de réponse IA affiché
- ✅ Messages d'erreur clairs

### **tests.pl (Personne 4)**
- ✅ Tous les tests unitaires passent
- ✅ Cas de test 1 validé (résultats exacts)
- ✅ Cas de test 2 fonctionnel

---

## 🎯 **Remise finale**

### **Livrables attendus**
1. **Code source** : Un fichier .PL fonctionnel
2. **Rapport PDF** : Selon template professeur
3. **Documentation** : Guide d'utilisation intégré

### **Responsabilités finales**
- **Personne 1** : Assemblage code final (.PL)
- **Personne 4** : Rapport et documentation
- **Tous** : Relecture croisée et validation

### **Validation avant remise**
- ✅ Test 1 professeur : résultats exacts
- ✅ Test 2 personnalisé : fonctionnel
- ✅ Interface CLI complète
- ✅ Aucune erreur compilation/exécution
- ✅ Rapport complet et soigné

---

## 📞 **Contacts et support**

### **Escalation des problèmes**
1. **Technique** → Personne 1 (leader)
2. **Intégration** → Session collective
3. **Délais** → Réassignation dynamique
4. **Qualité** → Personne 4 (tests)

### **Communication**
- **Urgent** : Contact direct
- **Routine** : Réunions quotidiennes
- **Documentation** : Partagée en temps réel

---

**🚀 Objectif équipe : Livrer un solveur de taquin parfaitement fonctionnel, respectant toutes les exigences du TP, dans les délais impartis !**