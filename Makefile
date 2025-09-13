# Makefile pour le projet Solveur de Taquin
# Cours: IFT-2003 Intelligence Artificielle

# Variables
SRC_DIR = src
TEST_DIR = tests  
DOCS_DIR = docs
ARCHIVE_DIR = archive

# Fichier principal
MAIN_FILE = $(SRC_DIR)/main.pl
TEST_FILE = $(TEST_DIR)/tests.pl

# Commandes par défaut
.PHONY: all run test clean docs help

all: help

# Lancer le programme principal  
run:
	@echo "Lancement du solveur de taquin..."
	@swipl -q -t main -s $(MAIN_FILE)

# Exécuter les tests
test:
	@echo "Exécution des tests unitaires..."
	@swipl -q -t run_all_tests -s $(TEST_FILE)

# Test cas spécifique du professeur
test-prof:
	@echo "Test du cas du professeur..."
	@swipl -q -t test_case_1 -s $(TEST_FILE)

# Test cas personnalisé
test-custom:
	@echo "Test du cas personnalisé..."
	@swipl -q -t test_case_2 -s $(TEST_FILE)

# Nettoyer les fichiers temporaires
clean:
	@echo "Nettoyage des fichiers temporaires..."
	@find . -name "*.tmp" -delete
	@find . -name "*~" -delete

# Générer la documentation
docs:
	@echo "La documentation se trouve dans $(DOCS_DIR)/"
	@echo "- Plan_Developpement.md : Plan technique détaillé"
	@echo "- README.md : Guide d'utilisation"
	@echo "- Repartition_Equipe.md : Organisation équipe"

# Vérifier la structure du projet
check:
	@echo "Vérification de la structure du projet..."
	@echo "Fichiers sources:"
	@ls -la $(SRC_DIR)/
	@echo "\nFichiers de test:"
	@ls -la $(TEST_DIR)/
	@echo "\nDocumentation:"
	@ls -la $(DOCS_DIR)/

# Aide
help:
	@echo "=== Solveur de Taquin 3x3 avec A* ==="
	@echo ""
	@echo "Commandes disponibles:"
	@echo "  make run        - Lancer le programme principal"
	@echo "  make test       - Exécuter tous les tests"
	@echo "  make test-prof  - Test cas du professeur"
	@echo "  make test-custom- Test cas personnalisé"
	@echo "  make check      - Vérifier structure projet"
	@echo "  make docs       - Afficher info documentation"
	@echo "  make clean      - Nettoyer fichiers temporaires"
	@echo "  make help       - Afficher cette aide"
	@echo ""
	@echo "Prérequis: SWI-Prolog installé"