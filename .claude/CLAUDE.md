# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Context

This is a sliding puzzle solver project for IFT-2003 (Artificial Intelligence) course implementing A* algorithm in Prolog. The project uses a 4-module architecture optimized for academic evaluation.

## ⚠️ CRITICAL RESTRICTIONS

**NEVER** modify, move, or update the file `archive/TP1_Enonce_Reformule.md` - this contains the original assignment requirements and evaluation criteria that must remain COMPLETELY unchanged and untouched.

## Project Architecture

The project follows a 4-module structure:

```
src/
├── main.pl      ~60 lines   (CLI interface, orchestration, test cases)
├── game.pl      ~100 lines  (Sliding puzzle states, moves, validation) 
├── astar.pl     ~150 lines  (A* algorithm, heuristics, search core)
├── display.pl   ~50 lines   (Formatted output, user interface)
└── tests.pl     ~80 lines   (Unit tests, integration validation)
```

## Development Commands

- **Run main program**: `swipl -g main src/main.pl`
- **Run tests**: `swipl -g run_all_tests src/tests.pl`
- **Interactive testing**: `swipl src/main.pl` then `?- main_menu.`

## Key Requirements

- **Algorithm focus**: A* implementation is the core evaluation criteria
- **Exact validation**: Case 1 must produce Cost=4, Expanded=9, Path length=5
- **Heuristic**: Primary = misplaced tiles (excluding blank), Optional = Manhattan distance
- **Results format**: Path/Cost/Expanded with AI response time

## Language Conventions

**Code**: English only
- Variable names, function names, predicate names: English
- Code structure and logic: English naming
- Examples: `current_state`, `generate_moves`, `heuristic_value`

**Documentation & Communication**: French
- Comments in code: French
- Git commit messages: French  
- Documentation files (.md): French
- User interface text: French
- Error messages: French

**Professional Language Policy**: 
- NEVER use academic references (TP1, assignments, homework, etc.) in code or user-facing text
- NEVER use development status words (beta, draft, simplified, improved, etc.) in final outputs
- Keep all user interfaces and documentation clean and professional
- Focus on functionality, not development process

## Team Structure

4-person team with module assignment:
- Dev 1: main.pl (leadership + integration)
- Dev 2: astar.pl (core algorithm responsibility)
- Dev 3: game.pl (domain logic foundations)
- Dev 4: display.pl + tests.pl (UX + quality assurance)

## Git Conventions

**Commit message format**:
```
type(module): description courte
```

**Types**:
- `feat` : nouvelle fonctionnalité
- `fix` : correction de bug
- `test` : ajout ou modification de tests
- `docs` : mise à jour documentation
- `refactor` : refactoring sans changement fonctionnel

**Exemples**:
```
feat(astar): implémentation heuristique tuiles mal placées
fix(game): correction génération mouvements valides
test(display): ajout tests formatage grille 3x3
docs: mise à jour spécifications techniques
```