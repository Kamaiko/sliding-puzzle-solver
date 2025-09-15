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
- **Exact validation**: Case 1 must produce Cost=4, Expanded=12, Path length=5, <3ms
- **Heuristic**: Primary = misplaced tiles (excluding blank), Optional = Manhattan distance
- **Results format**: Path/Cost/Expanded with AI response time

## Academic References Policy

**ALWAYS include academic references in rapport_tp1_template.md when implementing algorithms:**

- **A* Algorithm**: Hart, P. E., Nilsson, N. J., & Raphael, B. (1968). A formal basis for the heuristic determination of minimum cost paths. IEEE Transactions on Systems Science and Cybernetics, 4(2), 100-107.
- **Modern AI Textbook**: Russell, S. & Norvig, P. (2020). Artificial Intelligence: A Modern Approach. 4th Edition. Pearson.
- **Prolog for AI**: Bratko, I. (2012). Prolog Programming for Artificial Intelligence. 4th Edition. Addison-Wesley.

This maintains academic credibility and shows proper research methodology.

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

**Interface Display Policy**:
- Use ASCII characters ONLY in user interface
- NO Unicode characters, emojis, or special symbols in program output
- Stick to basic ASCII for maximum compatibility and professionalism
- Examples: use "*" instead of "⭐", use "+" instead of "✅", etc.

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

## Standards de Code

### Nommage des prédicats
```prolog
% Prédicats publics : verbe_nom
solve_puzzle/3, generate_moves/2, display_result/3, find_blank/2

% Prédicats internes : nom_descriptif
current_node/1, best_f_value/2, reconstruct_path/2, valid_position/2

% Variables : PascalCase ou snake_case cohérent
CurrentState, NextStates, FValue
current_state, next_states, f_value
```

### Documentation inline obligatoire
```prolog
%! solve_puzzle(+Initial:list, +Goal:list, -Result:compound) is det.
%  Résout le taquin avec A* depuis Initial vers Goal
%  @param Initial État de départ [1,2,3,5,0,6,4,7,8]
%  @param Goal    État objectif [1,2,3,4,5,6,7,8,0]
%  @param Result  result(Path, Cost, Expanded)
solve_puzzle(Initial, Goal, Result) :-
    % Implémentation avec commentaires français
    validate_state(Initial),  % Validation état initial
    astar_search(Initial, Goal, Path, Cost, Expanded),
    Result = result(Path, Cost, Expanded).
```

### Gestion d'erreurs uniforme
```prolog
% Pattern standard pour prédicats critiques
safe_solve_puzzle(Initial, Goal, Result) :-
    validate_state(Initial),
    validate_state(Goal),
    solve_puzzle(Initial, Goal, Result).
safe_solve_puzzle(_, _, error('Configuration invalide')).
```

### Format de sortie standardisé

**Résultats A*** :
```
Path: A→B→C→D→E
Cost: 4
Expanded: 9
Temps: 0.042s
```

**Messages d'erreur français** :
```prolog
error_message(invalid_state, 'Configuration de taquin invalide').
error_message(unsolvable, 'Configuration impossible à résoudre').
error_message(timeout, 'Temps d\'exécution dépassé (>10s)').
```

### Limites et contraintes

- **Timeout maximum** : 10 secondes par résolution
- **États impossibles** : Retourner `error('unsolvable')`
- **Validation entrées** : Tous les états doivent être validés avant traitement
- **Format temps** : Secondes avec 3 décimales (`0.042s`)

### Tests obligatoires

Chaque module DOIT avoir ses tests dans `tests.pl` :
```prolog
% Tests critiques obligatoires
test_heuristic_exact :-      % h([1,2,3,5,0,6,4,7,8]) = 4
test_case_1_validation :-    % Cost=4, Expanded=9 exact
test_find_blank :-           % Position case vide
test_generate_moves :-       % 4 directions valides
```