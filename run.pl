/** <module> Launcher avec configuration automatique UTF-8

Launcher universel qui configure automatiquement l'encodage UTF-8 selon
le système d'exploitation et lance le solveur de taquin A*.

@author Équipe 6 - IFT-2003
@see main.pl pour le programme principal

# Utilisation

==
swipl run.pl
==

# Fonctionnalités

- Configuration automatique UTF-8 pour Windows/Mac/Linux
- Détection automatique du système d'exploitation
- Configuration des streams d'entrée/sortie
- Lancement transparent du programme principal
- Gestion d'erreurs avec messages informatifs
- Support complet des caractères ASCII étendus

# Compatibilité

- Windows 10/11 (PowerShell, cmd, VS Code Terminal)
- macOS (Terminal.app, iTerm2)
- Linux (bash, zsh, gnome-terminal)
*/

% Point d'entrée automatique avec gestion d'erreurs
:- initialization(start, main).

%! start is det.
%  Point d'entrée principal du launcher
%  Configure l'environnement et lance le programme
start :-
    nl,
    write('>>> Configuration automatique UTF-8...'), nl,
    setup_utf8,
    write('>>> Lancement du solveur de taquin...'), nl, nl,

    % Charger et lancer le programme principal
    catch(
        (consult('src/main.pl'), main),
        Error,
        (   Error = unwind(halt(_)) ->
            true  % Sortie normale, ignorer silencieusement
        ;   handle_launcher_error(Error)
        )
    ).

%! setup_utf8 is det.
%  Configure l'encodage UTF-8 selon le système d'exploitation
%  Détecte automatiquement Windows vs Unix et applique la config appropriée
setup_utf8 :-
    % Configuration spécifique à Windows
    (   current_prolog_flag(windows, true)
    ->  setup_windows_utf8
    ;   setup_unix_utf8
    ).

%! setup_windows_utf8 is det.
%  Configuration UTF-8 spécifique pour Windows
%  Configure PowerShell, cmd et les streams SWI-Prolog
setup_windows_utf8 :-
    % Configuration SWI-Prolog pour UTF-8 (simple et sûr)
    catch(set_prolog_flag(encoding, utf8), _, true),

    % Configuration des streams d'entrée/sortie
    catch(set_stream(user_output, encoding(utf8)), _, true),
    catch(set_stream(user_input, encoding(utf8)), _, true),
    catch(set_stream(user_error, encoding(utf8)), _, true),

    write('    [OK] Windows UTF-8 configure'), nl.

%! setup_unix_utf8 is det.
%  Configuration UTF-8 pour Mac/Linux
%  La plupart des systèmes Unix sont déjà en UTF-8 par défaut
setup_unix_utf8 :-
    % Configuration SWI-Prolog pour UTF-8 (sécurité)
    set_prolog_flag(encoding, utf8),

    % Vérifier les variables d'environnement locale
    catch(
        (getenv('LANG', Lang),
         (sub_atom(Lang, _, _, _, 'UTF-8') ->
             write('    [OK] Unix UTF-8 detecte')
         ;
             write('    [WARN] Locale non-UTF-8, forcage UTF-8'))
        ),
        _,
        write('    [OK] Unix UTF-8 configure par defaut')
    ),
    nl.

%! handle_launcher_error(+Error:compound) is det.
%  Gère les erreurs du launcher avec messages informatifs
%  @param Error Structure d'erreur SWI-Prolog
handle_launcher_error(Error) :-
    nl,
    write('>>> ERREUR LAUNCHER <<<'), nl,
    write('Le launcher n\'a pas pu démarrer le programme principal.'), nl,
    nl,
    write('Erreur détaillée: '), write(Error), nl,
    nl,
    write('Solutions possibles:'), nl,
    write('1. Vérifiez que le fichier src/main.pl existe'), nl,
    write('2. Vérifiez que SWI-Prolog est correctement installé'), nl,
    write('3. Essayez de lancer directement: swipl -g main src/main.pl'), nl,
    nl,
    halt(1).