# Zork for Emacs

Die komplette Zork-Trilogie (Zork I, II, III) als native Emacs Lisp Implementation.

## ✅ Status: VOLLSTÄNDIG IMPLEMENTIERT

Alle drei Spiele sind jetzt spielbar!

## Features

- **Drei komplette Spiele**: Zork I, II und III ✅
- **Native Emacs Integration**: Nutzt `emacs-zmachine` ZIL-Engine ✅  
- **Original Infocom Content**: Basierend auf ZIL-Quellcode ✅
- **GPL-3.0 kompatibel**: MIT-lizenzierte Infocom-Quellen ✅

## Installation

### Manuell

```elisp
(add-to-list 'load-path "~/<PATH TO>/emacs-zmachine/elisp")
(add-to-list 'load-path "~/<PATH TO>/zork-emacs/elisp")
(require 'zork-emacs)
```

### Packaging (ELPA/MELPA)

- Dieses Paket liefert ausschließlich die Emacs Lisp-Spielmodule unter `elisp/`.
- Historische ZIL-Quellen liegen optional unter `zork-source/` und sind nicht für die Installation erforderlich.
- Für Paket-Builds werden `zork-source/`, `docs/`, `tests/`, `examples/` via `.elpaignore` ausgeschlossen.
- Abhängigkeit: `emacs-zmachine` ≥ 0.2.0.

## Usage

```
M-x zork-play-game  ; Spiel auswählen
M-x zork-i          ; Zork I direkt
M-x zork-ii         ; Zork II direkt
M-x zork-iii        ; Zork III direkt
```

## Spielbefehle

```
Movement: n, s, e, w, up, down, in, out
Actions:  look, take, drop, inventory, examine
          open, close, attack, read
Special:  quit
```

## Lizenz

GPL-3.0-or-later  
Original: MIT License (Infocom)

Siehe IMPLEMENTATION_SUMMARY.md für Details.

## Historische Quellen (optional)

Die Original-ZIL-Quellen (Infocom, MIT-Lizenz) sind als optionale Referenz in `zork-source/` verfügbar und werden nicht mit dem ELPA/MELPA-Paket ausgeliefert. Für reine Nutzung innerhalb von Emacs ist der Elisp-Port ausreichend.

Optional kannst du die Quellen als Submodule pflegen:

```bash
git submodule add https://github.com/historicalsource/zork1 zork-source/zork1
git submodule add https://github.com/historicalsource/zork2 zork-source/zork2
git submodule add https://github.com/historicalsource/zork3 zork-source/zork3
```
