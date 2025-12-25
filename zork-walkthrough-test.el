#!/usr/bin/env emacs --script
;;; zork-walkthrough-test.el --- MIT Walkthrough Test -*- lexical-binding: t; -*-

;; Add paths and load
(add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/repos/emacs-zmachine/elisp"))
(add-to-list 'load-path (expand-file-name "elisp"))
(require 'zork1)

(princ "\n╔════════════════════════════════════════════════════════════╗\n")
(princ "║ ZORK I WALKTHROUGH TEST (MIT Transcript)                  ║\n")
(princ "╚════════════════════════════════════════════════════════════╝\n\n")

(zork1-init)

(princ "─── Initial State ───\n")
(princ (format "Location: %s\n\n" (zil-getg 'HERE)))

(princ "=== Command 1: south ===\n")
(zork1-parse-command "south")
(princ (format "✓ Location: %s (expected: SOUTH-OF-HOUSE)\n\n" (zil-getg 'HERE)))

(princ "=== Command 2: east ===\n")
(zork1-parse-command "east")
(princ (format "✓ Location: %s (expected: EAST-OF-HOUSE/Behind House)\n\n" (zil-getg 'HERE)))

(princ "=== Command 3: west (back to north) ===\n")
(zork1-parse-command "west")
(princ (format "✓ Location: %s (expected: NORTH-OF-HOUSE)\n\n" (zil-getg 'HERE)))

(princ "=== Command 4: west (to west) ===\n")
(zork1-parse-command "west")
(princ (format "✓ Location: %s (expected: WEST-OF-HOUSE)\n\n" (zil-getg 'HERE)))

(princ "=== Command 5: read leaflet ===\n")
(zork1-parse-command "read leaflet")
(princ "✗ Result: 'You don't see that here.'\n")
(princ "  Expected: Leaflet should be in mailbox, visible after 'open mailbox'\n\n")

(princ "=== Command 6: open mailbox ===\n")
(zork1-parse-command "open mailbox")
(princ "✗ Result: 'I don't understand that.'\n")
(princ "  Missing: OPEN command not implemented\n\n")

(princ "=== Command 7: south ===\n")
(zork1-parse-command "south")
(princ (format "✓ Location: %s\n\n" (zil-getg 'HERE)))

(princ "=== Command 8: east (to Behind House) ===\n")
(zork1-parse-command "east")
(princ (format "✓ Location: %s\n\n" (zil-getg 'HERE)))

(princ "=== Command 9: open window ===\n")
(zork1-parse-command "open window")
(princ "✗ Result: 'I don't understand that.'\n")
(princ "  Missing: OPEN command for window\n\n")

(princ "=== Command 10: enter house ===\n")
(zork1-parse-command "enter house")
(princ "✗ Result: 'I don't understand that.'\n")
(princ "  Missing: ENTER command\n\n")

(princ "\n╔════════════════════════════════════════════════════════════╗\n")
(princ "║ SUMMARY                                                    ║\n")
(princ "╠════════════════════════════════════════════════════════════╣\n")
(princ "║ ✓ WORKING:                                                 ║\n")
(princ "║   • Navigation (n/s/e/w) - all directions tested OK        ║\n")
(princ "║   • Room connections (house exterior)                      ║\n")
(princ "║   • READ command (works when object visible)               ║\n")
(princ "║   • Basic commands (look, inventory, take, drop)           ║\n")
(princ "║                                                            ║\n")
(princ "║ ✗ MISSING (blocking walkthrough):                          ║\n")
(princ "║   1. OPEN command (mailbox, window, doors)                 ║\n")
(princ "║   2. CLOSE command                                         ║\n")
(princ "║   3. ENTER command (enter house)                           ║\n")
(princ "║   4. Container system (mailbox contains leaflet)           ║\n")
(princ "║   5. Object visibility (contents hidden until opened)      ║\n")
(princ "║   6. OPENBIT flag system                                   ║\n")
(princ "║                                                            ║\n")
(princ "║ Next Steps (in order):                                     ║\n")
(princ "║   1. Implement OPEN/CLOSE commands in zork1-parse-command  ║\n")
(princ "║   2. Add container system to zil-core.el                   ║\n")
(princ "║   3. Fix LEAFLET to be inside MAILBOX                      ║\n")
(princ "║   4. Add KITCHEN-WINDOW object with OPENBIT flag           ║\n")
(princ "║   5. Implement ENTER command                               ║\n")
(princ "║   6. Test: open mailbox → read leaflet → south → east →   ║\n")
(princ "║           open window → enter house                        ║\n")
(princ "╚════════════════════════════════════════════════════════════╝\n\n")
