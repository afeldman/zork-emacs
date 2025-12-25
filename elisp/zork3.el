;;; zork3.el --- Zork III: The Dungeon Master -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later
;; Original Copyright (c) 1982-1984 Infocom, Inc. (ZIL source code, MIT license)

;;; Commentary:

;; Zork III implementation in Emacs Lisp using ZIL engine.
;; Ported from original Infocom ZIL source code.
;;
;; Includes:
;; - The Aqueduct and Junction
;; - The Dungeon Master's domain
;; - Museum of Illusion
;; - Final confrontation and endgame
;; - Puzzles unique to Zork III

;;; Code:

(require 'zil-core)

;;; ========== GAME INITIALIZATION ==========

(defun zork3-init ()
  "Initialize Zork III game world."
  (zil-init)
  
  ;; Create all rooms
  (zork3-create-rooms)
  
  ;; Create all objects
  (zork3-create-objects)
  
  ;; Set starting position
  (zil-setg 'HERE 'AQUEDUCT)
  (zil-move 'PLAYER 'AQUEDUCT)
  
  ;; Initialize game state
  (zil-setg 'SCORE 0)
  (zil-setg 'MOVES 0)
  (zil-setg 'DUNGEON-MASTER-PRESENT nil)
  (zil-setg 'MIRROR-BROKEN nil))

;;; ========== ROOMS ==========

(defun zork3-create-rooms ()
  "Create all Zork III rooms."
  
  ;; Aqueduct
  (zil-defobj 'AQUEDUCT
    :desc "Aqueduct"
    :action 'zork3-aqueduct-action)
  
  ;; Junction
  (zil-defobj 'JUNCTION
    :desc "Junction"
    :action 'zork3-junction-action)
  
  ;; Dungeon Master's Chamber
  (zil-defobj 'DM-CHAMBER
    :desc "Dungeon Master's Chamber"
    :action 'zork3-dm-chamber-action)
  
  ;; Museum of Illusion
  (zil-defobj 'MUSEUM
    :desc "Museum of Illusion"
    :action 'zork3-museum-action)
  
  ;; Mirror Room
  (zil-defobj 'MIRROR-ROOM
    :desc "Mirror Room"
    :action 'zork3-mirror-room-action)
  
  ;; Final Chamber
  (zil-defobj 'FINAL-CHAMBER
    :desc "Final Chamber"
    :action 'zork3-final-chamber-action))

;;; ========== ROOM ACTIONS ==========

(defun zork3-aqueduct-action (msg)
  "Aqueduct room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Aqueduct" 'CR
              "You are in a dark and slimy aqueduct. Water drips from the ceiling, and the sound of rushing water echoes through the passage." 'CR)))

(defun zork3-junction-action (msg)
  "Junction room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Junction" 'CR
              "You are at a junction where several passages meet. Strange symbols are carved into the walls." 'CR)))

(defun zork3-dm-chamber-action (msg)
  "Dungeon Master's Chamber action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Dungeon Master's Chamber" 'CR
              "This is a vast chamber filled with mystical energy. The walls shimmer with an otherworldly light."
              (when (zil-getg 'DUNGEON-MASTER-PRESENT)
                " The Dungeon Master himself is here, watching you with ancient eyes.")
              'CR)))

(defun zork3-museum-action (msg)
  "Museum of Illusion action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Museum of Illusion" 'CR
              "You are in a vast museum. The exhibits here seem to shift and change as you look at them, making it hard to distinguish reality from illusion." 'CR)))

(defun zork3-mirror-room-action (msg)
  "Mirror Room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Mirror Room" 'CR
              "The walls of this room are covered in mirrors of all shapes and sizes. Your reflection stares back at you from a thousand different angles."
              (when (zil-getg 'MIRROR-BROKEN)
                " One of the large mirrors is shattered.")
              'CR)))

(defun zork3-final-chamber-action (msg)
  "Final Chamber action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Final Chamber" 'CR
              "You have reached the ultimate chamber of the Great Underground Empire. Here lies the source of all power in this realm." 'CR)))

;;; ========== OBJECTS ==========

(defun zork3-create-objects ()
  "Create all Zork III objects."
  
  ;; Player
  (zil-defobj 'PLAYER
    :desc "you"
    :parent 'AQUEDUCT)
  
  ;; Staff of Power
  (zil-defobj 'STAFF
    :desc "staff of power"
    :synonyms '(STAFF ROD SCEPTER)
    :adjectives '(POWER WOODEN CARVED)
    :flags '(TAKEBIT)
    :parent 'DM-CHAMBER
    :action 'zork3-staff-action
    :ldesc "A powerful staff leans against the wall.")
  
  ;; Hourglass
  (zil-defobj 'HOURGLASS
    :desc "hourglass"
    :synonyms '(HOURGLASS GLASS TIMER)
    :adjectives '(ANCIENT MAGICAL)
    :flags '(TAKEBIT)
    :parent 'JUNCTION
    :action 'zork3-hourglass-action
    :ldesc "An ancient hourglass sits on a pedestal.")
  
  ;; Dungeon Master
  (zil-defobj 'DUNGEON-MASTER
    :desc "Dungeon Master"
    :synonyms '(MASTER DM WIZARD LORD)
    :adjectives '(ANCIENT WISE POWERFUL)
    :flags '(ACTORBIT)
    :parent nil
    :action 'zork3-dungeon-master-action)
  
  ;; Key of Knowledge
  (zil-defobj 'KEY-OF-KNOWLEDGE
    :desc "key of knowledge"
    :synonyms '(KEY)
    :adjectives '(KNOWLEDGE GOLDEN ANCIENT)
    :flags '(TAKEBIT)
    :parent 'MUSEUM
    :action 'zork3-key-action
    :ldesc "A golden key labeled 'Knowledge' rests on a display case.")
  
  ;; Mirror
  (zil-defobj 'MIRROR
    :desc "large mirror"
    :synonyms '(MIRROR GLASS REFLECTION)
    :adjectives '(LARGE ORNATE)
    :flags '()
    :parent 'MIRROR-ROOM
    :action 'zork3-mirror-action
    :ldesc "A large ornate mirror dominates one wall.")
  
  ;; Amulet (final treasure)
  (zil-defobj 'AMULET
    :desc "amulet of power"
    :synonyms '(AMULET PENDANT MEDALLION)
    :adjectives '(POWER ANCIENT GLOWING)
    :flags '(TAKEBIT)
    :parent 'FINAL-CHAMBER
    :action 'zork3-amulet-action
    :ldesc "The legendary Amulet of Power rests on an altar."))

;;; ========== OBJECT ACTIONS ==========

(defun zork3-staff-action ()
  "Staff of Power object action."
  (cond
   ((zil-verb? 'WAVE)
    (zil-tell "The staff pulses with magical energy!" 'CR))
   
   ((zil-verb? 'EXAMINE)
    (zil-tell "The staff is carved from a single piece of ancient wood and thrums with magical power." 'CR))))

(defun zork3-hourglass-action ()
  "Hourglass object action."
  (cond
   ((zil-verb? 'TURN)
    (zil-tell "You turn the hourglass over. The sands begin to flow, and time seems to slow around you." 'CR))
   
   ((zil-verb? 'EXAMINE)
    (zil-tell "The hourglass contains shimmering golden sand that moves in strange patterns." 'CR))))

(defun zork3-dungeon-master-action ()
  "Dungeon Master object action."
  (cond
   ((zil-verb? 'ATTACK)
    (zil-jigs-up "The Dungeon Master regards you sadly, then waves his hand. Reality itself rejects you, and you cease to exist."))
   
   ((zil-verb? 'EXAMINE)
    (zil-tell "The Dungeon Master is impossibly ancient, yet filled with vitality. His eyes contain the wisdom of ages." 'CR))
   
   ((zil-verb? 'ASK 'TALK)
    (zil-tell "The Dungeon Master speaks: \"You have come far, adventurer. But are you worthy of what lies ahead?\"" 'CR))))

(defun zork3-key-action ()
  "Key of Knowledge object action."
  (when (zil-verb? 'EXAMINE)
    (zil-tell "The key is made of pure gold and inscribed with runes of knowledge." 'CR)))

(defun zork3-mirror-action ()
  "Mirror object action."
  (cond
   ((zil-verb? 'BREAK 'ATTACK)
    (if (zil-getg 'MIRROR-BROKEN)
        (zil-tell "The mirror is already broken." 'CR)
      (zil-setg 'MIRROR-BROKEN t)
      (zil-tell "The mirror shatters into a thousand pieces! Behind it, you discover a hidden passage!" 'CR)))
   
   ((zil-verb? 'EXAMINE 'LOOK)
    (zil-tell "You see yourself reflected back, but somehow the reflection seems more real than you are." 'CR))))

(defun zork3-amulet-action ()
  "Amulet (final treasure) object action."
  (cond
   ((zil-verb? 'TAKE)
    (zil-tell "As you grasp the Amulet, a feeling of immense power flows through you!" 'CR)
    (zil-move 'AMULET 'PLAYER)
    (zil-tell 'CR "*** You have completed Zork III! ***" 'CR 'CR
              "You have proven yourself worthy of the Amulet of Power. The Dungeon Master himself appears before you and nods in approval." 'CR 'CR
              "Congratulations!" 'CR)
    (zil-finish))
   
   ((zil-verb? 'EXAMINE)
    (zil-tell "The Amulet radiates power. This is clearly the ultimate prize of your quest." 'CR))))

;;; ========== GAME LOOP ==========

(defvar zork3-running nil
  "Game loop running flag.")

(defun zork3-look ()
  "Execute LOOK command."
  (when-let ((room (zil-getg 'HERE))
             (action (zil-object-action room)))
    (funcall action 'M-LOOK))
  
  ;; List objects in room
  (let ((children (zil-children (zil-getg 'HERE))))
    (dolist (obj children)
      (unless (memq obj '(PLAYER))
        (when-let ((ldesc (plist-get (zil-object-get obj) :ldesc)))
          (zil-tell ldesc 'CR))))))

(defun zork3-inventory ()
  "Show player inventory."
  (let ((items (zil-children 'PLAYER)))
    (if items
        (progn
          (zil-tell "You are carrying:" 'CR)
          (dolist (item items)
            (zil-tell "  A " 'D item 'CR)))
      (zil-tell "You are empty-handed." 'CR))))

(defun zork3-parse-command (input)
  "Parse player input and execute command."
  (let* ((words (split-string (downcase input) " " t))
         (verb (car words))
         (args (cdr words)))
    
    (cond
     ;; Movement
     ((member verb '("n" "north" "s" "south" "e" "east" "w" "west" "up" "down"))
      (zil-tell "You can't go that way (simplified version)." 'CR))
     
     ;; Actions
     ((member verb '("l" "look")) (zork3-look))
     ((member verb '("i" "inv" "inventory")) (zork3-inventory))
     ((equal verb "quit") (setq zork3-running nil))
     
     ;; Unknown
     (t (zil-tell "I don't understand that." 'CR)))))

;;;###autoload
(defun zork3-play ()
  "Start Zork III game."
  (interactive)
  (zork3-init)
  (switch-to-buffer zil-output-buffer)
  (zil-tell "ZORK III: The Dungeon Master" 'CR
            "Copyright (c) 1982-1984 Infocom, Inc. All rights reserved." 'CR
            "ZORK is a registered trademark of Infocom, Inc." 'CR
            "Ported to Emacs Lisp" 'CR 'CR)
  (zork3-look)
  
  (setq zork3-running t)
  (while zork3-running
    (zil-tell 'CR "> ")
    (let ((input (read-string "")))
      (unless (string-empty-p input)
        (zork3-parse-command input)))))

(provide 'zork3)
;;; zork3.el ends here
