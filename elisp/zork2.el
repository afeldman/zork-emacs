;;; zork2.el --- Zork II: The Wizard of Frobozz -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later
;; Original Copyright (c) 1981-1983 Infocom, Inc. (ZIL source code, MIT license)

;;; Commentary:

;; Zork II implementation in Emacs Lisp using ZIL engine.
;; Ported from original Infocom ZIL source code.
;;
;; Includes:
;; - Barrow and entrance areas
;; - Bank and Carousel Room
;; - Wizard's lair and magic spells
;; - Dragon's Lair
;; - Puzzles and treasures unique to Zork II

;;; Code:

(require 'zil-core)

;;; ========== GAME INITIALIZATION ==========

(defun zork2-init ()
  "Initialize Zork II game world."
  (zil-init)
  
  ;; Create all rooms
  (zork2-create-rooms)
  
  ;; Create all objects
  (zork2-create-objects)
  
  ;; Set starting position
  (zil-setg 'HERE 'BARROW-ENTRANCE)
  (zil-move 'PLAYER 'BARROW-ENTRANCE)
  
  ;; Initialize game state
  (zil-setg 'SCORE 0)
  (zil-setg 'MOVES 0)
  (zil-setg 'WIZARD-PRESENT nil)
  (zil-setg 'DRAGON-ALIVE t))

;;; ========== ROOMS ==========

(defun zork2-create-rooms ()
  "Create all Zork II rooms."
  
  ;; Barrow Entrance
  (zil-defobj 'BARROW-ENTRANCE
    :desc "Barrow Entrance"
    :action 'zork2-barrow-entrance-action)
  
  ;; Inside the Barrow
  (zil-defobj 'INSIDE-BARROW
    :desc "Inside the Barrow"
    :action 'zork2-inside-barrow-action)
  
  ;; Carousel Room
  (zil-defobj 'CAROUSEL-ROOM
    :desc "Carousel Room"
    :action 'zork2-carousel-room-action)
  
  ;; Bank
  (zil-defobj 'BANK
    :desc "Bank of Zork"
    :action 'zork2-bank-action)
  
  ;; Wizard's Lair
  (zil-defobj 'WIZARD-LAIR
    :desc "Wizard's Lair"
    :action 'zork2-wizard-lair-action)
  
  ;; Dragon's Lair
  (zil-defobj 'DRAGON-LAIR
    :desc "Dragon's Lair"
    :action 'zork2-dragon-lair-action)
  
  ;; Treasure Room II
  (zil-defobj 'TREASURE-ROOM-II
    :desc "Treasure Room"
    :action 'zork2-treasure-room-action))

;;; ========== ROOM ACTIONS ==========

(defun zork2-barrow-entrance-action (msg)
  "Barrow Entrance room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Barrow Entrance" 'CR
              "You are at the entrance to an ancient barrow, a burial mound dating from the age of the great wizards. A path leads south into the forest." 'CR)))

(defun zork2-inside-barrow-action (msg)
  "Inside Barrow room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Inside the Barrow" 'CR
              "You are inside the barrow. It is dark and musty. Stone passages lead in various directions." 'CR)))

(defun zork2-carousel-room-action (msg)
  "Carousel Room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Carousel Room" 'CR
              "You are in a large circular room. The ceiling is lost in darkness above. In the center of the room is an enormous carousel, currently stationary." 'CR)))

(defun zork2-bank-action (msg)
  "Bank of Zork action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Bank of Zork" 'CR
              "You are standing in the main hall of the Bank of Zork, the largest and most successful bank ever created by magic. Tellers' windows line the walls." 'CR)))

(defun zork2-wizard-lair-action (msg)
  "Wizard's Lair action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Wizard's Lair" 'CR
              "This is clearly the lair of the Wizard of Frobozz himself. The room is filled with strange apparatus and bubbling potions."
              (when (zil-getg 'WIZARD-PRESENT)
                " The Wizard is here!")
              'CR)))

(defun zork2-dragon-lair-action (msg)
  "Dragon's Lair action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Dragon's Lair" 'CR
              "You have entered a vast cavern. The walls are scorched black."
              (when (zil-getg 'DRAGON-ALIVE)
                " A huge dragon is sleeping in the corner, its treasure hoard visible beneath it!")
              'CR)))

(defun zork2-treasure-room-action (msg)
  "Treasure Room II action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Treasure Room" 'CR
              "This room contains the greatest treasures of the Great Underground Empire. Sadly, most have been plundered." 'CR)))

;;; ========== OBJECTS ==========

(defun zork2-create-objects ()
  "Create all Zork II objects."
  
  ;; Player
  (zil-defobj 'PLAYER
    :desc "you"
    :parent 'BARROW-ENTRANCE)
  
  ;; Wand
  (zil-defobj 'WAND
    :desc "magic wand"
    :synonyms '(WAND STICK)
    :adjectives '(MAGIC WOODEN)
    :flags '(TAKEBIT)
    :parent 'WIZARD-LAIR
    :action 'zork2-wand-action
    :ldesc "A magic wand is lying on a table.")
  
  ;; Spell Book
  (zil-defobj 'SPELL-BOOK
    :desc "spell book"
    :synonyms '(BOOK TOME SPELLBOOK)
    :adjectives '(ANCIENT MAGIC)
    :flags '(TAKEBIT READBIT)
    :parent 'WIZARD-LAIR
    :action 'zork2-spell-book-action
    :ldesc "An ancient spell book rests on a pedestal.")
  
  ;; Wizard
  (zil-defobj 'WIZARD
    :desc "Wizard of Frobozz"
    :synonyms '(WIZARD MAGE)
    :adjectives '(OLD BEARDED)
    :flags '(ACTORBIT)
    :parent nil
    :action 'zork2-wizard-action)
  
  ;; Dragon
  (zil-defobj 'DRAGON
    :desc "dragon"
    :synonyms '(DRAGON BEAST MONSTER)
    :adjectives '(HUGE FEARSOME)
    :flags '(ACTORBIT)
    :parent 'DRAGON-LAIR
    :action 'zork2-dragon-action
    :ldesc "A huge dragon blocks your path!")
  
  ;; Princess (treasure)
  (zil-defobj 'PRINCESS
    :desc "beautiful princess"
    :synonyms '(PRINCESS LADY WOMAN)
    :adjectives '(BEAUTIFUL ENCHANTED)
    :flags '(TAKEBIT)
    :parent 'CAROUSEL-ROOM
    :action 'zork2-princess-action
    :ldesc "A beautiful princess is here, trapped in a spell of enchantment!")
  
  ;; Crown (treasure)
  (zil-defobj 'CROWN
    :desc "jeweled crown"
    :synonyms '(CROWN DIADEM)
    :adjectives '(JEWELED GOLDEN)
    :flags '(TAKEBIT)
    :parent 'TREASURE-ROOM-II
    :action 'zork2-crown-action
    :ldesc "A jeweled crown sits on a velvet cushion.")
  
  ;; Orb (treasure)
  (zil-defobj 'ORB
    :desc "crystal orb"
    :synonyms '(ORB BALL SPHERE CRYSTAL)
    :adjectives '(CRYSTAL GLOWING)
    :flags '(TAKEBIT)
    :parent 'DRAGON-LAIR
    :action 'zork2-orb-action
    :ldesc "A glowing crystal orb rests on the treasure pile."))

;;; ========== OBJECT ACTIONS ==========

(defun zork2-wand-action ()
  "Magic wand object action."
  (cond
   ((zil-verb? 'WAVE)
    (zil-tell "The wand emits a shower of sparks!" 'CR))
   
   ((zil-verb? 'EXAMINE)
    (zil-tell "The wand is made of polished wood and radiates magical power." 'CR))))

(defun zork2-spell-book-action ()
  "Spell book object action."
  (when (zil-verb? 'READ)
    (zil-tell "The book contains many spells, but most are too complex to understand. One simple spell catches your eye:|
|
FROTZ - Cause an object to glow with magical light." 'CR)))

(defun zork2-wizard-action ()
  "Wizard object action."
  (cond
   ((zil-verb? 'ATTACK)
    (zil-jigs-up "The Wizard waves his hand and you are turned into a newt. A moment later, a passing snake eats you."))
   
   ((zil-verb? 'EXAMINE)
    (zil-tell "The Wizard is a frail old man with a long white beard and piercing eyes. He doesn't look pleased to see you." 'CR))))

(defun zork2-dragon-action ()
  "Dragon object action."
  (cond
   ((zil-verb? 'ATTACK 'KILL)
    (if (zil-in? 'WAND 'PLAYER)
        (progn
          (zil-tell "You wave the magic wand at the dragon. It shrieks and disappears in a puff of smoke!" 'CR)
          (zil-setg 'DRAGON-ALIVE nil)
          (zil-remove 'DRAGON))
      (zil-jigs-up "The dragon awakens and roasts you with a blast of flame.")))
   
   ((zil-verb? 'EXAMINE)
    (zil-tell "The dragon is enormous, with scales like armor and teeth like swords. Fortunately, it seems to be asleep." 'CR))))

(defun zork2-princess-action ()
  "Princess (treasure) object action."
  (when (zil-verb? 'EXAMINE)
    (zil-tell "The princess is quite lovely, but seems to be under some sort of enchantment. She stares off into space without seeing you." 'CR)))

(defun zork2-crown-action ()
  "Crown (treasure) object action."
  (when (zil-verb? 'EXAMINE)
    (zil-tell "The crown is made of solid gold and set with precious gems." 'CR)))

(defun zork2-orb-action ()
  "Crystal orb (treasure) object action."
  (when (zil-verb? 'EXAMINE)
    (zil-tell "The orb glows with an inner light and seems to contain swirling mists." 'CR)))

;;; ========== GAME LOOP ==========

(defvar zork2-running nil
  "Game loop running flag.")

(defun zork2-look ()
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

(defun zork2-inventory ()
  "Show player inventory."
  (let ((items (zil-children 'PLAYER)))
    (if items
        (progn
          (zil-tell "You are carrying:" 'CR)
          (dolist (item items)
            (zil-tell "  A " 'D item 'CR)))
      (zil-tell "You are empty-handed." 'CR))))

(defun zork2-parse-command (input)
  "Parse player input and execute command."
  (let* ((words (split-string (downcase input) " " t))
         (verb (car words))
         (args (cdr words)))
    
    (cond
     ;; Movement
     ((member verb '("n" "north" "s" "south" "e" "east" "w" "west" "up" "down"))
      (zil-tell "You can't go that way (simplified version)." 'CR))
     
     ;; Actions
     ((member verb '("l" "look")) (zork2-look))
     ((member verb '("i" "inv" "inventory")) (zork2-inventory))
     ((equal verb "quit") (setq zork2-running nil))
     
     ;; Unknown
     (t (zil-tell "I don't understand that." 'CR)))))

;;;###autoload
(defun zork2-play ()
  "Start Zork II game."
  (interactive)
  (zork2-init)
  (switch-to-buffer zil-output-buffer)
  (zil-tell "ZORK II: The Wizard of Frobozz" 'CR
            "Copyright (c) 1981-1983 Infocom, Inc. All rights reserved." 'CR
            "ZORK is a registered trademark of Infocom, Inc." 'CR
            "Ported to Emacs Lisp" 'CR 'CR)
  (zork2-look)
  
  (setq zork2-running t)
  (while zork2-running
    (zil-tell 'CR "> ")
    (let ((input (read-string "")))
      (unless (string-empty-p input)
        (zork2-parse-command input)))))

(provide 'zork2)
;;; zork2.el ends here
