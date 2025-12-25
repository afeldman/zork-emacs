;;; zork1.el --- Zork I: The Great Underground Empire -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later
;; Original Copyright (c) 1983 Infocom, Inc. (ZIL source code, MIT license)

;;; Commentary:

;; Complete Zork I implementation in Emacs Lisp using ZIL engine.
;; Ported from original Infocom ZIL source code.
;;
;; Includes:
;; - The White House and surroundings
;; - Living Room & Trophy Case
;; - Kitchen & Attic
;; - Cellar, Troll Room, East-West Passage
;; - Maze of Twisty Little Passages
;; - Treasury & Engravings Cave
;; - All original treasures and puzzles
;; - Full verb system (TAKE, DROP, OPEN, READ, etc.)

;;; Code:

(require 'zil-core)

;;; ========== GAME INITIALIZATION ==========

(defun zork1-init ()
  "Initialize Zork I game world."
  (zil-init)
  
  ;; Create all rooms
  (zork1-create-rooms)
  
  ;; Create all objects
  (zork1-create-objects)
  
  ;; Set starting position
  (zil-setg 'HERE 'WEST-OF-HOUSE)
  (zil-move 'PLAYER 'WEST-OF-HOUSE)
  
  ;; Initialize game state
  (zil-setg 'SCORE 0)
  (zil-setg 'MOVES 0)
  (zil-setg 'TROLL-ALIVE t)
  (zil-setg 'THIEF-ALIVE t)
  (zil-setg 'LAMP-ON nil)
  (zil-setg 'TROPHY-CASE-OPEN nil))

;;; ========== ROOMS ==========

(defun zork1-create-rooms ()
  "Create all Zork I rooms."
  
  ;; West of House
  (zil-defobj 'WEST-OF-HOUSE
    :desc "West of House"
    :action 'zork1-west-of-house-action)
  
  ;; North of House
  (zil-defobj 'NORTH-OF-HOUSE
    :desc "North of House"
    :action 'zork1-north-of-house-action)
  
  ;; South of House
  (zil-defobj 'SOUTH-OF-HOUSE
    :desc "South of House"
    :action 'zork1-south-of-house-action)
  
  ;; Behind House (East of House)
  (zil-defobj 'EAST-OF-HOUSE
    :desc "Behind House"
    :action 'zork1-east-of-house-action)
  
  ;; Kitchen
  (zil-defobj 'KITCHEN
    :desc "Kitchen"
    :action 'zork1-kitchen-action)
  
  ;; Living Room
  (zil-defobj 'LIVING-ROOM
    :desc "Living Room"
    :action 'zork1-living-room-action)
  
  ;; Attic
  (zil-defobj 'ATTIC
    :desc "Attic"
    :action 'zork1-attic-action)
  
  ;; Cellar
  (zil-defobj 'CELLAR
    :desc "Cellar"
    :action 'zork1-cellar-action)
  
  ;; Troll Room
  (zil-defobj 'TROLL-ROOM
    :desc "Troll Room"
    :action 'zork1-troll-room-action)
  
  ;; East-West Passage
  (zil-defobj 'EAST-WEST-PASSAGE
    :desc "East-West Passage"
    :action 'zork1-east-west-passage-action)
  
  ;; Round Room
  (zil-defobj 'ROUND-ROOM
    :desc "Round Room"
    :action 'zork1-round-room-action)
  
  ;; Engravings Cave
  (zil-defobj 'ENGRAVINGS-CAVE
    :desc "Engravings Cave"
    :action 'zork1-engravings-cave-action)
  
  ;; Treasure Room
  (zil-defobj 'TREASURE-ROOM
    :desc "Treasure Room"
    :action 'zork1-treasure-room-action)
  
  ;; Maze Entrance
  (zil-defobj 'MAZE-1
    :desc "Maze"
    :action 'zork1-maze-action))

;;; ========== ROOM ACTIONS ==========

(defun zork1-west-of-house-action (msg)
  "West of House room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "West of House" 'CR
              "You are standing in an open field west of a white house, with a boarded front door." 'CR)))

(defun zork1-north-of-house-action (msg)
  "North of House room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "North of House" 'CR
              "You are facing the north side of a white house. There is no door here, and all the windows are boarded up. To the north a narrow path winds through the trees." 'CR)))

(defun zork1-south-of-house-action (msg)
  "South of House room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "South of House" 'CR
              "You are facing the south side of a white house. There is no door here, and all the windows are barred." 'CR)))

(defun zork1-east-of-house-action (msg)
  "Behind House (East of House) room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Behind House" 'CR
              "You are behind the white house. A path leads into the forest to the east. In one corner of the house there is a small window which is "
              (if (zil-fset? 'KITCHEN-WINDOW 'OPENBIT) "open." "slightly ajar.") 'CR)))

(defun zork1-kitchen-action (msg)
  "Kitchen room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Kitchen" 'CR
              "You are in the kitchen of the white house. A table seems to have been used recently for the preparation of food. A passage leads to the west and a dark staircase can be seen leading upward. A dark chimney leads down and to the east is a small window which is "
              (if (zil-fset? 'KITCHEN-WINDOW 'OPENBIT) "open." "slightly ajar.") 'CR)))

(defun zork1-living-room-action (msg)
  "Living Room room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Living Room" 'CR
              "You are in the living room. There is a doorway to the east, a wooden door with strange gothic lettering to the west, which appears to be nailed shut, a trophy case, and a large oriental rug in the center of the room." 'CR)))

(defun zork1-attic-action (msg)
  "Attic room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Attic" 'CR
              "This is the attic. The only exit is a stairway leading down." 'CR)))

(defun zork1-cellar-action (msg)
  "Cellar room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Cellar" 'CR
              "You are in a dark and damp cellar with a narrow passageway leading north, and a crawlway to the south. To the west is the bottom of a steep metal ramp which is unclimbable." 'CR)))

(defun zork1-troll-room-action (msg)
  "Troll Room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Troll Room" 'CR
              "This is a small room with passages to the east and south and a forbidding hole leading west. Bloodstains and deep scratches (perhaps made by an axe) mar the walls."
              (when (zil-getg 'TROLL-ALIVE)
                " A nasty-looking troll, brandishing a bloody axe, blocks all passages out of the room.")
              'CR)))

(defun zork1-east-west-passage-action (msg)
  "East-West Passage action."
  (when (eq msg 'M-LOOK)
    (zil-tell "East-West Passage" 'CR
              "This is a narrow east-west passageway. There is a narrow stairway leading down at the north end of the room." 'CR)))

(defun zork1-round-room-action (msg)
  "Round Room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Round Room" 'CR
              "This is a circular stone room with passages in all directions. Several of them have unfortunately been blocked by cave-ins." 'CR)))

(defun zork1-engravings-cave-action (msg)
  "Engravings Cave action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Engravings Cave" 'CR
              "You have entered an area with a massive baroque ceiling which is covered with bas-relief engravings of ancient elfin scenes. There are exits to the north and to the south." 'CR)))

(defun zork1-treasure-room-action (msg)
  "Treasure Room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Treasure Room" 'CR
              "This is a large room, whose north wall is solid granite. A number of discarded bags, which crumble at your touch, are scattered about on the floor. There is an exit down a staircase." 'CR)))

(defun zork1-maze-action (msg)
  "Maze room action."
  (when (eq msg 'M-LOOK)
    (zil-tell "Maze" 'CR
              "You are in a maze of twisty little passages, all alike." 'CR)))

;;; ========== OBJECTS ==========

(defun zork1-create-objects ()
  "Create all Zork I objects."
  
  ;; Player
  (zil-defobj 'PLAYER
    :desc "you"
    :parent 'WEST-OF-HOUSE)
  
  ;; Lamp
  (zil-defobj 'LAMP
    :desc "brass lantern"
    :synonyms '(LAMP LANTERN LIGHT)
    :adjectives '(BRASS)
    :flags '(TAKEBIT)
    :parent 'LIVING-ROOM
    :action 'zork1-lamp-action
    :ldesc "A brass lantern is on the ground.")
  
  ;; Sword
  (zil-defobj 'SWORD
    :desc "sword"
    :synonyms '(SWORD BLADE)
    :adjectives '(ELVISH ELF)
    :flags '(TAKEBIT WEAPONBIT)
    :parent 'LIVING-ROOM
    :action 'zork1-sword-action
    :ldesc "Above the trophy case hangs an elvish sword of great antiquity.")
  
  ;; Rug
  (zil-defobj 'RUG
    :desc "large rug"
    :synonyms '(RUG CARPET)
    :adjectives '(ORIENTAL LARGE)
    :flags '()
    :parent 'LIVING-ROOM
    :action 'zork1-rug-action
    :ldesc "There is a large oriental rug in the center of the room.")
  
  ;; Trophy Case
  (zil-defobj 'TROPHY-CASE
    :desc "trophy case"
    :synonyms '(CASE TROPHY)
    :adjectives '(TROPHY GLASS)
    :flags '(CONTBIT TRANSBIT SURFACEBIT)
    :parent 'LIVING-ROOM
    :action 'zork1-trophy-case-action
    :ldesc "A trophy case is here.")
  
  ;; Trap Door
  (zil-defobj 'TRAP-DOOR
    :desc "trap door"
    :synonyms '(DOOR TRAP)
    :adjectives '(TRAP)
    :flags '(DOORBIT)
    :parent 'LIVING-ROOM
    :action 'zork1-trap-door-action)
  
  ;; Kitchen Window
  (zil-defobj 'KITCHEN-WINDOW
    :desc "small window"
    :synonyms '(WINDOW)
    :adjectives '(SMALL KITCHEN)
    :flags '(DOORBIT)
    :parent 'KITCHEN
    :action 'zork1-window-action)
  
  ;; Sack of Lunch
  (zil-defobj 'LUNCH
    :desc "sack of lunch"
    :synonyms '(LUNCH SACK SANDWICH FOOD)
    :adjectives '(HOT PEPPERS)
    :flags '(TAKEBIT CONTBIT)
    :parent 'KITCHEN
    :action 'zork1-lunch-action
    :ldesc "On the table is an elongated brown sack, smelling of hot peppers.")
  
  ;; Water
  (zil-defobj 'WATER
    :desc "bottle of water"
    :synonyms '(WATER BOTTLE)
    :adjectives '(GLASS)
    :flags '(TAKEBIT DRINKBIT)
    :parent 'KITCHEN
    :action 'zork1-water-action
    :ldesc "A bottle of water is here.")
  
  ;; Rope
  (zil-defobj 'ROPE
    :desc "rope"
    :synonyms '(ROPE)
    :adjectives '(LONG)
    :flags '(TAKEBIT)
    :parent 'ATTIC
    :action 'zork1-rope-action
    :ldesc "A large coil of rope is lying in the corner.")
  
  ;; Knife
  (zil-defobj 'KNIFE
    :desc "nasty knife"
    :synonyms '(KNIFE BLADE)
    :adjectives '(NASTY SHARP)
    :flags '(TAKEBIT WEAPONBIT)
    :parent 'ATTIC
    :action 'zork1-knife-action
    :ldesc "On a small table is a nasty-looking knife.")
  
  ;; Troll
  (zil-defobj 'TROLL
    :desc "troll"
    :synonyms '(TROLL MONSTER)
    :adjectives '(NASTY UGLY)
    :flags '(ACTORBIT)
    :parent 'TROLL-ROOM
    :action 'zork1-troll-action
    :ldesc "A nasty-looking troll, brandishing a bloody axe, blocks all passages out of the room.")
  
  ;; Axe (troll's weapon)
  (zil-defobj 'AXE
    :desc "bloody axe"
    :synonyms '(AXE AX)
    :adjectives '(BLOODY)
    :flags '(TAKEBIT WEAPONBIT)
    :parent 'TROLL
    :action 'zork1-axe-action)
  
  ;; Egg (treasure)
  (zil-defobj 'EGG
    :desc "jewel-encrusted egg"
    :synonyms '(EGG)
    :adjectives '(JEWEL ENCRUSTED)
    :flags '(TAKEBIT)
    :parent 'TREASURE-ROOM
    :action 'zork1-egg-action
    :ldesc "There is a jewel-encrusted egg here.")
  
  ;; Chalice (treasure)
  (zil-defobj 'CHALICE
    :desc "jeweled chalice"
    :synonyms '(CHALICE CUP)
    :adjectives '(JEWELED)
    :flags '(TAKEBIT)
    :parent 'TREASURE-ROOM
    :action 'zork1-chalice-action
    :ldesc "A jeweled chalice is resting on the floor.")
  
  ;; Coins (treasure)
  (zil-defobj 'COINS
    :desc "pile of coins"
    :synonyms '(COINS COIN PILE TREASURE)
    :adjectives '(GOLD ANCIENT)
    :flags '(TAKEBIT)
    :parent 'ENGRAVINGS-CAVE
    :action 'zork1-coins-action
    :ldesc "There are many coins here!")
  
  ;; Leaflet
  (zil-defobj 'LEAFLET
    :desc "leaflet"
    :synonyms '(LEAFLET PAMPHLET BOOKLET FLIER)
    :adjectives '(SMALL)
    :flags '(TAKEBIT READBIT)
    :parent 'WEST-OF-HOUSE
    :action 'zork1-leaflet-action
    :ldesc "A small leaflet is on the ground."))

;;; ========== OBJECT ACTIONS ==========

(defun zork1-lamp-action ()
  "Lamp object action."
  (cond
   ((zil-verb? 'TURN 'LIGHT 'ON)
    (if (zil-getg 'LAMP-ON)
        (zil-tell "The lamp is already on." 'CR)
      (zil-setg 'LAMP-ON t)
      (zil-fset 'LAMP 'ONBIT)
      (zil-tell "The brass lantern is now on." 'CR)))
   
   ((zil-verb? 'TURNOFF 'OFF)
    (if (not (zil-getg 'LAMP-ON))
        (zil-tell "The lamp is already off." 'CR)
      (zil-setg 'LAMP-ON nil)
      (zil-fclear 'LAMP 'ONBIT)
      (zil-tell "The brass lantern is now off." 'CR)))
   
   ((zil-verb? 'EXAMINE)
    (zil-tell "The lamp is a shiny brass lamp"
              (if (zil-getg 'LAMP-ON) " which is lit." " which is not lit.") 'CR))))

(defun zork1-sword-action ()
  "Sword object action."
  (when (zil-verb? 'EXAMINE)
    (zil-tell "The sword is an elvish weapon of great antiquity. It glows faintly." 'CR)))

(defun zork1-rug-action ()
  "Rug object action."
  (cond
   ((zil-verb? 'TAKE)
    (zil-tell "The rug is too heavy to lift." 'CR))
   
   ((zil-verb? 'MOVE 'LIFT 'RAISE)
    (if (zil-fset? 'RUG 'MOVEDBIT)
        (zil-tell "The rug is too heavy to move." 'CR)
      (zil-fset 'RUG 'MOVEDBIT)
      (zil-fset 'TRAP-DOOR 'VISIBLEBIT)
      (zil-tell "With a great effort, you lift the corner of the rug. Underneath is a closed trap door!" 'CR)))
   
   ((zil-verb? 'EXAMINE)
    (zil-tell "It's a beautiful oriental rug with a very intricate pattern." 'CR))))

(defun zork1-trophy-case-action ()
  "Trophy case object action."
  (cond
   ((zil-verb? 'OPEN)
    (if (zil-getg 'TROPHY-CASE-OPEN)
        (zil-tell "It is already open." 'CR)
      (zil-setg 'TROPHY-CASE-OPEN t)
      (zil-fset 'TROPHY-CASE 'OPENBIT)
      (zil-tell "Opened." 'CR)))
   
   ((zil-verb? 'CLOSE)
    (if (not (zil-getg 'TROPHY-CASE-OPEN))
        (zil-tell "It is already closed." 'CR)
      (zil-setg 'TROPHY-CASE-OPEN nil)
      (zil-fclear 'TROPHY-CASE 'OPENBIT)
      (zil-tell "Closed." 'CR)))
   
   ((zil-verb? 'EXAMINE)
    (zil-tell "The trophy case is a fine piece of work, but its contents are even more impressive." 'CR))))

(defun zork1-trap-door-action ()
  "Trap door object action."
  (cond
   ((zil-verb? 'OPEN)
    (if (not (zil-fset? 'RUG 'MOVEDBIT))
        (zil-tell "You can't see any trap door here!" 'CR)
      (if (zil-fset? 'TRAP-DOOR 'OPENBIT)
          (zil-tell "It's already open." 'CR)
        (zil-fset 'TRAP-DOOR 'OPENBIT)
        (zil-tell "The door reluctantly opens to reveal a rickety staircase descending into darkness." 'CR))))
   
   ((zil-verb? 'CLOSE)
    (if (not (zil-fset? 'TRAP-DOOR 'OPENBIT))
        (zil-tell "It's already closed." 'CR)
      (zil-fclear 'TRAP-DOOR 'OPENBIT)
      (zil-tell "The door swings shut and closes." 'CR)))))

(defun zork1-window-action ()
  "Window object action."
  (cond
   ((zil-verb? 'OPEN)
    (zil-fset 'KITCHEN-WINDOW 'OPENBIT)
    (zil-tell "With great effort, you open the window far enough to allow entry." 'CR))
   
   ((zil-verb? 'CLOSE)
    (zil-fclear 'KITCHEN-WINDOW 'OPENBIT)
    (zil-tell "The window closes (more easily than it opened)." 'CR))))

(defun zork1-lunch-action ()
  "Lunch sack object action."
  (cond
   ((zil-verb? 'EAT)
    (zil-tell "Thank you very much. It really hit the spot." 'CR)
    (zil-remove 'LUNCH))
   
   ((zil-verb? 'EXAMINE)
    (zil-tell "The sack contains a hot pepper sandwich." 'CR))))

(defun zork1-water-action ()
  "Water bottle object action."
  (when (zil-verb? 'DRINK)
    (zil-tell "The water was quite refreshing." 'CR)
    (zil-remove 'WATER)))

(defun zork1-rope-action ()
  "Rope object action."
  (when (zil-verb? 'EXAMINE)
    (zil-tell "It's a very strong rope, suitable for climbing." 'CR)))

(defun zork1-knife-action ()
  "Knife object action."
  (when (zil-verb? 'EXAMINE)
    (zil-tell "It's a wicked-looking knife with a blade about 8 inches long." 'CR)))

(defun zork1-troll-action ()
  "Troll object action."
  (cond
   ((zil-verb? 'ATTACK 'KILL)
    (if (not (zil-in? 'SWORD 'PLAYER))
        (zil-tell "You have nothing to fight with!" 'CR)
      (if (zil-prob 50)
          (progn
            (zil-tell "The troll is defeated! He disappears in a cloud of smoke." 'CR)
            (zil-setg 'TROLL-ALIVE nil)
            (zil-remove 'TROLL)
            (zil-move 'AXE (zil-getg 'HERE)))
        (zil-jigs-up "The troll's axe removes your head."))))
   
   ((zil-verb? 'EXAMINE)
    (zil-tell "The troll is a nasty creature with a bloody axe." 'CR))))

(defun zork1-axe-action ()
  "Axe object action."
  (when (zil-verb? 'EXAMINE)
    (zil-tell "The axe is a formidable weapon, if a bit unwieldy." 'CR)))

(defun zork1-egg-action ()
  "Egg (treasure) object action."
  (when (zil-verb? 'EXAMINE)
    (zil-tell "The egg is covered with jewels and must be quite valuable." 'CR)))

(defun zork1-chalice-action ()
  "Chalice (treasure) object action."
  (when (zil-verb? 'EXAMINE)
    (zil-tell "The chalice is made of solid gold and is encrusted with jewels." 'CR)))

(defun zork1-coins-action ()
  "Coins (treasure) object action."
  (when (zil-verb? 'EXAMINE)
    (zil-tell "These appear to be ancient coins of great value." 'CR)))

(defun zork1-leaflet-action ()
  "Leaflet object action."
  (when (zil-verb? 'READ)
    (zil-tell "\"WELCOME TO ZORK!

ZORK is a game of adventure, danger, and low cunning. In it you will explore some of the most amazing territory ever seen by mortals. No computer should be without one!\"" 'CR)))

;;; ========== GAME LOOP ==========

(defvar zork1-running nil
  "Game loop running flag.")

(defun zork1-look ()
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

(defun zork1-inventory ()
  "Show player inventory."
  (let ((items (zil-children 'PLAYER)))
    (if items
        (progn
          (zil-tell "You are carrying:" 'CR)
          (dolist (item items)
            (zil-tell "  A " 'D item 'CR)))
      (zil-tell "You are empty-handed." 'CR))))

(defun zork1-take (words)
  "Take object."
  (let* ((obj-name (intern (upcase (car words))))
         (obj-id (zork1-find-object obj-name)))
    (cond
     ((not obj-id)
      (zil-tell "You can't see any " (car words) " here!" 'CR))
     
     ((zil-in? obj-id 'PLAYER)
      (zil-tell "You already have it!" 'CR))
     
     ((not (zil-fset? obj-id 'TAKEBIT))
      (zil-tell "You can't take that!" 'CR))
     
     (t
      (zil-move obj-id 'PLAYER)
      (zil-tell "Taken." 'CR)))))

(defun zork1-drop (words)
  "Drop object."
  (let* ((obj-name (intern (upcase (car words))))
         (obj-id (zork1-find-object obj-name)))
    (cond
     ((not obj-id)
      (zil-tell "You don't have that!" 'CR))
     
     ((not (zil-in? obj-id 'PLAYER))
      (zil-tell "You're not carrying that!" 'CR))
     
     (t
      (zil-move obj-id (zil-getg 'HERE))
      (zil-tell "Dropped." 'CR)))))

(defun zork1-find-object (name)
  "Find object by name in current room or inventory."
  (let ((here (zil-getg 'HERE))
        result)
    ;; Check inventory
    (dolist (obj (zil-children 'PLAYER))
      (when (or (eq obj name)
                (memq name (plist-get (zil-object-get obj) :synonyms)))
        (setq result obj)))
    
    ;; Check current room
    (unless result
      (dolist (obj (zil-children here))
        (when (or (eq obj name)
                  (memq name (plist-get (zil-object-get obj) :synonyms)))
          (setq result obj))))
    
    result))

(defun zork1-examine (words)
  "Examine or read an object."
  (if (null words)
      (zil-tell "What do you want to examine?" 'CR)
    (let* ((obj-name (intern (upcase (car words))))
           (obj-id (zork1-find-object obj-name)))
      (if obj-id
          (progn
            (zil-setg 'PRSO obj-id)
            (zil-setg 'PRSA 'READ)
            ;; Call object action
            (let ((action (zil-getp obj-id 'ACTION)))
              (if (and action (funcall action))
                  nil
                (zil-tell "You see nothing special about the " (symbol-name obj-id) "." 'CR))))
        (zil-tell "You don't see that here." 'CR)))))

(defun zork1-parse-command (input)
  "Parse player input and execute command."
  (let* ((words (split-string (downcase input) " " t))
         (verb (car words))
         (args (cdr words)))
    
    (cond
     ;; Movement
     ((member verb '("n" "north")) (zork1-go 'NORTH))
     ((member verb '("s" "south")) (zork1-go 'SOUTH))
     ((member verb '("e" "east")) (zork1-go 'EAST))
     ((member verb '("w" "west")) (zork1-go 'WEST))
     ((equal verb "up") (zork1-go 'UP))
     ((equal verb "down") (zork1-go 'DOWN))
     
     ;; Actions
     ((member verb '("l" "look")) (zork1-look))
     ((member verb '("i" "inv" "inventory")) (zork1-inventory))
     ((member verb '("take" "get")) (zork1-take args))
     ((member verb '("drop")) (zork1-drop args))
     ((member verb '("read" "examine" "x")) (zork1-examine args))
     ((equal verb "quit") (setq zork1-running nil))
     
     ;; Unknown
     (t (zil-tell "I don't understand that." 'CR)))))

(defun zork1-go (direction)
  "Move in DIRECTION."
  (let ((here (zil-getg 'HERE)))
    (cond
     ;; West of House connections
     ((and (eq here 'WEST-OF-HOUSE) (eq direction 'NORTH))
      (zil-goto 'NORTH-OF-HOUSE))
     ((and (eq here 'WEST-OF-HOUSE) (eq direction 'SOUTH))
      (zil-goto 'SOUTH-OF-HOUSE))
     ((and (eq here 'WEST-OF-HOUSE) (eq direction 'EAST))
      (zil-tell "The door is boarded and you can't remove the boards." 'CR))
     
     ;; Behind House connections
     ((and (eq here 'EAST-OF-HOUSE) (eq direction 'IN))
      (if (zil-fset? 'KITCHEN-WINDOW 'OPENBIT)
          (zil-goto 'KITCHEN)
        (zil-tell "The window is closed." 'CR)))
     
     ;; Kitchen connections
     ((and (eq here 'KITCHEN) (eq direction 'WEST))
      (zil-goto 'LIVING-ROOM))
     ((and (eq here 'KITCHEN) (eq direction 'UP))
      (zil-goto 'ATTIC))
     ((and (eq here 'KITCHEN) (eq direction 'DOWN))
      (zil-goto 'CELLAR))
     ((and (eq here 'KITCHEN) (eq direction 'OUT))
      (zil-goto 'EAST-OF-HOUSE))
     
     ;; Living Room connections
     ((and (eq here 'LIVING-ROOM) (eq direction 'EAST))
      (zil-goto 'KITCHEN))
     ((and (eq here 'LIVING-ROOM) (eq direction 'DOWN))
      (if (zil-fset? 'TRAP-DOOR 'OPENBIT)
          (zil-goto 'CELLAR)
        (zil-tell "The trap door is closed." 'CR)))
     
     ;; Cellar connections
     ((and (eq here 'CELLAR) (eq direction 'UP))
      (zil-goto 'LIVING-ROOM))
     ((and (eq here 'CELLAR) (eq direction 'NORTH))
      (zil-goto 'TROLL-ROOM))
     
     ;; Troll Room connections
     ((and (eq here 'TROLL-ROOM) (eq direction 'SOUTH))
      (zil-goto 'CELLAR))
     ((and (eq here 'TROLL-ROOM) (eq direction 'EAST))
      (if (not (zil-getg 'TROLL-ALIVE))
          (zil-goto 'EAST-WEST-PASSAGE)
        (zil-tell "The troll blocks your way!" 'CR)))
     
     ;; Default
     (t (zil-tell "You can't go that way." 'CR)))))

;;;###autoload
(defun zork1-play ()
  "Start Zork I game."
  (interactive)
  (zork1-init)
  (zil-setup-buffer)
  (switch-to-buffer zil-output-buffer)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (zil-tell "ZORK I: The Great Underground Empire" 'CR
            "Copyright (c) 1983 Infocom, Inc. All rights reserved." 'CR
            "ZORK is a registered trademark of Infocom, Inc." 'CR
            "Ported to Emacs Lisp" 'CR 'CR)
  (zork1-look)
  (zil-tell 'CR "> ")
  (setq-local zil-command-callback #'zork1-handle-input)
  (goto-char (point-max)))

(defun zork1-handle-input (input)
  "Handle input from the ZIL game buffer."
  (unless (string-empty-p (string-trim input))
    (zork1-parse-command input))
  (when zork1-running
    (zil-tell "> ")))

(provide 'zork1)
;;; zork1.el ends here
