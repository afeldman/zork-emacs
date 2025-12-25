;;; zork-emacs.el --- Zork Trilogy for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
;; License: GPL-3.0-or-later

;;; Commentary:

;; Launcher for the Zork Trilogy (Zork I, II, and III) in Emacs.

;;; Code:

(require 'zork1)
(require 'zork2)
(require 'zork3)

;;;###autoload
(defun zork-play-game ()
  "Interactively select and play a Zork game."
  (interactive)
  (let ((choice (read-multiple-choice
                 "Which Zork game?"
                 '((?1 "Zork I")
                   (?2 "Zork II")
                   (?3 "Zork III")))))
    (pcase (car choice)
      (49 (zork1-play))
      (50 (zork2-play))
      (51 (zork3-play))
      (_ (zork1-play)))))

;;;###autoload
(defun zork-i ()
  "Start Zork I."
  (interactive)
  (zork1-play))

;;;###autoload
(defun zork-ii ()
  "Start Zork II."
  (interactive)
  (zork2-play))

;;;###autoload
(defun zork-iii ()
  "Start Zork III."
  (interactive)
  (zork3-play))

(provide 'zork-emacs)
;;; zork-emacs.el ends here
