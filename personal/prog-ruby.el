;; prog-ruby.el --- Enhance Ruby programming
;;
;; Copyright (C) 2012-2013 Huang Bin
;;
;; Author: Huang Bin <embrace.hbin@gmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

"Prevent Emacs from adding coding shebang automatically."
(custom-set-variables
 '(ruby-insert-encoding-magic-comment nil))

(defun hbin-ruby-mode-setup ()
  "Font lock for new hash style."
  (font-lock-add-keywords
   'ruby-mode
   '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face)))))

(defun hbin-ruby-mode-init ()
  ;; Words prefixed with $ are global variables,
  ;; prefixed with @ are instance variables.
  (modify-syntax-entry ?$ "w")
  (modify-syntax-entry ?@ "w")
  (modify-syntax-entry ?? "w")
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?: ".")

  (flycheck-mode 1))

(eval-after-load 'ruby-mode '(hbin-ruby-mode-setup))
(add-hook 'ruby-mode-hook 'hbin-ruby-mode-init)

(defun insert-arrow ()
  "Insert arrow and put cursor at the right position."
  (interactive)
  (delete-horizontal-space t)
  (insert " => "))
(global-set-key (kbd "C-.") 'insert-arrow)

;; Programming Rails App.
(prelude-require-package 'rinari)
(require 'rinari)
(define-key rinari-minor-mode-map (kbd "C-c m") 'rinari-find-model)
(define-key rinari-minor-mode-map (kbd "C-c v") 'rinari-find-view)
(define-key rinari-minor-mode-map (kbd "C-c c") 'rinari-find-controller)
(global-rinari-mode)

(provide 'prog-ruby)
;;; prog-ruby.el ends here
