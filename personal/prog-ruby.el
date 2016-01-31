;; prog-ruby.el --- Enhance Ruby programming
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(prelude-require-packages '(rbenv ruby-hash-syntax projectile-rails))

;; rbenv
(require 'rbenv)
(setq rbenv-show-active-ruby-in-modeline nil)
(global-rbenv-mode)

(defun hbin-ruby-mode-setup ()
  "Setup ruby mode."
  (require 'ruby-hash-syntax)
  (define-key ruby-mode-map (kbd "C-c #") 'ruby-toggle-hash-syntax)

  ;; Prevent Emacs from adding coding shebang automatically.
  (setq ruby-insert-encoding-magic-comment nil)

  ;; Font lock for new hash style.
  (font-lock-add-keywords
   'ruby-mode
   '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face))
     ("\\(^\\|[^_:.@$\\W]\\|\\.\\.\\)\\b\\(include\\|extend\\|require\\|autoload\\)\\b[^_:.@$\\W]" . font-lock-function-name-face)))
  )

(defun hbin-ruby-mode-init ()
  "Modify the Ruby syntax."

  ;; Words prefixed with $ are global variables,
  ;; prefixed with @ are instance variables.
  (modify-syntax-entry ?$ "w")
  (modify-syntax-entry ?@ "w")
  (modify-syntax-entry ?? "w")
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?: "."))

(eval-after-load 'ruby-mode '(hbin-ruby-mode-setup))
(add-hook 'ruby-mode-hook 'hbin-ruby-mode-init)

;;; Rails
(custom-set-variables
 '(projectile-rails-expand-snippet nil)
 '(projectile-rails-keymap-prefix (kbd "C-c ;"))
 '(projectile-rails-font-lock-face-name 'font-lock-function-name-face))

(require 'projectile-rails)

(define-key projectile-rails-mode-map (kbd "s-<return>") 'projectile-rails-goto-file-at-point)
(define-key projectile-rails-mode-map (kbd "C-c ; r") 'projectile-rails-find-spec)
(define-key projectile-rails-mode-map (kbd "C-c ; R") 'projectile-rails-find-current-spec)
(define-key projectile-rails-mode-map (kbd "C-c ; p") 'projectile-rails-console)
(define-key projectile-rails-mode-map (kbd "C-c ; P") 'projectile-rails-server)

(add-hook 'projectile-mode-hook 'projectile-rails-on)

(provide 'prog-ruby)
;;; prog-ruby.el ends here
