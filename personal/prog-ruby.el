;; prog-ruby.el --- Enhance Ruby programming
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

"Prevent Emacs from adding coding shebang automatically."
(custom-set-variables
 '(ruby-insert-encoding-magic-comment nil))

(prelude-require-package 'ruby-hash-syntax)
(require 'ruby-hash-syntax)

(prelude-require-package 'rbenv)
(require 'rbenv)
(setq rbenv-show-active-ruby-in-modeline nil)
(global-rbenv-mode)

(defun hbin-ruby-mode-setup ()
  "Font lock for new hash style."
  (font-lock-add-keywords
   'ruby-mode
   '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face))
     ("\\(^\\|[^_:.@$\\W]\\|\\.\\.\\)\\b\\(include\\|extend\\|require\\|autoload\\)\\b[^_:.@$\\W]" . font-lock-function-name-face)))

  (define-key ruby-mode-map (kbd "C-c }") 'ruby-toggle-hash-syntax))

(defun hbin-ruby-mode-init ()
  "Modify the Ruby syntax."

  ;; Words prefixed with $ are global variables,
  ;; prefixed with @ are instance variables.
  (modify-syntax-entry ?$ "w")
  (modify-syntax-entry ?@ "w")
  (modify-syntax-entry ?? "w")
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?: ".")

  ;; Launch rinari if in a rails project
  (rinari-launch))

(eval-after-load 'ruby-mode '(hbin-ruby-mode-setup))
(add-hook 'ruby-mode-hook 'hbin-ruby-mode-init)

;; Programming Rails App.
(prelude-require-package 'rinari)
(require 'rinari)

(defun rinari-mode-setup ()
  "Setup for rinari."
  (define-key rinari-minor-mode-map (kbd "C-c ; m") 'rinari-find-model)
  (define-key rinari-minor-mode-map (kbd "C-c ; v") 'rinari-find-view)
  (define-key rinari-minor-mode-map (kbd "C-c ; c") 'rinari-find-controller)
  (define-key rinari-minor-mode-map (kbd "C-c ; h") 'rinari-find-helper)
  (define-key rinari-minor-mode-map (kbd "C-c ; r") 'rinari-find-rspec)
  (define-key rinari-minor-mode-map (kbd "C-c ; p") 'rinari-goto-partial)
  (define-key rinari-minor-mode-map (kbd "C-c ; x") 'rinari-extract-partial)

  (setq rinari-controller-keywords
        (append rinari-controller-keywords
                '("before_action" "after_action" "respond_to" "cookies")))

  (setq rinari-model-keywords
        (append rinari-model-keywords
                '("delegate"))))

(eval-after-load 'rinari '(rinari-mode-setup))

;; RSpec.
(prelude-require-package 'rspec-mode)
(require 'rspec-mode)

(setq rspec-use-spring-when-possible nil
      rspec-use-zeus-when-possible nil
      rspec-use-opts-file-when-available nil)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

(provide 'prog-ruby)
;;; prog-ruby.el ends here
