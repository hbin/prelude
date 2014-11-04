;; prog-ruby.el --- Enhance Ruby programming
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(prelude-require-packages '(rbenv ruby-hash-syntax))

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

(provide 'prog-ruby)
;;; prog-ruby.el ends here
