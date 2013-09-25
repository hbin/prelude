;; prog-ruby.el --- Enhance Ruby programming
;;
;;; Commentary:

;;; Code:

(defun hbin-ruby-mode-setup ()
  "Font lock for new hash style."
  (font-lock-add-keywords
   'ruby-mode
   '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face)))))

(defun hbin-ruby-mode-init ()
  "Prevent Emacs from adding coding shebang automatically."
  (setq ruby-insert-encoding-magic-comment nil)

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

(provide 'prog-ruby)
;;; prog-ruby.el ends here
