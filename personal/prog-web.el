;; prog-web.el --- Enhance HTML & CSS
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

;;---- VARS --------------------------------------------------------------------

(defvar hbin-web-mode-hook nil)
(defvar hbin-slim-mode-hook nil)
(defvar hbin-scss-mode-hook nil)

(custom-set-variables
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(css-indent-offset 2)
 '(coffee-tab-width 2)

 ;; Customize zencoding mode
 '(zencoding-indentation 2)
 '(zencoding-preview-default nil)
 '(zencoding-insert-flash-time 0.2)

 ;; Customize web mode
 '(web-mode-markup-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-indent-style 2)
 '(web-mode-style-padding 2)
 '(web-mode-script-padding 2)
 '(web-mode-block-padding 0)
 '(web-mode-comment-style 2))

;;; web mode
(eval-after-load 'web-mode
  '(progn
     (defun hbin-web-mode-defaults ()
       ;; Auto complete
       (auto-complete-mode +1)

       ;; Subword
       (subword-mode 1)

       ;; Zencoding
       (defvar zencoding-mode-keymap nil)
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "C-,") 'zencoding-expand-yas)
         (setq zencoding-mode-keymap map))

       (prelude-require-package 'zencoding-mode)
       (require 'zencoding-mode)
       (zencoding-mode 1)

       ;; erb
       (ruby-tools-mode +1)
       (modify-syntax-entry ?$ "w")
       (modify-syntax-entry ?@ "w")
       (modify-syntax-entry ?? "w")
       (modify-syntax-entry ?! "w")
       (modify-syntax-entry ?: ".")

       ;; Set web engine
       (when (and (projectile-project-p)
                  (projectile-verify-file ".python-version"))
         (web-mode-set-engine "django"))

       ;; Load snippets
       (cond
        ((string= web-mode-engine "django")
         (yas-activate-extra-mode 'django-mode))
        ((string= web-mode-engine "erb")
         (yas-activate-extra-mode 'rhtml-mode)))

       (local-set-key (kbd "C-c C-c") 'web-mode-comment-or-uncomment))

     (setq hbin-web-mode-hook 'hbin-web-mode-defaults)
     (add-hook 'web-mode-hook (lambda () (run-hooks 'hbin-web-mode-hook)))))

;;; Slim-mode
(eval-after-load 'slim-mode
  '(progn
     (defun hbin-slim-mode-defaults ()
       (modify-syntax-entry ?? "w")
       (modify-syntax-entry ?! "w"))

     (setq hbin-slim-mode-hook 'hbin-slim-mode-defaults)
     (add-hook 'slim-mode-hook (lambda () (run-hooks 'hbin-slim-mode-hook)))))

(eval-after-load 'scss-mode
  '(progn
     (defun hbin-scss-mode-defaults ()
       (flycheck-mode -1))

     (setq hbin-scss-mode-hook 'hbin-scss-mode-defaults)
     (add-hook 'scss-mode-hook (lambda () (run-hooks 'hbin-scss-mode-hook)))))

(provide 'prog-web)
;;; prog-web.el ends here
