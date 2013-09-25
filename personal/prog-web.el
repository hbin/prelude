;; prog-web.el --- Enhance HTML & CSS
;;
;; Copyright (C) 2012-2013 Huang Bin
;;
;; Author: Huang Bin <embrace.hbin@gmail.com>
;; Version: 1.0.0

;;; Commentary:

;;; Code:

;;; Indentation
(custom-set-variables
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(css-indent-offset 2)
 '(coffee-tab-width 2))

;;; web mode
(eval-after-load 'web-mode
  '(progn
     (defun hbin-web-mode-defaults ()
       (setq web-mode-markup-indent-offset 2)
       (setq web-mode-css-indent-offset 2)
       (setq web-mode-code-indent-offset 2)
       (setq web-mode-indent-style 2)
       (setq web-mode-style-padding 2)
       (setq web-mode-script-padding 2)
       (setq web-mode-block-padding 0)
       (setq web-mode-comment-style 2)

       ;; erb
       (ruby-tools-mode +1)
       (modify-syntax-entry ?$ "w")
       (modify-syntax-entry ?@ "w")
       (modify-syntax-entry ?? "w")
       (modify-syntax-entry ?! "w")
       (modify-syntax-entry ?: "."))

     (setq hbin-web-mode-hook 'hbin-web-mode-defaults)
     (add-hook 'web-mode-hook (lambda ()
                                (run-hooks 'hbin-web-mode-hook)))))

;;; CSS
(prelude-require-package 'rainbow-mode)
(eval-after-load 'css-mode
  '(progn
     (defun hbin-css-mode-defaults ()
       (rainbow-mode 1))

     (setq hbin-css-mode-hook 'hbin-css-mode-defaults)
     (add-hook 'css-mode-hook (lambda ()
                                (run-hooks 'hbin-css-mode-hook)))))

;;; Slim-mode
(prelude-require-package 'slim-mode)
(add-to-list 'auto-mode-alist '("\\.slim$" . slim-mode))

(eval-after-load 'slim-mode
  '(progn
     (defun hbin-slim-mode-defaults ()
       (modify-syntax-entry ?? "w")
       (modify-syntax-entry ?! "w"))

     (setq hbin-slim-mode-hook 'hbin-slim-mode-defaults)
     (add-hook 'slim-mode-hook (lambda ()
                                 (run-hook 'hbin-slim-mode-hook)))))

;;; Scss-mode
(prelude-require-package 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

(eval-after-load 'scss-mode
  '(progn
     (defun hbin-scss-mode-defaults ()
       (rainbow-mode 1)
       (setq scss-compile-at-save nil))

     (setq hbin-scss-mode-hook 'hbin-scss-mode-defaults)
     (add-hook 'scss-mode-hook (lambda ()
                                 (run-hook 'hbin-scss-mode-hook)))))

(provide 'prog-web)
;;; prog-web.el ends here
