;; prog-go.el --- Enhance Go programming
;;
;; Copyright (C) 2012-2017 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:
(prelude-require-packages '(go-mode go-eldoc go-projectile gotest))

(eval-after-load 'go-mode
  '(progn
     (require 'go-projectile)

     ;; Ignore go test -c output files
     (add-to-list 'completion-ignored-extensions ".test")

     ;; GOPATH
     (when (memq window-system '(mac ns))
       (exec-path-from-shell-initialize)
       (exec-path-from-shell-copy-env "GOPATH"))

     (defun hbin-go-mode-defaults ()
       (setq-local tab-width 4)
       (setq indent-tabs-mode t)

       ;; Overide prelude minor mode keymap
       ;; FYI: http://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
       (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
             (newmap (make-sparse-keymap)))
         (set-keymap-parent newmap oldmap)
         (define-key newmap (kbd "C-c ; x") 'go-run)
         (define-key newmap (kbd "C-c ; f") 'go-test-current-file)
         (define-key newmap (kbd "C-c ; t") 'go-test-current-test)
         (define-key newmap (kbd "C-c ; p") 'go-test-current-project)
         (define-key newmap (kbd "C-c ; b") 'go-test-current-benchmark)
         (define-key newmap (kbd "C-c n") 'gofmt)
         (define-key newmap (kbd "M-.") 'godef-jump)
         (define-key newmap (kbd "M-*") 'pop-tag-mark)
         (define-key newmap (kbd "M-R") 'go-rename)
         (make-local-variable 'minor-mode-overriding-map-alist)
         (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist))

       ;; Prefer goimports to gofmt if installed
       (let ((goimports (executable-find "goimports")))
         (when goimports
           (setq gofmt-command goimports)))

       ;; gofmt on save
       (add-hook 'before-save-hook 'gofmt-before-save nil t)

       ;; stop whitespace being highlighted
       (whitespace-toggle-options '(tabs))

       ;; flycheck
       (setq-local flycheck-checkers '(go-golint))

       ;; CamelCase aware editing operations
       (subword-mode +1))

     (setq hbin-go-mode-hook 'hbin-go-mode-defaults)
     (add-hook 'go-mode-hook (lambda () (run-hooks 'hbin-go-mode-hook)))))

(provide 'prog-go)
;;; prog-go.el ends here
