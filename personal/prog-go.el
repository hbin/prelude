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

(defvar gofmt-command nil)
(defvar hbin-go-mode-hook nil)

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
       (setq-local indent-tabs-mode t)
       (setq-local whitespace-line-column 120)

       ;; Overide prelude minor mode keymap
       ;; FYI: http://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
       (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
             (newmap (make-sparse-keymap)))
         (set-keymap-parent newmap oldmap)
         (define-key newmap (kbd "C-c , x") 'go-run)
         (define-key newmap (kbd "C-c , v") 'go-test-current-file)
         (define-key newmap (kbd "C-c , s") 'go-test-current-test)
         (define-key newmap (kbd "C-c , a") 'go-test-current-project)
         (define-key newmap (kbd "C-c , b") 'go-test-current-benchmark)
         (define-key newmap (kbd "C-c n") 'gofmt)
         (define-key newmap (kbd "M-.") 'godef-jump)
         (define-key newmap (kbd "s-.") 'go-guru-implements)
         (define-key newmap (kbd "M-*") 'pop-tag-mark)
         (define-key newmap (kbd "M-R") 'go-rename)
         (make-local-variable 'minor-mode-overriding-map-alist)
         (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist))

       ;; Prefer goimports to gofmt if installed
       (let ((goimports (executable-find "goimports")))
         (when goimports
           (setq gofmt-command goimports)))

       ;; stop whitespace being highlighted
       (whitespace-turn-off)

       ;; flycheck
       ;; (setq-local flycheck-checkers '(go-golint go-vet))
       ;; (setq flycheck-go-vet-shadow t) ;; check for shadowed variables
       (flycheck-mode -1)
       (flyspell-mode -1)

       ;; CamelCase aware editing operations
       (subword-mode +1))

     (setq hbin-go-mode-hook 'hbin-go-mode-defaults)
     (add-hook 'go-mode-hook (lambda () (run-hooks 'hbin-go-mode-hook)))))

(require 'protobuf-mode)

(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))

(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "my-style" my-protobuf-style t)))

(eval-after-load 'protobuf-mode
  '(progn
     (defun hbin-protobuf-mode-defaults ()
       (flycheck-mode -1))

     (setq hbin-protobuf-mode-hook 'hbin-protobuf-mode-defaults)
     (add-hook 'protobuf-mode-hook (lambda () (run-hooks 'hbin-protobuf-mode-hook)))))

(provide 'prog-go)
;;; prog-go.el ends here
