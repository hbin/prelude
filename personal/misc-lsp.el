;; misc-lsp.el --- Language Server Protocal
;;
;; Copyright (C) 2012-2021 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(prelude-require-packages '(lsp-mode lsp-ui lsp-treemacs))

(require 'lsp-mode)
(setq lsp-enable-symbol-highlighting nil)

(define-key lsp-mode-map (kbd "C-c C-i") 'lsp-treemacs-symbols)
(define-key lsp-mode-map (kbd "M-.") 'xref-find-definitions) ; lsp-find-definition
(define-key lsp-mode-map (kbd "M-s-.") 'lsp-find-implementation)
(define-key lsp-mode-map (kbd "M-?") 'lsp-find-references)
(define-key lsp-mode-map (kbd "M-r") 'lsp-rename)

;; lsp-ui-mode
(require 'lsp-ui-doc)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-delay 0.1)

(require 'lsp-ui-imenu)
(define-key lsp-ui-imenu-mode-map (kbd "<return>") 'lsp-ui-imenu--visit)
(define-key lsp-ui-imenu-mode-map (kbd "TAB") 'lsp-ui-imenu--view)

;; Hook to programming modes
(add-hook 'ruby-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'json-mode-hook #'lsp-deferred)
(add-hook 'yaml-mode-hook #'lsp-deferred)

(provide 'misc-lsp)
