;; misc-ac.el --- Configuration for Auto Completion
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is not part of GNU Emacs.
;; Auto Completion - http://cx4a.org/software/auto-complete/manual.html#Introduction

;;; Code:
(prelude-require-packages '(auto-complete))
(require 'auto-complete-config)

;; ac-dictionaries for major modes
(add-to-list 'ac-dictionary-directories (concat user-emacs-directory "ac-dict"))

;; ac common settings
(setq ac-use-quick-help nil)                            ; 不需要 quick-help
(setq ac-auto-start nil)                                ; 不要自动补全提示 (ac-start)
(setq ac-auto-show-menu 0)                              ; 当补全提示时 (ac-start), 立即展开补全列表...
(setq ac-menu-height 15)                                ; ...补全列表长一点...
(setq ac-ignore-case nil)                               ; ...要区分大小写...
(setq ac-use-menu-map t)                                ; 当补全列表展开时...
(define-key ac-menu-map (kbd "C-n") 'ac-next)           ; ...可以使用 C-n....
(define-key ac-menu-map (kbd "C-p") 'ac-previous)       ; 和 C-p 上下移动待选项
(define-key ac-menu-map (kbd "M-/") 'ac-next)           ; ...可以使用 C-n....
(define-key ac-mode-map (kbd "M-/") 'ac-start)          ; 按 M-/ 触发补全提示 (ac-start)
(setq ac-trigger-key "TAB")                             ; 按 TAB 仅补全，但不触发补全提示

;; Workarounds
(setq ac-stop-flymake-on-completing t)
(ac-flyspell-workaround)
(ac-linum-workaround)

;; Override the default settings
(defun ac-common-setup ()
  (add-to-list 'ac-sources 'ac-source-filename))

(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources (append '(ac-source-features
                             ac-source-functions
                             ac-source-yasnippet
                             ac-source-variables
                             ac-source-symbols) ac-sources)))

(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))

(defun ac-ruby-mode-setup ()
  (setq ac-sources (append '(ac-source-yasnippet) ac-sources)))

(defun ac-css-mode-setup ()
  (setq ac-sources (append '(ac-source-css-property) ac-sources)))

(defun ac-config-default ()
  (setq-default ac-sources '(ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'web-mode-hook 'ac-css-mode-setup)
  (add-hook 'scss-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))

(ac-config-default)

;; This will make the function definition void
;; (fmakunbound 'completion-at-point)

(defun completion-at-point ()
  "Do nothing.

Override this method to disable `completion-at-point'.")

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-ac)

;;; misc-ac.el ends here
