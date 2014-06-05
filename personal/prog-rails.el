;; prog-rails.el --- Enhance Programming Rails
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(prelude-require-packages '(projectile-rails rspec-mode))

;;; Rails mode based on projectile
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

;;; RSpec.
(require 'rspec-mode)
(setq rspec-use-spring-when-possible nil
      rspec-use-zeus-when-possible nil
      rspec-use-opts-file-when-available nil)

(defadvice rspec-compile (around rspec-compile-around)
  "Use BASH shell for running the specs because of ZSH issues."
  (let ((shell-file-name "/bin/bash"))
    ad-do-it))

(ad-activate 'rspec-compile)

(provide 'prog-rails)
;;; prog-rails.el ends here
