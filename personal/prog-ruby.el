;; prog-ruby.el --- Enhance Ruby programming
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(prelude-require-packages '(rbenv bundler robe ruby-hash-syntax projectile-rails))

;;; rbenv
(require 'rbenv)
(setq rbenv-show-active-ruby-in-modeline nil)
(global-rbenv-mode)

;;; bundler
(require 'bundler)

(defvar bundle-commonly-used-gems
  '("actionpack"
    "activemodel"
    "activerecord"
    "activesupport"
    "railties"
    ))

(defun bundle-commonly-used-gem-paths ()
  "Get commonly used gems' paths."
  (-filter 'stringp (-map 'bundle-gem-location
                          bundle-commonly-used-gems)))

(defun bundle-gtags ()
  "Generate gtags for every commonly used gems."
  (interactive)
  (let ((bundler-stdout
         (shell-command-to-string "bundle check")))
    (unless (string-match "Could not locate Gemfile" bundler-stdout)
      (let ((gem-paths (bundle-commonly-used-gem-paths)))
        (if gem-paths
            (progn
              (-each gem-paths
                (lambda (path)
                  (helm-gtags-create-tags path nil)))
              (setenv "GTAGSLIBPATH" (s-join ":" gem-paths))))))))

;;; robe-mode
;; Robe is a code assistance tool that uses a Ruby REPL subprocess with your
;; application.
;;
;; The following settings is intend to start a robe subprocess to provide
;; ac-source for auto-complete only, without enable robe-mode.
(require 'ac-robe)
(require 'robe)

;;override
(defun ac-robe-available ()
  (let* ((ruby-buffer (and inf-ruby-buffer
                           (get-buffer inf-ruby-buffer)))
         (process (get-buffer-process ruby-buffer)))
    process))

(defadvice robe-start (after ac-setup-after-robe-start (&optional force) activate)
  "Setup ac-robe-source when robe-start successfuly."
  (if (and (or robe-running
               (ac-robe-available))
           (not (-contains? 'ac-sources ac-source-robe)))
      (ac-robe-setup)))

;;; Ruby mode
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
  (modify-syntax-entry ?@ "w"))

(eval-after-load 'ruby-mode '(hbin-ruby-mode-setup))
(add-hook 'ruby-mode-hook 'hbin-ruby-mode-init)

;;; Rails
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
(add-hook 'projectile-rails-mode-hook 'helm-gtags-mode)

(provide 'prog-ruby)
;;; prog-ruby.el ends here
