;; prog-ruby.el --- Enhance Ruby programming
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

(require 'prelude-programming)

(prelude-require-packages '(bundler robe ruby-hash-syntax projectile-rails))

(require 'ruby-mode)

(eval-after-load 'ruby-mode
  '(progn
     "Setup ruby mode."
     (require 'ruby-hash-syntax)
     (define-key ruby-mode-map (kbd "C-c #") 'ruby-toggle-hash-syntax)

     ;; Prevent Emacs from adding coding shebang automatically.
     (setq ruby-insert-encoding-magic-comment nil)

     ;; Don't use spring
     (setq rspec-use-spring-when-possible nil)

     ;; Font lock for new hash style.
     (font-lock-add-keywords
      'ruby-mode
      '(("\\(\\b\\sw[_a-zA-Z0-9]*:\\)\\(?:\\s-\\|$\\)" (1 font-lock-constant-face))
        ("\\(^\\|[^_:.@$\\W]\\|\\.\\.\\)\\b\\(include\\|extend\\|require\\|autoload\\)\\b[^_:.@$\\W]" . font-lock-function-name-face)))

     ;;; Bundler configuration start {{{
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
     ;;; Bundler configuration ends}}}

     ;;; Robe
     (require 'robe)
     (define-key ruby-mode-map (kbd "s-.") 'robe-jump)
     (define-key ruby-mode-map (kbd "s-,") 'pop-tag-mark)
     (setq inf-ruby-console-environment "development")

     ;;; Rails start {{{
     (custom-set-variables
      '(projectile-rails-expand-snippet nil)
      '(projectile-rails-keymap-prefix (kbd "C-c ;"))
      '(projectile-rails-font-lock-face-name 'font-lock-function-name-face))

     (require 'projectile-rails)
     (projectile-rails-global-mode)
     (diminish 'projectile-rails-mode)
     (add-hook 'projectile-rails-mode-hook 'helm-gtags-mode)

     (let ((map projectile-rails-mode-map))
       (define-key map (kbd "s-<return>") 'projectile-rails-goto-file-at-point)
       (define-key map (kbd "C-c ; r") 'projectile-rails-find-spec)
       (define-key map (kbd "C-c ; R") 'projectile-rails-find-current-spec)
       (define-key map (kbd "C-c ; p") 'projectile-rails-console)
       (define-key map (kbd "C-c ; P") 'projectile-rails-server))
     ;;; Rails end }}}

     ;;; Defaults
     (defun hbin-ruby-mode-defaults ()
       (setq-local whitespace-line-column 120)

       (helm-gtags-mode +1)
       (diminish 'helm-gtags-mode)

       ;; Words prefixed with $ are global variables,
       ;; prefixed with @ are instance variables.
       (modify-syntax-entry ?$ "w")
       (modify-syntax-entry ?@ "w"))

     (setq hbin-ruby-mode-hook 'hbin-ruby-mode-defaults)
     (add-hook 'ruby-mode-hook (lambda () (run-hooks 'hbin-ruby-mode-hook)))))

(provide 'prog-ruby)
;;; prog-ruby.el ends here
