;;; misc-company.el --- company-mode setup
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:
(prelude-require-packages '(company company-anaconda robe))

(require 'company)

(setq company-idle-delay nil)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 1)
(setq company-abort-manual-when-too-short t)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above nil)

;;override
(defun company-complete-common-or-cycle-dwim (&optional arg)
  "Insert the common part of all candidates, or select the next one.

With ARG, move by that many elements."
  (interactive "p")
  (when (company-manual-begin)
    (let ((tick (buffer-chars-modified-tick)))
      (call-interactively 'company-complete-common)
      (when (eq tick (buffer-chars-modified-tick))
        (let ((company-selection-wrap-around t)
              (current-prefix-arg arg))
          (if (or (eq last-command 'company-select-next)
                  (eq last-command 'company-select-previous))
              (company-complete-selection)
            (company-select-next)))))))

(global-set-key (kbd "TAB") 'company-indent-or-complete-common)
(global-set-key (kbd "M-/") 'company-complete-common)
(define-key company-active-map [tab] 'company-complete-common-or-cycle-dwim)
(define-key company-active-map (kbd "M-/") 'company-complete-common-or-cycle)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)

;;;robe-mode
(require 'robe)

(defun fake-company-robe (orig-fun &rest args)
  "robe-mode should be true to use company-robe backend without enable robe-mode."
  (let ((robe-mode t))
    (apply orig-fun args)))
(advice-add 'company-robe :around #'fake-company-robe)

(eval-after-load 'company
  '(progn
     (add-hook 'ruby-mode-hook
               (lambda ()
                 (let ((origin-backends company-backends))
                   (set (make-local-variable 'company-backends)
                        (add-to-list 'origin-backends 'company-robe)))))
     (add-hook 'python-mode-hook
               (lambda ()
                 (let ((origin-backends company-backends))
                   (set (make-local-variable 'company-backends)
                        (add-to-list 'origin-backends '(company-anaconda :with company-capf))))))))

;;;Override
(defun inf-ruby-console-rails (dir)
  "Run Rails console in DIR."
  (interactive "D")
  (let ((with-bundler (file-exists-p "Gemfile")))
    (run-ruby (concat (when with-bundler "bundle exec ")
                      "rails console development")
              "robe")))

(defun start-robe-before-complete-in-rails (manually)
  "Start robe before complete in rails app."
  (when (and manually
             (bound-and-true-p projectile-rails-mode)
             (not robe-running))
    (robe-start t)))
(add-hook 'company-completion-started-hook 'start-robe-before-complete-in-rails)

(global-company-mode 1)

(provide 'misc-company)
;;; misc-company.el ends here
