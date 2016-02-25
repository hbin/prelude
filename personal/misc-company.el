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
(prelude-require-packages '(company))

(require 'company)

(setq company-idle-delay nil)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
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

(defadvice company-robe
    (before fake-robe-mode (command &optional arg &rest ignore) activate)
  (unless robe-mode (setq robe-mode t)))

;; (global-company-mode 1)

(provide 'misc-company)
;;; misc-company.el ends here
