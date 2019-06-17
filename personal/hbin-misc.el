;;; hbin-misc --- miscellany
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

;; Miscellaneous Packages
(prelude-require-packages
 '(popwin whole-line-or-region key-chord projectile ag dimmer))

;;; Disable guru-mode completely
(require 'guru-mode)
(setq prelude-guru nil)

;;; Whole line or region
(require 'whole-line-or-region)
(whole-line-or-region-global-mode)
(diminish 'whole-line-or-region-local-mode)

;;; Whitespace
(diminish 'whitespace-mode)

;; auto-revert-mode
(add-hook 'after-revert-hook (lambda) () (run-hooks 'projectile-invalidate-cache))

;;; iSpell
(require 'ispell)
(setq ispell-dictionary "english")
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)
(add-hook 'message-mode-hook 'turn-on-flyspell)
;; (add-hook 'text-mode-hook 'turn-on-flyspell)
(diminish 'flyspell-mode)

;;; Popwin
(require 'popwin)
(popwin-mode 1)
(global-set-key (kbd "C-q") popwin:keymap)

(push '("*ag search*" :height 20) popwin:special-display-config)
(push '("*Go Test*" :noselect t :height 20) popwin:special-display-config)
(push '("*Gofmt Errors*" :noselect t :height 20) popwin:special-display-config)
(push '(completion-list-mode :noselect t :height 20) popwin:special-display-config)
(push '(compilation-mode :noselect t :height 20) popwin:special-display-config)
(push '("*Warnings*" :noselect t :dedicated :height 20) popwin:special-display-config)
(push '("*rspec-compilation*" :noselect t :height 20) popwin:special-display-config)
(push '("*go-guru-output*" :noselect t :height 15) popwin:special-display-config)

;; ;;; Highlight symbols
;; (custom-set-variables
;;  ;; '(hl-highlight-flexible-match t)
;;  '(hl-highlight-foreground-colors
;;    '("black" "snow" "snow" "snow" "black" "snow"
;;      "snow" "snow" "black" "snow" "snow" "snow"))
;;  '(hl-highlight-background-colors
;;    '( "gold" "DeepPink" "firebrick" "Orange" "green1" "DeepSkyBlue1"
;;       "dark blue" "blue violet" "gray90" "gray60" "gray30" "OliveDrab"))
;;  '(hl-highlight-save-file
;;    (expand-file-name "hl-save" prelude-savefile-dir)))

;; (require 'hl-anything)

;; Overriden
;; (defun hl-highlight-fontify (&optional current-line?)
;;   (unless (equal (buffer-name) " *NeoTree*")
;;     (save-excursion
;;       (if current-line?
;;           (font-lock-fontify-region (line-beginning-position) (line-end-position))
;;         (font-lock-fontify-region (point-min) (point-max))))))

;; (hl-highlight-mode +1)
;; (diminish 'hl-highlight-mode)
;; (global-set-key (kbd "M-m") 'hl-highlight-thingatpt-local)
;; (global-set-key (kbd "M-M") 'hl-unhighlight-all-local)
(global-set-key (kbd "M-'") 'highlight-symbol)
(global-set-key (kbd "M-m") 'highlight-symbol)
(global-set-key (kbd "M-M") 'highlight-symbol-remove-all)
(global-set-key (kbd "C-c '") 'highlight-symbol-remove-all)

;;; key chord mode
(require 'key-chord)
(key-chord-define-global "JJ" 'crux-switch-to-previous-buffer)
(key-chord-mode +1)

;;; ag
(require 'ag)
(setq ag-highlight-search t)
(setq ag-reuse-buffers t)
(setq ag-reuse-window nil)
(setq ag-project-root-function 'projectile-project-root)

;;; Smartparens
(require 'smartparens)
(setq sp-highlight-pair-overlay nil)     ; Do not highlight autoinserted pairs
(diminish 'smartparens-mode)

;;; Dimmer
(require 'dimmer)
(dimmer-mode 1)

;;; Ignore '/'
(require 'ffap)
(defvar ffap-c-commment-regexp "^/\\*+"
  "Matches an opening C-style comment, like \"/***\".")

(defadvice ffap-file-at-point (after avoid-c-comments activate)
  "Don't return paths like \"/******\" unless they actually exist.
This fixes the bug where ido would try to suggest a C-style
comment as a filename."
  (ignore-errors
    (when (and ad-return-value
               (string-match-p ffap-c-commment-regexp
                               ad-return-value)
               (not (ffap-file-exists-string ad-return-value)))
      (setq ad-return-value nil))))

(defadvice ffap-file-at-point (after ffap-file-at-point-after-advice ())
  "The ffap should ignore the '/'."
  (if (string= ad-return-value "/")
      (setq ad-return-value nil)))
(ad-activate 'ffap-file-at-point)

(require 'cl-lib)
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))
    ad-do-it))

(provide 'hbin-misc)
;;; hbin-misc.el ends here
