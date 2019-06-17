;;; hbin-kbds --- Personal kbds
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Key bindings.

;;; Code:

;; Swap the mapping of Meta and Super if Mac OSX
(when (string= system-type "darwin")
  (if (eq mac-command-modifier 'super)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super))))

;; Define my own keymap
;; (defvar hbin-map (make-sparse-keymap))
;; (global-set-key (kbd "C-q") hbin-map)

;; Disable several ido kbds
(require'ido)
(defun ido-my-keys ()
  "Add my keybindings for ido."
  (define-key ido-file-dir-completion-map [(meta ?b)] 'backward-word)
  (define-key ido-file-dir-completion-map [(meta ?d)] 'kill-word)
  (define-key ido-file-dir-completion-map [(meta ?f)] 'forward-word)
  (define-key ido-file-dir-completion-map "\C-x\C-f" nil)
  (define-key ido-common-completion-map "\C-a" 'crux-move-beginning-of-line)
  (define-key ido-common-completion-map "\C-e" 'move-end-of-line)
  (define-key ido-common-completion-map "\C-f" 'forward-char))
(add-hook 'ido-setup-hook 'ido-my-keys)

;;; Unbinding keys
(global-unset-key (kbd "C-x C-p"))      ; used to mark page
(global-unset-key (kbd "C-x C-n"))      ; used to set-goal-column
(global-unset-key (kbd "C-x 0"))        ; used to delete-window
(global-unset-key (kbd "C-x 1"))        ; used to delete-other-windows
(global-unset-key (kbd "C-x 2"))        ; used to split-window-vertically
(global-unset-key (kbd "C-x 3"))        ; used to split-window-horizontally
(global-unset-key (kbd "C-x C-l"))      ; used to lowercase the buffer/region
(global-unset-key (kbd "s-n"))
(global-unset-key (kbd "s-p"))

;; Align the code
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Help command
(global-set-key (kbd "<f1>") 'help-command)

;; Back killing
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(define-key key-translation-map [?\C-h] [?\C-?])

;; Vim like open previous/next line
(global-set-key (kbd "C-o") 'crux-smart-open-line)
(global-set-key (kbd "C-M-o") 'crux-smart-open-line-above)

;; Window switching
(define-key prelude-mode-map (kbd "M-0") 'delete-window)
(define-key prelude-mode-map (kbd "M-1") 'delete-other-windows)
(define-key prelude-mode-map (kbd "M-2") 'split-window-vertically)
(define-key prelude-mode-map (kbd "M-3") 'split-window-horizontally)
(define-key prelude-mode-map (kbd "M-k") 'kill-this-buffer)
(define-key prelude-mode-map (kbd "s-0") 'balance-windows)

;; Jump between window
(global-set-key [remap other-window] 'other-window)
(define-key prelude-mode-map (kbd "M-o") 'other-window)
(define-key prelude-mode-map (kbd "M-O") (lambda () (interactive) (other-window -1)))
(define-key prelude-mode-map (kbd "C-x o") 'ace-window)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)
(global-set-key (kbd "M-r") 'highlight-symbol-query-replace)

(prelude-require-packages
 '(crux expand-region highlight-symbol multiple-cursors smart-shift find-file-in-project))

(require 'crux)
(crux-with-region-or-line comment-or-uncomment-region)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "M-h") 'er/expand-region)

;;; Highlight symbols
(require 'highlight-symbol)
(setq highlight-symbol-colors
      '("Yellow" "YellowGreen" "pink" "Purple" "PeachPuff" "RoyalBlue" "LightBlue" "SlateBlue" "SteelBlue"
        "violet" "cyan" "SeaGreen" "#A0522D" "SpringGreen" "LimeGreen" "LightSeaGreen" "MistyRose" "Magenta"
        "orange" "PaleVioletRed" "#FF6347" "grey" "brown" "RosyBrown" "SandyBrown"))
(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

;;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "S-<down>") 'mc/mark-next-lines)
(global-set-key (kbd "S-<up>") 'mc/mark-previous-lines)
(global-set-key (kbd "C-*") 'mc/mark-all-symbols-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-symbol-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-symbol-like-this)

(require 'smart-shift)
(global-set-key (kbd "M-[") 'smart-shift-left)
(global-set-key (kbd "M-]") 'smart-shift-right)

(require 'find-file-in-project)
(define-key prelude-mode-map (kbd "M-T") 'find-file-in-project)

;; Goto line
(global-set-key (kbd "s-l") 'goto-line)

(global-set-key (kbd "C-j") 'reindent-then-newline-and-indent)
(global-set-key (kbd "C-S-J") 'crux-top-join-line)

;;; Winner Mode
(winner-mode 1)
(global-set-key (kbd "C-x <left>") 'winner-undo)
(global-set-key (kbd "C-x <right>") 'winner-redo)

(define-key prelude-mode-map (kbd "s-+") 'enlarge-window-horizontally)
(define-key prelude-mode-map (kbd "s-=") 'enlarge-window-horizontally)
(define-key prelude-mode-map (kbd "s--") 'shrink-window-horizontally)
(define-key prelude-mode-map (kbd "C-s-+") 'enlarge-window)
(define-key prelude-mode-map (kbd "C-s-=") 'enlarge-window)
(define-key prelude-mode-map (kbd "C-s--") 'shrink-window)

(provide 'hbin-kbds)
;;; hbin-kbds.el ends here
