;; misc-gtags.el --- Navigation between method definitions
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:
(prelude-require-packages '(projectile ggtags))

(require 'ggtags)

(eval-after-load 'smart-shift
  '(progn
     (define-key ggtags-mode-map (kbd "M-]") 'smart-shift-right)))

;;; Custom stuff
(setq ggtags-use-sqlite3 nil)
(setq ggtags-use-idutils nil)
(setq ggtags-global-window-height nil)
(setq ggtags-enable-navigation-keys nil)
(setq ggtags-mode-sticky nil)
(setq ggtags-auto-jump-to-match 'history)
(setq ggtags-global-abbreviate-filename whitespace-line-column)
(setenv "GTAGSLABEL" "ctags")

;; Override it to create tags automatically.
(defun ggtags-tag-at-point ()
  "Get ggtags tag at point.

1. thing at 'current_user'   get current_user;
2. thing at '!current_user'  get current_user;
3. thing at 'current_user!'  get current_user!;
4. thing at 'current_user='  get current_user=;
5. thing at 'current_user =' get current_user=;
6. thing at 'current_user ==' get current_user;
7. thing at 'current_user ||=' get current_user=;
Otherwise, get `find-tag-default symbol."
  (regexp-quote
   (if (member (symbol-name major-mode)
               '("ruby-mode" "rhtml-mode" "haml-mode" "slim-mode"))
       (let ((symbol (thing-at-point 'symbol))
             (remain (if (thing-at-point 'symbol)
                         (save-excursion
                           (let ((from (beginning-of-thing 'symbol))
                                 (to   (end-of-thing 'line)))
                             (and (> to from)
                                  (buffer-substring-no-properties from to)))))))
         (if (and symbol remain)
             (let ((sym (s-chop-prefixes '("!!" "!" "::" ":") symbol))
                   (rem (s-chop-prefixes '("!!" "!" "::" ":") remain)))
               (if (s-matches? (concat "^" sym "\\( *\\(||\\)?=[^=]\\)") rem)
                   (concat sym "=")
                 sym))
           (find-tag-default)))
     (find-tag-default))))

(defun ggtags-ensure-project ()
  (when (projectile-project-p)
    (let ((gtags-file (expand-file-name "GTAGS" (projectile-project-root))))
      (unless (file-exists-p gtags-file)
        (ggtags-create-tags (projectile-project-root))))
    (ggtags-check-project)))

(defun projectile-regenerate-tags-if ()
  (if (memq major-mode '(ruby-mode))
      (projectile-regenerate-tags)))

(remove-hook 'projectile-idle-timer-hook 'projectile-regenerate-tags)
(add-hook 'projectile-idle-timer-hook 'projectile-regenerate-tags-if)

(provide 'misc-tags)
;;; misc-gtags.el ends here
