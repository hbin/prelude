;; misc-tags.el --- Navigation between method definitions
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.
;; This file is a variant version of `etags-select` which created by
;; Scott Frazer.

;;; Code:
(prelude-require-package '(projectile ggtags))

(require 'ggtags)

;;; Custom stuff
(setq ggtags-mode-sticky nil)

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
             (let ((sym (s-chop-prefixes '("!!" "!") symbol))
                   (rem (s-chop-prefixes '("!!" "!") remain)))
               (if (s-matches? (concat "^" sym "\\( *\\(||\\)?=[^=]\\)") rem)
                   (concat sym "=")
                 sym))
           (find-tag-default)))
     (find-tag-default))))

(defun ggtags-ensure-project ()
  (or (ggtags-find-project)
      (when (or (yes-or-no-p "File GTAGS not found; run gtags? ")
                (user-error "Aborted"))
        (ggtags-create-tags (projectile-project-root))
        (ggtags-check-project))))

(provide 'misc-tags)
;;; misc-tags.el ends here
