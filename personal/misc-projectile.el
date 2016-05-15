;; misc-projectile.el --- Configuration for Projectile
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:

;; VARS---------------------
(defvar prelude-mode-map nil)
(defvar projectile-mode-map nil)

(prelude-require-packages '(projectile))

;;; Customizes
(custom-set-variables
 '(projectile-enable-idle-timer t)
 '(projectile-idle-timer-hook nil)      ; Empty default hook
 '(projectile-ignored-projects '("/usr/local/")))

;;; Keybindings
(define-key prelude-mode-map [?\s-d] nil)
(define-key prelude-mode-map [?\s-p] nil)
(define-key prelude-mode-map [?\s-f] nil)
(define-key prelude-mode-map [?\s-g] nil)

(define-key projectile-mode-map (kbd "M-t") 'projectile-find-file)
(define-key projectile-mode-map (kbd "M-T") 'projectile-find-file-other-window)
(define-key projectile-mode-map (kbd "M-g") 'helm-projectile-ag)
(define-key projectile-mode-map (kbd "M-G") 'helm-do-ag-this-file)
(define-key projectile-mode-map (kbd "M-R") 'projectile-replace)
(define-key projectile-mode-map (kbd "C-x d") 'projectile-find-dir)
(define-key projectile-mode-map (kbd "C-x D") 'projectile-dired)
(define-key projectile-mode-map (kbd "C-x C-i") 'projectile-find-tag)
(define-key projectile-mode-map (kbd "C-x C-p") 'projectile-switch-project)

(defun projectile-completing-read (prompt choices &optional initial-input)
  "Override this method for global fuzzy match."
  (let ((prompt (projectile-prepend-project-name prompt)))
    (cond
     ((eq projectile-completion-system 'helm)
      (if (fboundp 'helm-comp-read)
          (helm-comp-read prompt choices
                          :initial-input initial-input
                          :candidates-in-buffer t
                          :fuzzy t   ;; Enable fuzzy matching
                          :must-match 'confirm)))
     ((eq projectile-completion-system 'grizzl)
      (if (and (fboundp 'grizzl-completing-read)
               (fboundp 'grizzl-make-index))
          (grizzl-completing-read prompt (grizzl-make-index choices))))
     (t (funcall projectile-completion-system prompt choices)))))

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'misc-projectile)

;;; misc-projectile.el ends here
