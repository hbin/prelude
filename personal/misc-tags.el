;; misc-gtags.el --- Navigation between method definitions
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.

;;; Code:
;; (prelude-require-packages '(projectile helm-gtags))

;; (require 'helm-gtags)
;; (custom-set-variables
;;  '(tags-add-tables nil)
;;  '(tags-revert-without-query t)
;;  '(helm-gtags-auto-update t)
;;  '(helm-gtags-mode-name " Gtags"))

;; (eval-after-load "helm-gtags"
;;   '(progn
;;      (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag-from-here)
;;      (define-key helm-gtags-mode-map (kbd "M-*") 'helm-gtags-pop-stack)
;;      (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;;      (define-key helm-gtags-mode-map (kbd "C-x C-i") 'helm-gtags-parse-file)))

;; ;;; Custom stuff
;; (setenv "GTAGSLABEL" "ctags")

;; (defun ruby-tag-at-point ()
;;   "Get ruby tag at point.

;; 1. thing at 'current_user'   get current_user;
;; 2. thing at '!current_user'  get current_user;
;; 3. thing at 'current_user!'  get current_user!;
;; 4. thing at 'current_user='  get current_user=;
;; 5. thing at 'current_user =' get current_user=;
;; 6. thing at 'current_user ==' get current_user;
;; 7. thing at 'current_user ||=' get current_user=;
;; Otherwise, get `find-tag-default symbol."
;;   (regexp-quote
;;    (if (member (symbol-name major-mode)
;;                '("ruby-mode" "rhtml-mode" "haml-mode" "slim-mode"))
;;        (let ((symbol (thing-at-point 'symbol))
;;              (remain (if (thing-at-point 'symbol)
;;                          (save-excursion
;;                            (let ((from (beginning-of-thing 'symbol))
;;                                  (to   (end-of-thing 'line)))
;;                              (and (> to from)
;;                                   (buffer-substring-no-properties from to)))))))
;;          (if (and symbol remain)
;;              (let ((sym (s-chop-prefixes '("!!" "!" "::" ":") symbol))
;;                    (rem (s-chop-prefixes '("!!" "!" "::" ":") remain)))
;;                (if (s-matches? (concat "^" sym "\\( *\\(||\\)?=[^=]\\)") rem)
;;                    (concat sym "=")
;;                  sym))
;;            (find-tag-default)))
;;      (find-tag-default))))

;; (defun helm-gtags--read-gtagslabel ()
;;   "Always use Ctags as a plug-in parser for `ruby-mode'."
;;   (if (memq major-mode '(ruby-mode))
;;       (getenv "GTAGSLABEL")
;;     (let ((labels '("default" "native" "ctags" "pygments")))
;;       (completing-read "GTAGSLABEL(Default: default): " labels nil t nil nil "default"))))

;; (defun helm-gtags-create-tags (dir label)
;;   (interactive
;;    (list (projectile-project-root)
;;          (helm-gtags--read-gtagslabel)))
;;   (let ((default-directory dir)
;;         (label (helm-gtags--read-gtagslabel))
;;         (proc-buf (get-buffer-create " *helm-gtags-create*")))
;;     (let ((proc (start-file-process "helm-gtags-create" proc-buf
;;                                     "gtags" "-q" (helm-gtags--label-option label))))
;;       (set-process-sentinel proc (helm-gtags--make-gtags-sentinel 'create)))))

;; (defun helm-gtags--find-tag-simple ()
;;   (or (getenv "GTAGSROOT")
;;       (locate-dominating-file default-directory "GTAGS")
;;       (let* ((tagroot (projectile-project-root))
;;              (label (helm-gtags--read-gtagslabel))
;;              (default-directory tagroot))
;;         (message "gtags is generating tags....")
;;         (unless (zerop (process-file "gtags" nil nil nil
;;                                      "-q" (helm-gtags--label-option label)))
;;           (error "Faild: 'gtags -q'"))
;;         tagroot)))

;; (defun helm-gtags-find-tag-from-here ()
;;   "Jump to definition."
;;   (interactive)
;;   (helm-gtags--common '(helm-source-gtags-tags) (ruby-tag-at-point)))

;; (provide 'misc-tags)
;;; misc-gtags.el ends here
