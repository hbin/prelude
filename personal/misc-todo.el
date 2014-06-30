;; misc-todo.el --- Org ToDo-List
;;
;; Copyright (C) 2012-2014 Huang Bin
;;
;; Author: Huang Bin <huangbin88@foxmail.com>
;; Version: 1.0.0

;;; Commentary:

;; This file is not part of GNU Emacs.
;; Use Org-mode for my ToDo List

;;; Code:

(defvar hbin-todo-list-file
  (let ((todo-file-name "todo.org")
        (dropbox-dir "~/Dropbox/"))
    (expand-file-name todo-file-name
                      (if (file-directory-p dropbox-dir)
                          dropbox-dir
                        prelude-savefile-dir)))
  "The file stores my todo list.")

;;; Startup
(setq initial-scratch-message nil)
(setq initial-buffer-choice hbin-todo-list-file)

(defun hbin-find-todo-list-file ()
  "Find my todo-list file conveniently."
  (interactive)
  (find-file hbin-todo-list-file))

(eval-after-load 'key-chord
  '(progn
     (key-chord-define-global "TT" 'hbin-find-todo-list-file)))

(provide 'misc-todo)
;;; misc-todo.el ends here
