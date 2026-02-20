;;; init-org.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Configuration for org mode

;;; Code:

(use-package org
  :ensure nil
  :defer t
  :custom
  (org-agenda-files '("~/Documents/org/gtd.org"))
  (org-babel-python-command "/opt/homebrew/bin/python3")
  (org-capture-templates
   '(("t" "Todo" entry (file+headline  "~/Documents/org/gtd.org" "Tasks")
	  "* TODO %?\n:CREATED: %U\n:RELATED: %a\n:END:")
	 ("i" "Idea" entry (file "~/Documents/org/ideas.org")
	  "* %? :NOTE:\n:CREATED: %U\n:END:")
	 ))
  (org-confirm-babel-evaluate nil)
  (org-default-notes-file "~/Documents/org/notes.org")
  (org-directory "~/Documents/org")
  (org-log-done 'time)
  (org-todo-keywords
   '((sequence "TODO(t)" "PENDING(p)" "|" "DONE(d)" "CANCELLED(c)")
	 (sequence "SCHEDULED(s)" "|" "FINISHED(f)" "POSTPONED(p)")))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
			                   '((emacs-lisp . t)
                                 (python     . t))))

(provide 'init-org)

;;; init-org.el ends here
