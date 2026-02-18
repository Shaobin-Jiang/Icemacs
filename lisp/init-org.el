;;; init-org.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Configuration for org mode

;;; Code:

(use-package org
  :ensure nil
  :defer t
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
			                   '((emacs-lisp . t)
                                 (python     . t)))
  (setq org-babel-python-command "/opt/homebrew/bin/python3"
        org-confirm-babel-evaluate nil
        org-todo-keywords
		'((sequence "TODO(t)" "PENDING(p)" "|" "DONE(d)" "CANCELLED(c)")
		  (sequence "SCHEDULED(s)" "|" "FINISHED(f)" "POSTPONED(p)"))
        org-log-done 'time))

(provide 'init-org)

;;; init-org.el ends here
