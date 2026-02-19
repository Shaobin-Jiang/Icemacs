;;; init-edit.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Enhances editing experience

;;; Code:

(use-package undo-tree
  :straight t
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000)
  :config
  (setq undo-tree-history-directory-alist
		`(("." . ,(no-littering-expand-var-file-name "undo")))))


(use-package avy
  :straight t
  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m))
  :config
  (avy-setup-default))

(provide 'init-edit)

;;; init-edit.el ends here
