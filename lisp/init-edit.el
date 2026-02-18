;;; init-edit.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Enhances editing experience

;;; Code:

(use-package undo-tree
  :straight t
  :defer t
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
  :after evil
  :custom
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m))
  :config
  (avy-setup-default)
  (evil-define-key 'normal 'global (kbd "<leader> h p") 'avy-goto-char-2))

(provide 'init-edit)

;;; init-edit.el ends here
