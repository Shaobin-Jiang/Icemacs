;;; init-git.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Configuration for git-related tools

;;; Code:

(use-package diff-hl
  :defer t
  :straight t
  :hook
  (find-file . (lambda ()
                 (global-diff-hl-mode)
                 (diff-hl-flydiff-mode) ; Automatically refresh diffs.
                 (diff-hl-margin-mode)))
  :custom
  (diff-hl-side 'left)
  (diff-hl-margin-symbols-alist '((insert . "┃")
								  (delete . "-")
								  (change . "┃")
								  (unknown . "┆")
								  (ignored . "i")))
  :config
  (evil-define-key 'normal 'global
	(kbd "<leader> g d") 'diff-hl-show-hunk
	(kbd "<leader> g s") 'diff-hl-stage-current-hunk
	(kbd "<leader> g n") 'diff-hl-next-hunk
	(kbd "<leader> g p") 'diff-hl-previous-hunk))


(use-package magit
  :straight t
  :after evil
  :config
  (setopt magit-format-file-function #'magit-format-file-nerd-icons)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (evil-define-key 'normal 'global (kbd "<leader> g t") 'magit-project-status)
  (evil-define-key 'normal magit-status-mode-map (kbd "q") 'magit-bury-or-kill-buffer))

(provide 'init-git)

;;; init-git.el ends here
