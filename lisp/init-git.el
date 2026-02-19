;;; init-git.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Configuration for git-related tools

;;; Code:

(use-package diff-hl
  :straight t
  :defer t ; WTF I cannot remove this despite having hook below?
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
								  (ignored . "i"))))


(use-package magit
  :straight t
  :defer t
  :config
  (setopt magit-format-file-function #'magit-format-file-nerd-icons)
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-define-key 'normal magit-status-mode-map
	(kbd "q") 'magit-bury-or-kill-buffer)
  (evil-define-key '(normal insert) with-editor-mode-map
	;; Default C-c C-k throws weird error to which I have no solution.
	;; However, commit with C-c C-c, which calls `with-editor-finish'
	;; works fine. Also, commiting an empty message will not actually
	;; commit anything.
	;; This does affect not only git commit, but all other with-editor
	;; mode buffers as well, but they still have ZQ.
	(kbd "C-c C-k") (lambda () (interactive)
					  (delete-region (point-min) (point-max))
					  (with-editor-finish t))))

(provide 'init-git)

;;; init-git.el ends here
