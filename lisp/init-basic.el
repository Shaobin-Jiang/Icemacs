;;; init-basic.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin-Jiang <shaobin-jiang@outlook.com>
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Basic settings that occur during startup.

;;; Code:

(setq read-process-output-max (* 1024 1024 4))

(use-package emacs
  :ensure nil
  ;; Prefer `customize' over `setq' if it is "customizable".
  :custom
  (auto-save-default nil)
  (case-fold-search nil)                    ; Use case sensitive search
  (comment-empty-lines t)
  (create-lockfiles nil)
  (custom-file "~/.local/share/emacs/custom-vars.el")
  (default-frame-alist '((undecorated           . t)
                         (drag-internal-border  . 1)
                         (internal-border-width . 5)))
  (delete-by-moving-to-trash t)
  (display-line-numbers-type 'relative)
  (global-auto-revert-non-file-buffers t)
  (help-window-select t)
  (history-length 25)
  (inhibit-startup-message t)
  (initial-scratch-message "")
  (ispell-dictionary "en_US")
  (make-backup-files nil)
  (maximum-scroll-margin 0.5)
  (ring-bell-function 'ignore)
  (scroll-margin 999)
  (shell-file-name "/bin/zsh")
  (split-width-threshold 300)               ; Prevent automatic window splitting if the window width exceeds 300 pixels
  (switch-to-buffer-obey-display-actions t) ; Cleverer `switch-to-buffer'
  (tab-width 4)
  (treesit-font-lock-level 4)               ; Use advanced font locking for Treesit mode
  (truncate-lines t)                        ; Enable line truncation to avoid wrapping long lines
  (use-dialog-box nil)                      ; Disable dialog boxes in favor of minibuffer prompts
  (use-short-answers t)                     ; Use short answers in prompts for quicker responses (y instead of yes)
  (warning-minimum-level :emergency)

  :init
  (indent-tabs-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)

  (column-number-mode 1)
  (delete-selection-mode 1)   ; Enable replacing selected text with typed text
  (electric-pair-mode 1)
  (file-name-shadow-mode 1)
  (global-auto-revert-mode 1) ; Keep buffers up to date with corresponding files
  (global-display-fill-column-indicator-mode 1)
  (global-display-line-numbers-mode 1)
  (global-hl-line-mode 1)
  (pixel-scroll-precision-mode 1)
  (recentf-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (xterm-mouse-mode 1)

  (modify-coding-system-alist 'file "" 'utf-8)

  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font Mono" :height 180)
  (set-fontset-font t 'han (font-spec :family "霞鹜文楷等宽" :size 22))
  (set-frame-parameter (selected-frame) 'alpha '(90 90))

  (add-hook 'after-init-hook
            (lambda ()
              ;; Insert a welcome message in the *scratch* buffer.
              (with-current-buffer (get-buffer-create "*scratch*")
                (insert (format ";;    Welcome to Icemacs!
;;
;;    Loading time : %s
;;    Packages     : %s
"
								(emacs-init-time)
								(length (hash-table-keys straight--recipe-cache)))))))

  :config
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Skip special buffers when switching to prev / next buffer."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)

  ;; Use symbol │ for vertical devisor instead of |.
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  :bind
  (("TAB" . (lambda () (interactive) (insert-tab))))

  :hook
  (after-init . which-key-mode))


(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(
	 ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ("\\*\\(Flymake diagnostics\\|xref\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))))
  :config
  (defun ice/push-button-on-file-same-window ()
	"Perform `push-button' in help window but open file in same window."
	(interactive)
	(let ((cwc (current-window-configuration))
          (hb (current-buffer))
          (file? (button-get (button-at (point)) 'help-args)))
      (funcall
       `(lambda ()
          (defun ice/push-button-on-file-same-window-internal ()
			(if (> (length ',file?) 1)
				(let ((cb (current-buffer)))
                  (set-window-configuration ,cwc)
                  (switch-to-buffer cb)
                  (kill-buffer ,hb)))))))
	(call-interactively 'push-button)
	(run-with-timer 0.01 nil 'ice/push-button-on-file-same-window-internal))

  (define-key button-map (kbd "RET") 'ice/push-button-on-file-same-window))


(use-package no-littering
  :straight t
  :config
  (setq yas-snippet-dirs (list (locate-user-emacs-file "snippets"))))

(provide 'init-basic)

;;; init-basic.el ends here
