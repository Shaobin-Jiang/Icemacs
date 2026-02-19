;;; init-prog.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Configuration for programming, including:
;;
;; - corfu / cape           - completion
;; - eldoc		            - viewing documentation
;; - flymake	            - diagnostics
;; - lsp-mode	            - you know what it is
;; - format-all	            - formatting
;; - yasnippet / yasnippets - snippets

;;; Code:

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-echo-area-display-truncation-message nil)
  :init
  (global-eldoc-mode))


(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-margin-indicators-string
   '((error "!»" compilation-error)
	 (warning "»" compilation-warning)
     (note "»" compilation-info))))


(use-package corfu
  :straight t
  :defer t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1) ; Trigger completion after typing 1 character
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5)
  (corfu-max-width 50)
  (corfu-min-width 50)
  (corfu-popupinfo-delay 0.5)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (keymap-unset corfu-map "RET")
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode t)
  :bind (:map corfu-map
			  ("C-j" . corfu-next)
			  ("C-k" . corfu-previous)
			  ("C-M-i" . corfu-quit)
			  ("TAB" . corfu-insert)))


(use-package cape
  :straight t
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))


(use-package lsp-mode
  :straight t
  :hook ((lsp-mode . evil-normalize-keymaps)
         (lsp-mode . lsp-enable-which-key-integration)
         ((js-mode
		   tsx-ts-mode
		   typescript-ts-base-mode
		   css-mode
		   js-ts-mode
		   prisma-mode
		   python-base-mode
		   python-mode
		   web-mode) . lsp-deferred))
  :commands lsp
  :custom
  (lsp-inlay-hint-enable t)
  (lsp-completion-provider :none)
  (lsp-log-io nil)
  (lsp-idle-delay 0.5)
  (lsp-keep-workspace-alive nil)
  ;; Core settings
  (lsp-enable-xref t)                       ; Enable cross-references
  (lsp-auto-configure t)
  (lsp-eldoc-enable-hover t)
  (lsp-enable-links nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-suggest-server-download t)
  (lsp-enable-symbol-highlighting t)
  (lsp-enable-text-document-color t)
  ;; Modeline settings
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)     ; Use `flymake' instead
  (lsp-modeline-workspace-status-enable t)  ; Display "LSP" in the modeline when enabled
  (lsp-session-file (expand-file-name ".lsp-session" no-littering-var-directory))
  (lsp-signature-doc-lines 1)
  (lsp-eldoc-render-all t)
  ;; Completion settings
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t)
  (lsp-enable-snippet nil)
  (lsp-completion-show-kind t)

  (lsp-lens-enable t)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-semantic-tokens-enable nil)
  :config
  (evil-define-key 'normal lsp-mode-map
    (kbd "<leader> l c") 'lsp-execute-code-action
    (kbd "<leader> l d") 'lsp-find-definition
    (kbd "<leader> l h") 'ice/lsp-describe-thing-at-point
    (kbd "<leader> l i") 'lsp-find-implementation
    (kbd "<leader> l n") 'flymake-goto-next-error
    (kbd "<leader> l p") 'flymake-goto-prev-error
    (kbd "<leader> l r") 'lsp-rename
    (kbd "<leader> l R") 'lsp-find-references
    (kbd "<leader> l t") 'consult-flymake))


(use-package format-all
  :straight t
  :defer t
  :config
  (setq-default format-all-formatters
                '(("Python"     (black))
				  ("JavaScript" (prettier)))))


(use-package yasnippet
  :straight t
  :bind (("M-s" . yas-insert-snippet)
		 (:map yas-minor-mode-map ("TAB" . nil)))
  :config
  (yas-global-mode))


(use-package yasnippet-snippets
  :straight t
  :defer t)

(provide 'init-prog)

;;; init-prog.el ends here
