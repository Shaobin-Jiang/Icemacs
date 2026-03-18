;;; init-look.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Better look for Icemacs

;;; Code:

(defun transparent-toggle (&optional state)
  "Sets the transparency state of the frame.
If `STATE' is 1, make frame transparent.  If `STATE' is specified but
takes any other value, make frame opaque.  If unspecified, toggle
transparency."
  (interactive)
  (let* ((frame (selected-frame))
		 (transparency (frame-parameter frame 'alpha))
		 (alpha '(90 90)))
	(cond
	 ((eq state 1)
	  (set-frame-parameter frame 'alpha alpha))
	 ((null state)
	  (if (equal transparency '(100 100))
		  (set-frame-parameter frame 'alpha alpha)
		(set-frame-parameter frame 'alpha '(100 100))))
	 (t
	  (set-frame-parameter frame 'alpha '(100 100))))))
(transparent-toggle 1)

(use-package treesit-auto
  :straight t
  :commands treesit-auto-install-all
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; `treesit-auto-install-all' automatically installs to ~/.emacs.d
  ;; This is a work-around to install it elsewhere by temporarily
  ;; overiding `locate-user-emacs-file'.
  (advice-add
   'treesit-auto-install-all :around
   (lambda (install &rest args)
	 (let ((orig-func (symbol-function 'locate-user-emacs-file)))
       (unwind-protect
           (progn
			 (fset
              'locate-user-emacs-file
			  (lambda (&rest fname)
				(if (and (consp fname)
						 (stringp (car fname))
						 (null (cdr fname))
						 (string= (car fname) "tree-sitter"))
					(expand-file-name "treesit/" no-littering-var-directory)
				  (apply orig-func fname))))
			 (apply install args)
			 (fset 'locate-user-emacs-file orig-func))))))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))


(use-package hl-todo
  :straight t
  :hook prog-mode)


(use-package indent-guide
  :straight t
  :hook
  (prog-mode . indent-guide-mode)
  :config
  (setq indent-guide-char "│"))


(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode))


(use-package pulsar
  :straight t
  :hook
  (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t
		pulsar-delay 0.025
		pulsar-iterations 10
		pulsar-face 'evil-ex-lazy-highlight)

  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'evil-yank)
  (add-to-list 'pulsar-pulse-functions 'evil-yank-line)
  (add-to-list 'pulsar-pulse-functions 'evil-delete)
  (add-to-list 'pulsar-pulse-functions 'evil-delete-line)
  (add-to-list 'pulsar-pulse-functions 'evil-jump-item)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-next-hunk)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-previous-hunk))


(use-package doom-modeline
  :straight t
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-name t)
  (doom-modeline-vcs-max-length 25)
  :config
  (setq doom-modeline-icon t)
  :hook
  (after-init . doom-modeline-mode))


(use-package nerd-icons
  :straight t
  :defer t)


(use-package nerd-icons-corfu
  :straight t
  :after (:all corfu))


(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))


(use-package nerd-icons-completion
  :straight t
  :after (:all nerd-icons marginalia)
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))


(use-package catppuccin-theme
  :straight t
  :config
  (defconst catppuccin-flavors
	'(latte frappe macchiato mocha)
	"List of catppuccin flavors.")
  (defun set-catppuccin-flavor (flavor)
	"Set the flavor of catppuccin theme."
	(interactive
	 (list
	  (intern
	   (completing-read
		"Catppuccin flavor: "
		(mapcar #'symbol-name catppuccin-flavors) nil t))))
	(unless (memq flavor catppuccin-flavors)
	  (user-error "Invalid flavor %S (expected one of %S)" flavor catppuccin-flavors))
	(setq catppuccin-flavor flavor)

	(custom-set-faces
	 `(diff-hl-change ((t (:background unspecified :foreground ,(catppuccin-get-color 'blue))))))
	(custom-set-faces
	 `(diff-hl-delete ((t (:background unspecified :foreground ,(catppuccin-get-color 'red))))))
	(custom-set-faces
	 `(diff-hl-insert ((t (:background unspecified :foreground ,(catppuccin-get-color 'green))))))
	(load-theme 'catppuccin :no-confirm))
  (set-catppuccin-flavor 'frappe))

(provide 'init-look)

;;; init-look.el ends here
