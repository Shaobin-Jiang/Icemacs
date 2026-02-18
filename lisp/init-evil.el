;;; init-evil.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin-Jiang <shaobin-jiang@outlook.com>
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Configuration for evil mode and a set of evil-related packages including
;;
;; - evil-collection
;; - evil-surround
;; - evil-matchit
;; - evil-anzu

;;; Code:

(use-package evil
  :straight t
  :defer t
  :hook
  (after-init . evil-mode)
  :init
  ;; As instructed in evil docs: "preferably before Evil is loaded"
  (setq evil-split-window-below t
	    evil-vsplit-window-right t
	    evil-want-fine-undo t
	    evil-want-C-u-scroll t
	    evil-echo-state nil
	    evil-want-empty-ex-last-command nil
        evil-search-module "evil-search")
  :config
  (evil-set-undo-system 'undo-tree)
  (setq evil-leader/in-all-states t
		evil-want-fine-undo t)

  (defun ice/evil-recenter-after-motion (&rest _args)
    "Recenter window after Evil vertical motions."
    (recenter))

  ;; Another safe guard over setting `scroll-margin'
  ;; Since `maximum-scroll-margin' can only be up to 0.5 and the line
  ;; number of the window can be an odd number, this would cause weird
  ;; stuttering scrolling behavior.
  (advice-add 'evil-next-line			:after #'ice/evil-recenter-after-motion)
  (advice-add 'evil-previous-line		:after #'ice/evil-recenter-after-motion)
  (advice-add 'evil-scroll-down			:after #'ice/evil-recenter-after-motion)
  (advice-add 'evil-scroll-up			:after #'ice/evil-recenter-after-motion)
  (advice-add 'evil-scroll-page-down	:after #'ice/evil-recenter-after-motion)
  (advice-add 'evil-scroll-page-up		:after #'ice/evil-recenter-after-motion)
  (advice-add 'evil-search-next			:after #'ice/evil-recenter-after-motion)
  (advice-add 'evil-search-previous		:after #'ice/evil-recenter-after-motion)
  (advice-add 'evil-forward-paragraph	:after #'ice/evil-recenter-after-motion)
  (advice-add 'evil-backward-paragraph	:after #'ice/evil-recenter-after-motion)

  ;; Unhighlight search with ESC
  (advice-add 'evil-force-normal-state :after #'evil-ex-nohighlight)

  (evil-set-leader '(normal visual) (kbd "SPC"))

  (evil-define-operator ice/join (beg end)
    "Join the selected lines. Slight modification to evil-join:
<count>J joins <count + 1> lines"
    :motion evil-line
    (evil-join beg (+ end 1)))

  (evil-define-key 'normal 'global
    (kbd "J") 'ice/join
	(kbd "C-t") (lambda () (interactive)
				  (evil-window-vsplit)
				  (term "/bin/zsh"))

	(kbd "<leader> P") 'consult-yank-from-kill-ring

    (kbd "<leader> b h") 'switch-to-prev-buffer
    (kbd "<leader> b l") 'switch-to-next-buffer
    (kbd "<leader> b c") 'kill-buffer
    (kbd "<leader> b d") (lambda () (interactive) (kill-buffer))
    (kbd "<leader> b p") 'consult-buffer
    (kbd "<leader> b i") 'ibuffer

    (kbd "<leader> u f") 'dired-jump
	(kbd "<leader> u u") 'undo-tree-visualize
    (kbd "<leader> u c") '(lambda () (interactive)
							(find-file (read-file-name
										"Config: "
										(expand-file-name user-emacs-directory)
										nil
										(confirm-nonexistent-file-or-buffer))))
	(kbd "g c") 'ice/comment
	(kbd "g C") 'ice/comment-operator)

  (evil-define-key 'visual 'global (kbd "g c")
    (lambda ()
      (interactive)
      (if (use-region-p)
          (comment-or-uncomment-region (region-beginning) (region-end)))))

  (evil-define-key '(normal visual insert) 'global
    (kbd "C-z") '(lambda () (interactive) (evil-undo 1) (evil-normal-state))
    (kbd "C-s") '(lambda () (interactive) (save-buffer) (evil-normal-state)))

  (evil-define-key '(normal visual) 'global
    (kbd "\\") '(lambda () (interactive)
				  (setq this-command 'evil-use-register evil-this-register ?_)))

  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "<leader> l d") (lambda () (interactive)
						   (xref-find-definitions (symbol-name (symbol-at-point))))
    (kbd "<leader> l h") (lambda () (interactive)
						   (describe-symbol (symbol-at-point)))
    (kbd "<leader> l n") 'flymake-goto-next-error
    (kbd "<leader> l p") 'flymake-goto-prev-error
    (kbd "<leader> l t") 'consult-flymake)


  (defun ice/lsp-describe-thing-at-point ()
    "Show hover documentation and jump to *lsp-help* buffer."
    (interactive)
    (lsp-describe-thing-at-point)
    (let ((help-buffer "*lsp-help*"))
      (when (get-buffer help-buffer)
        (switch-to-buffer-other-window help-buffer))))

  ;; =================================================================
  ;; This part is for commenting using the gc* keymap like neovim
  ;; =================================================================

  (defun ice/comment ()
    "Comment or uncomment depending on the context and the key press
(evil motion or a few special keys). When invoked, starts an event loop
that only ends when a non-digit key is pressed...

with the exception when the first non-digit key is 'g'. In this case,
the event loop further listens for one more key press. This is because
'gg' is a motion in evil mode and it is a necessity to be capable of
commenting / uncommenting to the first line of the buffer.

The function behaves when the following keys are the terminator key:

- Escape: quits the event loop
- c: toggles comment for the current line
- A: adds comment at the end of the current line
- O: adds comment above current line
- o: adds comment below current line
- other keys: act as though triggered after ice/comment-operator"
    (interactive)
    (let ((beg (point))
          (listen-to-gg nil)
          (done nil)
          (escape nil)
          (digits "")
          (terminator ""))
      (while (not done)
        (let ((ev (read-event)))
          (setq temp ev)
          (cond
           ;; Accept 0..9
           ((and (characterp ev) (>= ev ?0) (<= ev ?9))
            (setq digits (concat digits (string ev))))

           ;; gg
           ((and (eq ev ?g) (not listen-to-gg))
            (setq listen-to-gg t
                  terminator "g"))

           ;; Escape; this has to be handled differently in term / gui
           ((or (eq ev 27) (eq ev 'escape))
            (setq done t
                  escape t))

           ;; Anything else ends interception
           (t
            (setq done t
                  terminator (concat terminator (string ev)))))))

      (cond
       (escape nil)
       ((string= terminator "c") (ice/toggle-line-comment))
       ((string= terminator "O") (evil-open-above 1) (comment-dwim nil))
       ((string= terminator "o") (evil-open-below 1) (comment-dwim nil))
       ((string= terminator "A") (evil-append-line 1) (comment-dwim nil))
       (t (execute-kbd-macro (kbd (concat "gC" digits terminator)))))
      (list :digits digits :terminator terminator)))

  (defun ice/toggle-line-comment ()
    "Toggle comment for current line. Decides which function to uses
depending on whether the line is empty."
    (interactive)
    (if (eq (line-beginning-position) (line-end-position))
        (comment-dwim nil)
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

  (evil-define-operator ice/comment-operator (beg end type)
    "Toggle comment for text motioned over."
    :motion: evil-line
    (interactive "<R>")
    (if (and (eq beg end) (eq beg (line-beginning-position)))
        (comment-dwim nil)
      (comment-or-uncomment-region beg end)))

  ;; =================================================================
  ;; This part is for automatic im switching upon state (mode) change
  ;; =================================================================

  (defvar ice/previous-im-code nil
    "Previous ime state from macism before leaving Evil insertstate.")

  (defun ice/macism-set-im (ime)
    "Set the ime state to IME asynchronously using macism."
    (when ime
      (let ((process (start-process "macism" nil "macism" ime))))))

  (defun ice/evil-insert-leave ()
    "Use ABC ime and store previous ime state on leaving insert state."
    (make-process
     :name "get-ime-state"
     :command '("macism")
     :filter (lambda (_proc output)
               ;; Trim the output and call the callback
               (funcall (lambda ()(setq ice/previous-im-code (string-trim output)))))
     :noquery t)
    (ice/macism-set-im "com.apple.keylayout.ABC"))

  (defun ice/evil-insert-enter ()
    "Restore the previous ime state asynchronously."
    (when ice/previous-im-code
      (ice/macism-set-im ice/previous-im-code)
      (setq ice/previous-im-code nil)))

  (add-hook 'evil-insert-state-exit-hook 'ice/evil-insert-leave)
  (add-hook 'evil-insert-state-entry-hook 'ice/evil-insert-enter)

  (evil-mode 1))

(use-package evil-collection
  :straight t
  :defer t
  :custom
  (evil-collection-want-find-usages-bindings t)
  :config
  (setq evil-collection-key-blacklist '("SPC"))
  :hook
  (evil-mode . evil-collection-init))


(use-package evil-surround
  :straight t
  :after evil-collection
  :config
  (global-evil-surround-mode 1))


(use-package evil-matchit
  :straight t
  :after evil-collection
  :config
  (global-evil-matchit-mode 1))


(use-package evil-anzu
  :straight t
  :config
  (global-anzu-mode 1))

(provide 'init-evil)

;;; init-evil.el ends here
