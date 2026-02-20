;;; init-ibuffer.el --- Shaobin Jiang's emacs configuration  -*- lexical-binding: t -*-

;; Author: Shaobin Jiang
;; URL: https://github.com/Shaobin-Jiang/Icemacs

;;; Commentary:
;; Configuration for ibuffer

;;; Code:

(defun ice/ibuffer-open ()
  "Opens the buffer and closes the ibuffer window."
  (interactive nil ibuffer-mode)
  (let ((buf (ibuffer-current-buffer t)))
	(quit-window t)
    (switch-to-buffer buf)))

(defun ice/ibuffer-split (&optional split)
  "Split the buffer at point.
If SPLIT is non nil, the file is opened in a horizontal split.
Otherwise, it is opened in a vsplit."
  (interactive nil ibuffer-mode)
  (let ((buf (ibuffer-current-buffer t)))
    (quit-window t)
    (if split
        (evil-window-split)
      (evil-window-vsplit))
	(switch-to-buffer buf)))

(defun ice/ibuffer-quit ()
  "Quits ibuffer buffer and kills the buffer when doing so."
  (interactive nil ibuffer-mode)
  (quit-window t))

(defun ice/add-ibuffer-bindings ()
  "Add keymap for ibuffer mode.  This is hooked to ibuffer mode."
  (evil-define-key 'normal ibuffer-mode-map
	(kbd "V") 'ice/ibuffer-split
	(kbd "S") (lambda () (interactive) (ice/ibuffer-split t))
	(kbd "g g") (lambda () (interactive) (evil-goto-line 4))
	(kbd "G") (lambda () (interactive)
				(evil-goto-line (- (line-number-at-pos (point-max)) 2)))
	(kbd "q") 'ice/ibuffer-quit))

(use-package ibuffer
  :ensure nil
  :hook
  (ibuffer-mode . ice/add-ibuffer-bindings)
  :bind (:map ibuffer-mode-map
              ("RET"      . ice/ibuffer-open)
              ("<return>" . ice/ibuffer-open)))

(provide 'init-ibuffer)

;;; init-ibuffer.el ends here
