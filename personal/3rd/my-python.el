;;; package --- Summary
;;; Commentary:

;; My Python setting.

;;; Code:

;; elpy for python
(elpy-enable)

;; show elpy buffer in the current window
(defun my-elpy-shell-switch-to-shell ()
  "Switch to inferior Python process buffer."
  (interactive)
  (pop-to-buffer-same-window (process-buffer (elpy-shell-get-or-create-process))))

;; using ipython as the default python console
(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter-args "--pylab")

;; remap up key in python shell
(define-key comint-mode-map (kbd "<up>") 'comint-previous-matching-input-from-input)

;; update modifying date field in the comment area (for python)
(defun my-python-modify-date ()
  "Update modifying date field in the comment area (for python)."
  (interactive)
  (save-excursion
    (let ((time-format "%Y-%m") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "  modify" nil t))
      (if pt1
          (progn
            (message "done")
            (search-forward "gmail.com), ")
            (setq pt1 (point))
            (end-of-line)
            (setq pt2 (point))
            (delete-region pt1 pt2)
            (insert (format-time-string time-format (current-time))))
        (message "modify xxx not found")))))

;; update creating date in the comment area (for python)
(defun my-python-create-date ()
  "Update creating date in the comment area (for python)."
  (interactive)
  (save-excursion
    (let ((time-format "%Y-%m") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "  create" nil t))
      (if pt1
          (progn
            (message "done")
            (search-forward "gmail.com), ")
            (setq pt1 (point))
            (end-of-line)
            (setq pt2 (point))
            (delete-region pt1 pt2)
            (insert (format-time-string time-format (current-time))))
        (message "create xxx not found")))))

;; save hook
(defun my-python-save-hook ()
  "My hook for saving python file (*.py)."
  (if (eq major-mode 'python-mode)
      (progn
        (message "%s is python-mode" (buffer-file-name))
        (my-python-modify-date))))
(add-hook 'before-save-hook 'my-python-save-hook)

;; python mode (save C-c C-p for other use)
(eval-after-load "python"
  '(define-key python-mode-map (kbd "C-c C-p") nil))

;; reserve M-. for find-tag
(eval-after-load "elpy"
  '(define-key elpy-mode-map (kbd "M-.") nil))

(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "H-c") 'my-python-create-date)
             (setq python-indent-offset 2)))

;; Use only own snippets, do not use bundled ones
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-reload-all)

;; Select all lines belong to the sub-block
(defun my-nav-expand-to-sub-block ()
  "Select all lines belong to the sub-block."
  (interactive)
  (save-excursion
    (setq elpy-nav-expand--initial-position (point))
    (python-nav-forward-statement)
    (let ((indentation (current-indentation)))
      (if (= indentation 0)
          (mark-whole-buffer)
        (while (<= indentation (current-indentation))
          (python-nav-backward-statement))
        (next-line)
        (push-mark (point) nil t)
        (while (<= indentation (current-indentation))
          (python-nav-forward-statement))
        (python-nav-backward-statement)))
    (elpy-nav-indent-shift-left)))
(eval-after-load "elpy"
  '(define-key elpy-mode-map (kbd "<M-S-left>") 'my-nav-expand-to-sub-block))

(provide 'my-python)
;;; my-python.el ends here
