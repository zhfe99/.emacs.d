;;; package --- Summary
;;; Commentary:

;; My shell mode setting.

;;; Code:

;; update modifying date field in the comment area (for sh)
(defun my-sh-modify-date ()
  "Update modifying date field in the comment area (for sh)."
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

;; update creating date in the comment area (for sh)
(defun my-sh-create-date ()
  "Update creating date in the comment area (for sh)."
  (interactive)
  (save-excursion
    (let ((time-format "%Y-%m") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "#   create" nil t))
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
(defun my-sh-save-hook ()
  "My hook for saving sh file (*.sh)."
  (if (eq major-mode 'sh-mode)
      (progn
        (message "%s is sh-mode" (buffer-file-name))
        (my-sh-modify-date))))
(add-hook 'before-save-hook 'my-sh-save-hook)

(require 'indent-guide)

;; mode hook
(add-hook 'sh-mode-hook
          (lambda ()
            (subword-mode 1)
            (yas-minor-mode)
            (indent-guide-mode)
            (git-gutter+-mode)
            (local-set-key (kbd "H-c") 'my-sh-create-date)))

;; emacs-lisp
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (indent-guide-mode)
            (git-gutter+-mode)))

(provide 'my-sh)
;;; my-sh.el ends here
