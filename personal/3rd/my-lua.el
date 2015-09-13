;;; package --- Summary
;;; Commentary:
;;; Code:

;; lua
(prelude-require-package 'lua-mode)
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; update modifying date field in the comment area (for lua)
(defun my-lua-modify-date ()
  "Update modifying date field in the comment area (for lua)."
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

;; update creating date in the comment area (for lua)
(defun my-lua-create-date ()
  "Update creating date in the comment area (for lua)."
  (interactive)
  (save-excursion
    (let ((time-format "%Y-%m") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "--   create" nil t))
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

;; lua mode hood
(add-hook 'lua-mode-hook
          (lambda ()
            (subword-mode 1)
            (yas-minor-mode)
            (setq lua-indent-level 2)
            (local-set-key (kbd "H-c") 'my-lua-create-date)))

(defun my-lua-save-hook ()
  "My hook for saving python file (*.py)."
  (if (eq major-mode 'lua-mode)
      (progn
        (message "%s is lua-mode" (buffer-file-name))
        (my-lua-modify-date))))
(add-hook 'before-save-hook 'my-lua-save-hook)

(provide 'my-lua)
;;; my-lua.el ends here
