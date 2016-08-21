;;; package --- Summary
;;; Commentary:

;; My lua setting.

;;; Code:

;; lua
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; update creating date in the comment area (for lua)
(defun my-lua-create-date ()
  "Update creating date in the comment area (for lua)."
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

;; lua mode hood
(add-hook 'lua-mode-hook
          (lambda ()
            (subword-mode 1)
            (yas-minor-mode)
            (indent-guide-mode)
            (git-gutter+-mode)
            (setq safe-local-variable-values
                  '((lua-indent-level . 2)
                    (lua-indent-level . 3)
                    (lua-indent-level . 4)
                    (lua-indent-level . 8)))
            (local-set-key (kbd "H-c") 'my-lua-create-date)))

(provide 'my-lua)
;;; my-lua.el ends here
