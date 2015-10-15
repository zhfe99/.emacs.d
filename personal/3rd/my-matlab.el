;;; package --- Summary
;;; Commentary:
;;; Code:

;; matlab
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(setq auto-mode-alist (cons '("\\.m$" . matlab-mode) auto-mode-alist))

;; matlab-shell-mode
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)

;; matlab install position
(cond
 ((string-equal system-type "darwin")
  (setq matlab-shell-command "/Applications/MATLAB.app/bin/matlab"))
 ((string-equal system-type "gnu/linux")
  (setq matlab-shell-command "/usr/bin/matlab")))

;; matlab startup configuration
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))

;; update modifying date field in the comment area (for matlab)
(defun my-matlab-modify-date ()
  "Update modifying date field in the comment area (for matlab)."
  (interactive)
  (save-excursion
    (let ((time-format "%m-%d-%Y") (pt1) (pt2))
      (goto-char (point-min))
      (setq pt1 (search-forward "%   modify" nil t))
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

;; update the "modify date" in the comment before saving
(add-hook 'before-save-hook
          (lambda()
            (if (eq major-mode 'matlab-mode)
                (progn
                  (message "%s is matlab-mode" (buffer-file-name))
                  (my-matlab-modify-date)))))

;; matlab-mode-hook
(add-hook 'matlab-mode-hook
          (lambda()
            (setq matlab-indent-function t)
            (run-hooks 'prelude-prog-mode-hook)
            (auto-fill-mode -1)
            (local-set-key (kbd "H-c") 'my-matlab-create-date)
            (local-set-key (kbd "M-;") 'comment-dwim)))

;; julia-mode
(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

(provide 'my-matlab)
;;; my-editor.el ends here
