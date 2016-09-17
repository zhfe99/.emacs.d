;;; package --- Summary
;;; Commentary:

;; My matlab setting.

;;; code:

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
  (setq matlab-shell-command "/home/adu/common/MATLAB/R2013a/bin/matlab")))

;; matlab startup configuration
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))

;; update creating date in the comment area (for matlab)
(defun my-matlab-create-date ()
  "Update creating date in the comment area (for matlab)."
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

;; matlab-mode-hook
(add-hook 'matlab-mode-hook
          (lambda()
            (setq matlab-indent-function t)
            (run-hooks 'prelude-prog-mode-hook)
            (auto-fill-mode -1)
            (indent-guide-mode)
            (git-gutter+-mode)))

(provide 'my-matlab)
;;; my-editor.el ends here
