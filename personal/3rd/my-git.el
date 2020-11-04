;;; package --- Summary
;;; Commentary:

;; My Git setting.

;;; Code:

;; turn-off diff-hl mode
(global-diff-hl-mode -1)

;; use git-gutter+ (in terminal) / git-gutter-fringe+ (in Gui)
(if (display-graphic-p)
    (require 'git-gutter-fringe+)
  (require 'git-gutter+))

;; modified from http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html
(defun my-reshape-git-gutter+ (gutter)
  "Re-shape gutter for `ivy-read'."
  (let* ((lineno (nth 5 gutter))
         line)
    (save-excursion
      (goto-line lineno)
      (setq line (buffer-substring (line-beginning-position)
                                   (line-end-position))))
    ;; build (key . lineno)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (nth 1 gutter)) "-" "+")
                  lineno
                  (replace-regexp-in-string "^[ \t]*" "" line))
          lineno)))

(defun my-goto-git-gutter+ ()
  (interactive)
  (if git-gutter+-diffinfos
      (let* ((collection (mapcar 'my-reshape-git-gutter+
                                 git-gutter+-diffinfos)))
        (ivy-read "git-gutters:"
                  collection
                  :action (lambda (lineno)
                            (goto-line (cdr lineno)))))
    (message "NO git-gutters!")))

;;==============
;; Magit setting

;; use ivy in magit
(setq magit-completing-read-function 'ivy-completing-read)

;; use magit-status in fullscreen
(defun magit-status-fullscreen (prefix)
  (interactive "P")
  (magit-status)
  (unless prefix
    (delete-other-windows)))

(set-default 'magit-push-always-verify nil)
(set-default 'magit-revert-buffers 'silent)
(set-default 'magit-no-confirm '(stage-all-changes
                                 unstage-all-changes))

(defun my-magit-remove-git-lock-file ()
  "Remove git's index lock file, if it exists."
  (interactive)
  (let ((base (magit-toplevel)))
    (delete-file (concat base "/.git/index.lock"))))

(provide 'my-git)
;;; my-git.el ends here
