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

;; http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html
(defun my-reshape-git-gutter (gutter)
  "Re-shape gutter for `ivy-read'."
  (let* ((lineno (aref gutter 3))
         line)
    (save-excursion
      (goto-line lineno)
      (setq line (buffer-substring (line-beginning-position)
                                   (line-end-position))))
    ;; build (key . lineno)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (aref gutter 1)) "-" "+")
                  lineno
                  (replace-regexp-in-string "^[ \t]*" "" line))
          lineno)))

;; use ivy to show diffs
(defun my-goto-git-gutter ()
  (interactive)
  (if git-gutter:diffinfos
      (let* ((collection (mapcar 'my-reshape-git-gutter
                                 git-gutter:diffinfos)))
        (ivy-read "git-gutters:" collection
                  :action (lambda (lineno)
                            (goto-line (cdr lineno)))))
    (message "NO git-gutters!")))

;; http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html
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

;; see what line changed in latest commit
;; http://blog.binchen.org/posts/git-gutter-tip.html
(defun git-gutter-reset-to-head-parent()
  (interactive)
  (let (parent (filename (buffer-file-name)))
    (if (eq git-gutter:vcs-type 'svn)
        (setq parent "PREV")
      (setq parent (if filename (concat (shell-command-to-string (concat "git --no-pager log --oneline -n1 --pretty='format:%H' " filename)) "^") "HEAD^")))
    (git-gutter:set-start-revision parent)
    (message "git-gutter:set-start-revision HEAD^")))

(defun git-gutter-reset-to-default ()
  (interactive)
  (git-gutter:set-start-revision nil)
  (message "git-gutter reset"))

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

(provide 'my-git)
;;; my-git.el ends here
