;;; package --- Summary
;;; Commentary:

;; My Git setting.

;;; Code:

;; turn-off diff-hl mode
(global-diff-hl-mode -1)

;; use git-gutter
(if (fboundp 'define-fringe-bitmap)
    (require 'git-gutter-fringe)
  (require 'git-gutter))

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

(defun my-goto-git-gutter ()
  (interactive)
  (if git-gutter:diffinfos
      (let* ((collection (mapcar 'my-reshape-git-gutter
                                 git-gutter:diffinfos)))
        (ivy-read "git-gutters:"
                  collection
                  :action (lambda (lineno)
                            (goto-line lineno))))
    (message "NO git-gutters!")))

;; use ivy in magit
(setq magit-completing-read-function 'ivy-completing-read)

(provide 'my-git)
;;; my-git.el ends here
