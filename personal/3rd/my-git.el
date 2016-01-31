;;; package --- Summary
;;; Commentary:

;; My Git setting.

;;; Code:

(defun git-push-remote-origin ()
  "run `git push'"
  (interactive)
  (shell-command "git add .")
  (shell-command "git commit -m \"Update\"")
  (shell-command "git push")
  (message "DONE! git push at %s" default-directory))

;; turn-off diff-hl mode
(global-diff-hl-mode -1)

;; use git-gutter+
(require 'git-gutter-fringe+)
(cond
 ((string-equal system-type "gnu/linux")
  (git-gutter+-toggle-fringe)))

;; key
(eval-after-load 'git-gutter+
  '(progn
     ;;; Jump between hunks
     (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)

     ;;; Act on hunks
     (define-key git-gutter+-mode-map (kbd "C-x v =") 'git-gutter+-show-hunk)
     (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks)
     ;; Stage hunk at point.
     ;; If region is active, stage all hunk lines within the region.
     (define-key git-gutter+-mode-map (kbd "C-x t") 'git-gutter+-stage-hunks)
     (define-key git-gutter+-mode-map (kbd "C-x c") 'git-gutter+-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C") 'git-gutter+-stage-and-commit)
     (define-key git-gutter+-mode-map (kbd "C-x C-y") 'git-gutter+-stage-and-commit-whole-buffer)
     (define-key git-gutter+-mode-map (kbd "C-x U") 'git-gutter+-unstage-whole-buffer)))

(provide 'my-git)
;;; my-git.el ends here
