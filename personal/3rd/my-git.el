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

(provide 'my-git)
;;; my-git.el ends here
