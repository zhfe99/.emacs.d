;;; package --- Summary
;;; Commentary:
;;; Code:

;; My projectile setting.

;; add bing search-backward
(prelude-install-search-engine "bing" "http://www.bing.com/search?q=" "Bing: ")
(prelude-install-search-engine "baidu" "http://www.baidu.com/s?wd=" "Baidu: ")

;; use ivy
(setq projectile-completion-system 'ivy)

;; use counsel-projectile
(setq projectile-find-dir-includes-top-level t)

;; projectile: remote project will expire in 1 hour
(setq projectile-file-exists-remote-cache-expire (* 60 60))

;; clean up recentf & projectile projects
(defun my-cleanup()
  "Clean up."
  (interactive)
  (recentf-cleanup)
  (projectile-cleanup-known-projects))

(provide 'my-projectile)
;;; my-editor.el ends here
