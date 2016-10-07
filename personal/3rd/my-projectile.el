;;; package --- Summary
;;; Commentary:
;;; Code:

;; My projectile setting.

;; enable prelude sub-modules
(require 'prelude-company)
(require 'prelude-ido)
(require 'prelude-programming)
(require 'prelude-org)
(require 'prelude-python)
(require 'prelude-web)
(require 'prelude-css)
(require 'prelude-js)

;; projectile-bookmark location
(setq projectile-known-projects-file
  (expand-file-name "projectile-bookmarks.eld"
                    (concat user-emacs-directory "savefile/")))

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
