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

(require 'counsel-projectile)

;; Change counsel-projectile default action to dired
(defun counsel-projectile (&optional arg)
  "Use projectile with Ivy instead of ido.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (ivy-read "Switch to project: "
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
                      (projectile-relevant-known-projects))
              projectile-known-projects)
            :action
            (lambda (dir)
              ;; (projectile-switch-project-by-name dir arg)
              (dired dir))
            :require-match t
            :caller 'counsel-projectile))

;; add "." to open root
(ivy-set-actions
 'counsel-projectile
 '(("f" (lambda (dir)
          (let ((projectile-switch-project-action 'counsel-projectile-find-file))
            (projectile-switch-project-by-name dir arg)))
    "find file")
   ("d" (lambda (dir)
          (let ((projectile-switch-project-action 'counsel-projectile-find-dir))
            (projectile-switch-project-by-name dir arg)))
    "find directory")
   ("b" (lambda (dir)
          (let ((projectile-switch-project-action 'counsel-projectile-switch-to-buffer))
            (projectile-switch-project-by-name dir arg)))
    "switch to buffer")
   ("s" (lambda (dir)
          (let ((projectile-switch-project-action 'projectile-save-project-buffers))
            (projectile-switch-project-by-name dir arg)))
    "save all buffers")
   ("k" (lambda (dir)
          (let ((projectile-switch-project-action 'projectile-kill-buffers))
            (projectile-switch-project-by-name dir arg)))
    "kill all buffers")
   ("r" (lambda (dir)
          (let ((projectile-switch-project-action
                 'projectile-remove-current-project-from-known-projects))
            (projectile-switch-project-by-name dir arg)))
    "remove from known projects")
   ("l" (lambda (dir)
          (let ((projectile-switch-project-action 'projectile-edit-dir-locals))
            (projectile-switch-project-by-name dir arg)))
    "edit dir-locals")
   ("g" (lambda (dir)
          (let ((projectile-switch-project-action 'projectile-vc))
            (projectile-switch-project-by-name dir arg)))
    "open in vc-dir / magit / monky")
   ("e" (lambda (dir)
          (let ((projectile-switch-project-action 'projectile-run-eshell))
            (projectile-switch-project-by-name dir arg)))
    "start eshell")
   ("." (lambda (dir)
          (dired dir))
    "open root directory")))

(provide 'my-projectile)
;;; my-editor.el ends here
