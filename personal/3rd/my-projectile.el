;;; package --- Summary
;;; Commentary:
;;; Code:

;; My projectile setting.

;; prelude
(defun my-prelude-mode-keys ()
  "My keybindings for prelude-mode."
  (define-key prelude-mode-map (kbd "M-o") nil)
  (define-key prelude-mode-map (kbd "C-c s") nil)
  (define-key prelude-mode-map (kbd "<M-S-up>") nil)
  (define-key prelude-mode-map (kbd "<M-S-down>") nil)
  (define-key prelude-mode-map (kbd "<C-S-up>") nil)
  (define-key prelude-mode-map (kbd "<C-S-down>") nil))
(add-hook 'prelude-mode-hook 'my-prelude-mode-keys)

;; helm
(helm-autoresize-mode -1)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-switch-project-action 'helm-projectile)

;; projectile: remote project will expire in 1 hour
(setq projectile-file-exists-remote-cache-expire (* 60 60))

;; projectile-key
(define-key projectile-command-map (kbd "g") 'helm-projectile-grep)
(define-key projectile-command-map (kbd "t") 'projectile-regenerate-tags)
(define-key projectile-command-map (kbd "o") 'projectile-find-other-file)

(provide 'my-projectile)
;;; my-editor.el ends here
