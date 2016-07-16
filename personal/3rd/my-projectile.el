;;; package --- Summary
;;; Commentary:
;;; Code:

;; My projectile setting.

;; disable some prelude keys
(defun my-prelude-mode-keys ()
  "My keybindings for prelude-mode."
  (define-key prelude-mode-map (kbd "M-o") nil)
  (define-key prelude-mode-map (kbd "C-c t") nil)
  (define-key prelude-mode-map (kbd "C-c s") nil)
  (define-key prelude-mode-map (kbd "<M-S-up>") nil)
  (define-key prelude-mode-map (kbd "<M-S-down>") nil)
  (define-key prelude-mode-map (kbd "<C-S-up>") nil)
  (define-key prelude-mode-map (kbd "<C-S-down>") nil))
(add-hook 'prelude-mode-hook 'my-prelude-mode-keys)

;; use ivy
(setq projectile-completion-system 'ivy)

;; use counsel-projectile
(setq projectile-switch-project-action 'counsel-projectile)
(setq projectile-find-dir-includes-top-level t)

;; projectile: remote project will expire in 1 hour
(setq projectile-file-exists-remote-cache-expire (* 60 60))

;; projectile-key
(define-key projectile-command-map (kbd "h") 'counsel-projectile)

(provide 'my-projectile)
;;; my-editor.el ends here
