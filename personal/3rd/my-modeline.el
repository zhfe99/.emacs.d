;;; package --- Summary
;;; Commentary:

;; My mode-line setting.

;;; Code:

;; use my own zenburn
(use-package zenburn-theme
  :load-path "site-lisp/zenburn-emacs/"
  :config
  (load-theme 'zenburn t))

(require 'smart-mode-line)
(setq sml/no-confirm-load-theme t)
;; delegate theming to the currently active theme
(add-hook 'after-init-hook #'sml/setup)

(setq sml/theme 'respectful)
(size-indication-mode -1)
(setq sml/name-width 20)
(setq sml/line-number-format " %3l")
(setq sml/mode-width 3)
(setq sml/mule-info nil)
(setq sml/prefix nil)
(sml/setup)

(provide 'my-modeline)
;;; my-modeline.el ends here
