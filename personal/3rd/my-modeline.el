;;; package --- Summary
;;; Commentary:

;; My mode-line setting.

;;; Code:

(setq sml/theme 'respectful)
(size-indication-mode -1)
(setq sml/name-width 50)
(setq sml/line-number-format " %3l")
(setq sml/mode-width nil)
(setq sml/mule-info nil)
(setq sml/prefix nil)
(sml/setup)

(provide 'my-modeline)
;;; my-modeline.el ends here
