;;; package --- Summary
;;; Commentary:

;; My web/js mode setting.

;;; Code:

;; web-mode
(add-hook 'web-mode-hook
          (lambda()
            (highlight-indentation-mode)
            (setq web-mode-markup-indent-offset 2)))

(provide 'my-web)
;;; my-web.el ends here
