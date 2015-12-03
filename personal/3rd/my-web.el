;;; package --- Summary
;;; Commentary:
;;; Code:

;; web-mode
(add-hook 'web-mode-hook
          (lambda()
            (setq web-mode-markup-indent-offset 2)))

(provide 'my-web)
;;; my-web.el ends here

;; reserve M-. for find-tag
(eval-after-load 'js2-mode
  '(define-key js2-mode-map (kbd "M-.") nil))
