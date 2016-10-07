;;; package --- Summary
;;; Commentary:

;; My latex setting.

;;; Code:
(require 'prelude-latex)

;; latex-mode-hook
(add-hook 'LaTeX-mode-hook
          (lambda()
            (TeX-PDF-mode t)
            (git-gutter+-mode)
            (yas-minor-mode t)
            (setq TeX-save-query nil)
            (visual-line-mode)
            (auto-fill-mode -1)
            (subword-mode)
            (toggle-truncate-lines)))

;; reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-auctex t)

(provide 'my-latex)
;;; my-latex.el ends here
