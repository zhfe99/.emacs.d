;;; package --- Summary
;;; Commentary:

;; My SmartParent setting.

;;; Code:

;; loop sp-up-sexp until the end
(defun my-sp-up-sexp-loop (&optional arg interactive)
  "Move forward out of one level of parentheses."
  (interactive)
  (while (sp-up-sexp) nil))

;; loop sp-up-sexp until the end
(defun my-sp-backward-up-sexp-loop (&optional arg interactive)
  "Move forward out of one level of parentheses."
  (interactive)
  (while (sp-backward-up-sexp) nil))

(provide 'my-sp)
;;; my-sp.el ends here
