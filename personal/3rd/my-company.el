;;; package --- Summary
;;; Commentary:

;; My company-mode setting.

;;; Code:

(setq company-idle-delay 0.5)
(setq company-show-numbers t)
(setq company-backends (delete 'company-semantic company-backends))
(add-to-list 'company-backends 'company-c-headers)

(provide 'my-company)
;;; my-company.el ends here
