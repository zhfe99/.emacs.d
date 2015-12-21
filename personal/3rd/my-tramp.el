;;; package --- Summary
;;; Commentary:

;; My tramp setting.

;;; Code:

;; tramp
(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(setq tramp-default-method "scp")
(setq tramp-chunksize 500)

;; disable vc on tramp
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(provide 'my-tramp)
;;; my-tramp.el ends here
