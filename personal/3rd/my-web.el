;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'prelude-web)
(require 'prelude-css)
(require 'prelude-js)

;; web-mode
(add-hook 'web-mode-hook
          (lambda()
            (setq web-mode-markup-indent-offset 2)))

(provide 'my-web)
;;; my-web.el ends here
