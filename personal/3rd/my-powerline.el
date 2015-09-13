;;; package --- Summary
;;; Commentary:
;;; Code:

;; powerline
(require 'powerline)
(powerline-default-theme)

;; change the background to darker color
(custom-set-variables
 '(powerline-default-separator 'slant))
(custom-set-faces
 '(powerline-active2 ((t (:inherit mode-line :background "#535353"))))
 '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "#474747")))))

;; fix the incorrect render on Emacs 24.4
(add-hook 'desktop-after-read-hook 'powerline-reset)
(defadvice desktop-kill(before clear-power-line-cache () activate)
  (set-frame-parameter nil 'powerline-cache nil))

(provide 'my-powerline)
;;; my-powerline.el ends here
