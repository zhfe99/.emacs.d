;;; package --- Summary
;;; Commentary:

;; My multi-cursor setting.

;;; Code:

;; use multiple-cursor
(use-package multiple-cursors
  :ensure t)

;; multiple-cursor: map RET to get consistent behavior at the terminal
;; https://github.com/magnars/multiple-cursors.el/pull/168
(define-key mc/keymap (kbd "<RET>") 'multiple-cursors-mode)

;; http://emacs.stackexchange.com/questions/18716/why-does-multiple-cursors-use-the-same-char-for-all-cursors-with-zap-to-char-but/18880#18880
(defun mc-friendly/zap-up-to-char (arg char)
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Zap up to char: " t)))
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
                 (progn
                   (forward-char direction)
                   (unwind-protect
                       (search-forward (char-to-string char) nil nil arg)
                     (backward-char direction))
                   (point)))))

(use-package change-inner
  :ensure t)

(provide 'my-mc)
;;; my-mc.el ends here
