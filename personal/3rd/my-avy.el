;;; package --- Summary
;;; Commentary:

;; My avy setting.

;;; Code:

;;===================================
;; Add action copy-line in avy-action
;; copy line in avy-action
(defun avy-action-copy-line (pt)
  "Copy current line on pt."
  (save-excursion
    (let (str pt2)
      (goto-char pt)
      (beginning-of-line)
      (setq pt2 (point))
      (end-of-line)
      (setq str (buffer-substring pt2 (point)))
      (kill-new str)
      (message "Copied: %s" str)))
  (let ((dat (ring-ref avy-ring 0)))
    (select-frame-set-input-focus
     (window-frame (cdr dat)))
    (select-window (cdr dat))
    (goto-char (car dat))))

;; add avy-action-copy-line
(setq avy-dispatch-alist
  '((?x . avy-action-kill-move)
    (?X . avy-action-kill-stay)
    (?m . avy-action-mark)
    (?n . avy-action-copy)
    (?N . avy-action-copy-line)
    (?i . avy-action-ispell)))

(provide 'my-avy)
;;; my-avy.el ends here
