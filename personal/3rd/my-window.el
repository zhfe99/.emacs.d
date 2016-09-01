;;; package --- Summary
;;; Commentary:

;; My window management setting.

;;; Code:

(window-numbering-mode)

(defun split-window-below-and-move-there-dammit ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun my-push-window-up ()
  (interactive)
  (let ((other-window (windmove-find-other-window 'up)))
    (cond ((null other-window)
           (error "No window %s from selected window" 'up))
          (t (delete-window other-window)))))

(defun my-push-window-left ()
  (interactive)
  (let ((other-window (windmove-find-other-window 'left)))
    (cond ((null other-window)
           (error "No window %s from selected window" 'left))
          (t (delete-window other-window)))))

(defun my-push-window-down ()
  (interactive)
  (let ((other-window (windmove-find-other-window 'down)))
    (cond ((null other-window)
           (error "No window %s from selected window" 'down))
          (t (delete-window other-window)))))

(defun my-push-window-right ()
  (interactive)
  (let ((other-window (windmove-find-other-window 'right)))
    (cond ((null other-window)
           (error "No window %s from selected window" 'right))
          (t (delete-window other-window)))))

(defun my-save-window-conf-1 ()
  (interactive)
  (window-configuration-to-register 1))

(defun my-save-window-conf-2 ()
  (interactive)
  (window-configuration-to-register 2))

(defun my-save-window-conf-3 ()
  (interactive)
  (window-configuration-to-register 3))

(defun my-save-window-conf-4 ()
  (interactive)
  (window-configuration-to-register 4))

(defun my-goto-window-conf-1 ()
  (interactive)
  (jump-to-register 1))

(defun my-goto-window-conf-2 ()
  (interactive)
  (jump-to-register 2))

(defun my-goto-window-conf-3 ()
  (interactive)
  (jump-to-register 3))

(defun my-goto-window-conf-4 ()
  (interactive)
  (jump-to-register 4))

(defun my-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun my-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun my-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun my-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun my-scroll-down-half ()
  (interactive)
  (next-line (window-half-height)))

(defun my-scroll-up-half ()
  (interactive)
  (previous-line (window-half-height)))

;; smooth scroll
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

(provide 'my-window)
;;; my-window.el ends here
