;;; package --- Summary
;;; Commentary:
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

(provide 'my-window)
;;; my-window.el ends here
