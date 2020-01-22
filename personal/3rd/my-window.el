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

(defun split-window-below-and-balance ()
  (interactive)
  (split-window-below)
  (balance-windows))

(defun split-window-right-and-balance ()
  (interactive)
  (split-window-right)
  (balance-windows))

(defun ace-delete-window-and-balance ()
  (interactive)
  (ace-delete-window)
  (balance-windows))

(defun my-push-window-up ()
  (interactive)
  (let ((other-window (windmove-find-other-window 'up)))
    (cond ((null other-window)
           (error "No window %s from selected window" 'up))
          (t (delete-window other-window)))
    (balance-windows)))

(defun my-push-window-left ()
  (interactive)
  (let ((other-window (windmove-find-other-window 'left)))
    (cond ((null other-window)
           (error "No window %s from selected window" 'left))
          (t (delete-window other-window)))
    (balance-windows)))

(defun my-push-window-down ()
  (interactive)
  (let ((other-window (windmove-find-other-window 'down)))
    (cond ((null other-window)
           (error "No window %s from selected window" 'down))
          (t (delete-window other-window)))
    (balance-windows)))

(defun my-push-window-right ()
  (interactive)
  (let ((other-window (windmove-find-other-window 'right)))
    (cond ((null other-window)
           (error "No window %s from selected window" 'right))
          (t (delete-window other-window)))
    (balance-windows)))

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

;; transpose frame
(require 'transpose-frame)

;; smooth scroll
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

;; ===========
;; use pop win
(require 'popwin)
(popwin-mode 1)

;; =============
;; use eyebrowse
(eyebrowse-mode t)

;; popwin settings
;; (setq popwin:special-display-config
;;       '(("*Help*" :height 30 :stick t)
;;         ("*Completions*" :noselect t)
;;         ("*compilation*" :noselect t)
;;         ("*Messages*")
;;         ("*Occur*" :height 0.3  t)
;;         ("\\*Slime Description.*" :noselect t :regexp t :height 30)
;;         ("*magit-commit*" :noselect t :height 0.3 :width 80 :stick t)
;;         ("*magit-diff*" :noselect t :height 0.3 :width 80)
;;         ("*magit-edit-log*" :noselect t :height 0.2 :width 80)
;;         ("*magit-process*" :noselect t :height 0.2 :width 80)
;;         ("\\*Slime Inspector.*" :regexp t :height 30)
;;         ("*Ido Completions*" :noselect t :height 30)
;;         ;;("*eshell*" :height 20)
;;         ("*ansi-term*.*" :regexp t :height 30)
;;         ("*shell*" :height 30)
;;         (".*overtone.log" :regexp t :height 30)
;;         ("*gists*" :height 30)
;;         ("*sldb.*":regexp t :height 30)
;;         ("*Gofmt Errors*" :noselect t)
;;         ("\\*godoc*" :regexp t :height 30)
;;         ;("*Shell Command Output*" :noselect t)
;;         ("*nrepl-error*" :height 20 :stick t)
;;         ("*nrepl-doc*" :height 20 :stick t)
;;         ("*nrepl-src*" :height 20 :stick t)
;;         ("*Kill Ring*" :height 30)
;;         ("*project-status*" :noselect t)
;;         ("*Compile-Log" :height 20 :stick t)
;;         ("*pytest*" :noselect t)
;;         ("*Python*" :stick t)
;;         ("*jedi:doc*" :noselect t)
;;         )
;;       )

;; (when (require 'popwin nil t)
;;   (setq anything-samewindow nil)
;;   (setq display-buffer-function 'popwin:display-buffer)
;;   (push '("anything" :regexp t :height 0.5) popwin:special-display-config)
;;   (push '("helm" :regexp t :height 0.3) popwin:special-display-config)
;;   (push '("*magit-edit-log*" :height 0.3) popwin:special-display-config)
;;   (push '("magit" :regexp t :height 0.3) popwin:special-display-config)
;;   (push '("*Completions*" :height 0.4) popwin:special-display-config)
;;   (push '("*compilation*" :height 0.4 :noselect t :stick t) popwin:special-display-config)
;;   (push '("*Help*" :height 0.3 :stick t) popwin:special-display-config)
;;   (push '("*quickrun*" :height 0.3 :stick t) popwin:special-display-config)
;;   )

;; (push '(dired-mode :position top) popwin:special-display-config)
;; (push '(term-mode :position top) popwin:special-display-config)

;; (defun popwin-term:term ()
;;   (interactive)
;;   (popwin:display-buffer-1
;;    (or (get-buffer "*terminal*")
;;        (save-window-excursion
;;          (call-interactively 'term)))
;;    :default-config-keywords '(:position :top)))

(defun buffer-resize1 ()
  (interactive)
  (delete-other-windows)
  (split-window-vertically (floor (* 0.68 (window-height))))
  (other-window 1)
  (switch-to-buffer buf)
  (other-window 1))

(defun buffer-resize2 ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally (floor (* 0.68 (window-width))))
  (other-window 1)
  (switch-to-buffer buf)
  (other-window 1))

(provide 'my-window)
;;; my-window.el ends here
