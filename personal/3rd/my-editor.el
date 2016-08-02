;;; package --- Summary
;;; Commentary:

;; My main editor setting.

;;; Code:

;; smooth scroll
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

;; turn-off guru warning
(setq prelude-guru nil)

;; ace-mode
(global-ace-isearch-mode -1)
(setq ace-isearch-use-ace-jump nil)

;; turn-off beacon
(beacon-mode -1)

;; use my own zenburn
(use-package zenburn-theme
  :load-path "site-lisp/zenburn-emacs/"
  :config
  (load-theme 'zenburn t))

;; turn-on which-function-mode
;; but turn-off it for cython (.pyx, .pyd) otherwise it will be extremely slow
(which-function-mode 1)
(eval-after-load "which-func"
  '(setq which-func-modes '(java-mode c++-mode c-mode org-mode python-mode emacs-lisp-mode)))

;; search with selected region
(defun jrh-isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))
(add-hook 'isearch-mode-hook #'jrh-isearch-with-region)

;; revert buffer without confirmation
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

;; dash for doc
(add-to-list 'dash-at-point-mode-alist
             '(python-mode . "python2,numpy,django,sqlalchemy,numpy,scipy"))
(add-to-list 'dash-at-point-mode-alist '(sql-mode . "PASCAL"))

(defun my-align-comment()
  (interactive)
  (align-regexp
   (region-beginning)
   (region-end)
   (concat "\\(\\s-*\\)" " -")))

(defun my-cleanup()
  "Clean up."
  (interactive)
  (recentf-cleanup)
  (projectile-cleanup-known-projects))

(global-flycheck-mode -1)
;; (require 'helm-flycheck)
;; (setq flycheck-display-errors-delay 0.9)
;; (setq flycheck-display-errors-function #'flycheck-display-error-messages)
;; (add-to-list 'display-buffer-alist (cons "\\*Flycheck error messages\\*" (cons #'display-buffer-no-window nil)))

;; narrow / widen the current region
;; or narrow / widen the current subtree if in org-mode
(defun my-narrow-or-widen-dwim ()
  "If the buffer is narrowed, it widens. Otherwise, it narrows to region, or Org subtree."
  (interactive)
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        ((equal major-mode 'org-mode) (org-narrow-to-subtree))
        (t (error "Please select a region to narrow to"))))

;; use multiple-cursor
(require 'multiple-cursors)

;; multiple-cursor: map RET to get consistent behavior at the terminal
;; https://github.com/magnars/multiple-cursors.el/pull/168
(define-key mc/keymap (kbd "<RET>") 'multiple-cursors-mode)

;; use auto-save
;; follow http://www.jianshu.com/p/998ceaf522d1
(require 'auto-save)
(auto-save-enable)
(setq auto-save-slient t)

(cond
 ((string-equal system-type "darwin")
  (require 'reveal-in-osx-finder)))

(provide 'my-editor)
;;; my-editor.el ends here
