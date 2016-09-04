;;; package --- Summary
;;; Commentary:

;; My main editor setting.

;;; Code:

;; ace-mode
(global-ace-isearch-mode -1)
(setq ace-isearch-use-ace-jump nil)

;; turn-on which-function-mode
;; but turn-off it for cython (.pyx, .pyd) otherwise it will be extremely slow
(which-function-mode 1)
(eval-after-load "which-func"
  '(setq which-func-modes '(java-mode c++-mode c-mode org-mode python-mode emacs-lisp-mode)))

;; isearch with selected region
(defun jrh-isearch-with-region ()
  "Use region as the isearch text."
  (when mark-active
    (let ((region (funcall region-extract-function nil)))
      (deactivate-mark)
      (isearch-push-state)
      (isearch-yank-string region))))
(add-hook 'isearch-mode-hook #'jrh-isearch-with-region)

;; insert current date
(defun my-insert-current-date()
  "Insert current date."
  (interactive)
  (let ((time-format "%Y-%m-%d"))
    (insert (format-time-string time-format (current-time)))))

;; insert current file name

;; align function head comment
(defun my-align-comment()
  "Align function head comment."
  (interactive)
  (align-regexp
   (region-beginning)
   (region-end)
   (concat "\\(\\s-*\\)" " -")))

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

;; Type M-y after C-y to activate counsel-yank-pop
(advise-commands "indent" (yank counsel-yank-pop) after
                 "If current mode is one of `prelude-yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
                 (if (and (not (ad-get-arg 0))
                          (not (member major-mode prelude-indent-sensitive-modes))
                          (or (derived-mode-p 'prog-mode)
                              (member major-mode prelude-yank-indent-modes)))
                     (let ((transient-mark-mode nil))
                       (yank-advised-indent-function (region-beginning) (region-end)))))

(use-package reveal-in-osx-finder
  :if (string-equal system-type "darwin"))

(use-package find-file-in-project
  :load-path "site-lisp/find-file-in-project/"
  :ensure t)

(use-package fasd
  :load-path "site-lisp/emacs-fasd/")
(global-fasd-mode 1)

;; (use-package workgroups2
;;   :load-path "site-lisp/workgroups2/src/")
;; (workgroups-mode 1)

(provide 'my-editor)
;;; my-editor.el ends here
