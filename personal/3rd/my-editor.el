;;; package --- Summary
;;; Commentary:

;; My main editor setting.

;;; Code:

;; smooth scroll
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

;; ace-mode
(global-ace-isearch-mode -1)
(setq ace-isearch-use-ace-jump nil)

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

;; isearch with selected region
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

;; insert current date
(defun my-insert-current-date()
  "Insert current date."
  (interactive)
  (let ((time-format "%Y-%m-%d"))
    (insert (format-time-string time-format (current-time)))))

;; align function head comment
(defun my-align-comment()
  "Align function head comment."
  (interactive)
  (align-regexp
   (region-beginning)
   (region-end)
   (concat "\\(\\s-*\\)" " -")))

;; clean up recentf & projectile projects
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

(defun window-half-height ()
  (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun my-scroll-down-half ()
  (interactive)
  (next-line (window-half-height)))

(defun my-scroll-up-half ()
  (interactive)
  (previous-line (window-half-height)))

;; use multiple-cursor
(use-package multiple-cursors
  :ensure t)

;; multiple-cursor: map RET to get consistent behavior at the terminal
;; https://github.com/magnars/multiple-cursors.el/pull/168
(define-key mc/keymap (kbd "<RET>") 'multiple-cursors-mode)

;; loop sp-up-sexp until the end
(defun my-sp-up-sexp-loop (&optional arg interactive)
  "Move forward out of one level of parentheses."
  (interactive)
  (while (sp-up-sexp) nil))

;; loop sp-up-sexp until the end
(defun my-sp-backward-up-sexp-loop (&optional arg interactive)
  "Move forward out of one level of parentheses."
  (interactive)
  (while (sp-backward-up-sexp) nil))

;; use auto-save
;; follow http://www.jianshu.com/p/998ceaf522d1
(require 'auto-save)
(auto-save-enable)
(setq auto-save-slient t)

(use-package reveal-in-osx-finder
  :if (string-equal system-type "darwin"))

(use-package change-inner
  :ensure t)

(use-package find-file-in-project
  :load-path "site-lisp/find-file-in-project/"
  :ensure t)

(use-package fasd
  :load-path "site-lisp/emacs-fasd/")
(global-fasd-mode 1)

;; copy a selected line
(defun my-avy-copy-line (arg)
  "Copy a selected line."
  (interactive "p")
  (let ((initial-window (selected-window)))
    (avy-with avy-copy-line
      (let* ((start (avy--line))
             (str (buffer-substring-no-properties
                   start
                   (save-excursion
                     (goto-char start)
                     (move-end-of-line arg)
                     (point)))))
        (kill-new str)
        (select-window initial-window)))))

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

;; save buffer with whitespace cleanup
(defun my-save-buffer ()
  "Save buffer with whitespace cleanup."
  (interactive)
  (whitespace-cleanup)
  (save-buffer))

(provide 'my-editor)
;;; my-editor.el ends here
